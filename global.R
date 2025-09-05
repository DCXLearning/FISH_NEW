library(shiny)
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)
library(tidyr)
library(scales)
library(openxlsx)
library(tibble)
library(officer)
library(forcats)
library(cowplot)
library(flextable)

source("ConnectKobo2.R")

# Call once
cleaned_data <- clean_all_kobo()

# Access each cleaned dataframe
main_df <- cleaned_data$main
nat_df <- cleaned_data$nat
aqu_df <- cleaned_data$aqu
processing_df <- cleaned_data$processing
patrol_df <- cleaned_data$patrol

# --- Connect to DB ---
con <- tryCatch(
  dbConnect(
    RMariaDB::MariaDB(),
    user = "root",
    password = "Mymariadb123",
    host = "104.248.155.82",
    port = 3306,
    dbname = "DATA_MART"
  ),
  error = function(e) {
    message("Failed to connect to the database: ", e$message)
    NULL
  }
)
if (is.null(con)) stop("Database connection failed. Please check your credentials and network.")

# --- Lookup Tables ---
choices <- dbGetQuery(con, "SELECT * FROM KOBO_CHOICES_FISHERIES_MART")

province_lookup <- choices %>%
  filter(list_name == "provinces") %>%
  select(name, label_khmer, label_english) %>%
  rename(province_code = name,
         province_kh = label_khmer,
         province_en = label_english)

marine_codes <- c("7","9","18","23")  # Kampot, Koh Kong, Preah Sihanouk, Kep

# Province choices by group (English shown, Khmer value)
get_province_choices_for_group <- function(group = "All") {
  pl <- province_lookup %>%
    dplyr::mutate(code_norm = gsub("^0+", "", as.character(province_code)),
                  Province_Group = dplyr::if_else(code_norm %in% marine_codes, "Marine", "Inland"))
  if (!is.null(group) && group != "All") pl <- pl %>% dplyr::filter(Province_Group == group)
  c("All" = "All", setNames(pl$province_kh, pl$province_en))
}

# Area choices by group (values are your area labels, e.g. "Marine Capture", "Freshwater Capture")
get_area_choices_for_group <- function(group = "All", area_choices) {
  vals <- unname(area_choices); labs <- names(area_choices)
  keep <- rep(TRUE, length(vals))
  lv <- tolower(trimws(vals))
  if (!is.null(group) && group == "Inland") {
    keep <- !grepl("marine", lv)  # Inland group must NOT offer Marine
  }
  # always keep "All"
  keep <- keep | vals == "All" | labs == "All"
  setNames(vals[keep], labs[keep])
}


# Map quarter -> month values (your month select uses "1","2",... as values)
quarter_to_months <- list(
  Q1 = c("1","2","3"),
  Q2 = c("4","5","6"),
  Q3 = c("7","8","9"),
  Q4 = c("10","11","12")
)

# Limit month choices to a quarter
get_month_choices_for_quarter <- function(q, month_choices) {
  if (is.null(q) || q == "All") return(month_choices)
  vals <- unname(month_choices); labs <- names(month_choices)
  keep <- (vals == "All") | (vals %in% quarter_to_months[[q]])
  setNames(vals[keep], labs[keep])
}

# Month -> Quarter
month_to_quarter <- function(m) {
  if (is.null(m) || m == "All") return(NULL)
  m <- suppressWarnings(as.integer(m))
  if      (m %in% 1:3)    "Q1"
  else if (m %in% 4:6)    "Q2"
  else if (m %in% 7:9)    "Q3"
  else if (m %in% 10:12)  "Q4"
  else NULL
}


fish_label_lookup <- choices %>%
  filter(list_name == "type") %>%
  select(name, label_khmer, label_english) %>%
  rename(fish_code = name,
         fish_name_kh = label_khmer,
         fish_name_en = label_english)

processed_fish_lookup <- choices %>%
  filter(list_name == "typepro") %>%
  select(name, label_khmer, label_english) %>%
  rename(proc_code = name,
         proc_name_kh = label_khmer,
         proc_name_en = label_english)

area_lookup <- choices %>%
  filter(list_name == "area") %>%
  select(name, label_khmer, label_english) %>%
  rename(area_code = name, area_kh = label_khmer, area_en = label_english)



reports_raw <- main_df %>%
  mutate(date = unlist(date)) %>%
  filter(!is.na(date)) %>%
  mutate(
    date      = as.Date(date, format = "%Y-%m-%d"),
    province  = as.character(province),                 # <-- ensure string codes
    Year      = year(date),
    Month     = month(date),
    Quarter   = case_when(
      Month %in% 1:3   ~ 'Q1',
      Month %in% 4:6   ~ 'Q2',
      Month %in% 7:9   ~ 'Q3',
      Month %in% 10:12 ~ 'Q4'
    ),
    week      = paste0("Week", ceiling(day(date) / 7)),
    MonthAbbr = month(date, label = TRUE, abbr = TRUE),
    
    # Two groups by province code
    Province_Group = factor(
      dplyr::if_else(province %in% c("9","18","23","7"), "Marine", "Inland"),
      levels = c("Marine","Inland")
    )
  ) %>%
  left_join(province_lookup, by = c("province" = "province_code")) %>%
  mutate(
    province_en = coalesce(province_en, province),
    province_kh = coalesce(province_kh, province)
  )

# FIX: Added unlist() to handle the list date column
nat_raw <- nat_df %>%
  mutate(date = unlist(date)) %>%
  mutate(
    date      = as.Date(date),
    province  = as.character(province),  # ensure codes are character first
    Year      = year(date),
    Month     = month(date),
    Quarter   = case_when(
      Month %in% 1:3   ~ 'Q1',
      Month %in% 4:6   ~ 'Q2',
      Month %in% 7:9   ~ 'Q3',
      Month %in% 10:12 ~ 'Q4'
    ),
    week                      = paste0("Week", ceiling(day(date) / 7)),
    catch                     = as.numeric(nat_fishcatch_nat_catch),
    area_code_nat             = as.character(nat_fishcatch_area_nat),
    natfishcatch_typefishing  = as.character(nat_fishcatch_type_fishing),
    natfishcatch_nat_fishtype = as.character(nat_fishcatch_nat_fishtype),
    
    # --- NEW: two groups by province code ---
    Province_Group = factor(
      dplyr::if_else(gsub("^0+", "", province) %in% c("7","9","18","23"), "Marine", "Inland"),
      levels = c("Marine","Inland")
    )
  ) %>%
  left_join(province_lookup,      by = c("province" = "province_code")) %>%
  left_join(fish_label_lookup,    by = c("natfishcatch_nat_fishtype" = "fish_code")) %>%
  left_join(area_lookup,          by = c("area_code_nat" = "area_code")) %>%
  mutate(
    province_kh  = coalesce(province_kh, province),
    province_en  = coalesce(province_en, province),
    fish_name_kh = coalesce(fish_name_kh, natfishcatch_nat_fishtype),
    fish_name_en = coalesce(fish_name_en, natfishcatch_nat_fishtype),
    area_en      = coalesce(area_en, area_code_nat)
  )


# FIX: Added unlist() to handle the list date column
aqu_raw <- aqu_df %>%
  mutate(date = unlist(date)) %>%
  mutate(
    date      = as.Date(date),
    province  = as.character(province),        # make sure codes are character first
    Year      = year(date),
    Month     = month(date),
    Quarter   = case_when(
      Month %in% 1:3   ~ 'Q1',
      Month %in% 4:6   ~ 'Q2',
      Month %in% 7:9   ~ 'Q3',
      Month %in% 10:12 ~ 'Q4'
    ),
    # --- Two groups by province code (strip any leading zeros) ---
    Province_Group = factor(
      dplyr::if_else(gsub("^0+", "", province) %in% c("7","9","18","23"), "Marine", "Inland"),
      levels = c("Marine","Inland")
    ),
    week         = paste0("Week", ceiling(day(date) / 7)),
    catch        = as.numeric(aqu_fishcatch_aqu_catch),
    area_code_aqu= as.character(aqu_fishcatch_area_aqu),
    aqu_type     = as.character(aqu_fishcatch_aqu_type),
    aqu_fishtype = as.character(aqu_fishcatch_aqu_fishtype)
  ) %>%
  left_join(province_lookup, by = c("province" = "province_code")) %>%
  left_join(fish_label_lookup, by = c("aqu_fishcatch_aqu_fishtype" = "fish_code")) %>%
  left_join(area_lookup, by = c("area_code_aqu" = "area_code")) %>%
  mutate(
    province_kh  = coalesce(province_kh, province),
    province_en  = coalesce(province_en, province),
    fish_name_kh = coalesce(fish_name_kh, aqu_fishtype),
    fish_name_en = coalesce(fish_name_en, aqu_fishtype),
    area_en      = coalesce(area_en, area_code_aqu)
  )


# FIX: Added unlist() to handle the list date column 
processing_raw <- processing_df %>%
  mutate(date = unlist(date)) %>%
  filter(!is.na(processing_amountprocessing)) %>%
  mutate(
    date      = as.Date(date),
    province  = as.character(province),  # ensure codes are character first
    Year      = year(date),
    Month     = month(date),
    Quarter   = case_when(
      Month %in% 1:3   ~ 'Q1',
      Month %in% 4:6   ~ 'Q2',
      Month %in% 7:9   ~ 'Q3',
      Month %in% 10:12 ~ 'Q4'
    ),
    # --- NEW: two groups by province code (strip any leading zeros) ---
    Province_Group = factor(
      dplyr::if_else(gsub("^0+", "", province) %in% c("7","9","18","23"), "Marine", "Inland"),
      levels = c("Marine","Inland")
    ),
    week                     = paste0("Week", ceiling(day(date) / 7)),
    processing_amount        = as.numeric(processing_amountprocessing),
    processing_typeprocessing= as.character(processing_typeprocessing),
    area_code_pro            = as.character(processing_area_pro)
  ) %>%
  left_join(province_lookup,           by = c("province" = "province_code")) %>%
  left_join(area_lookup,               by = c("area_code_pro" = "area_code")) %>%
  left_join(processed_fish_lookup,     by = c("processing_typeprocessing" = "proc_code")) %>%
  mutate(
    province_kh  = coalesce(province_kh, province),
    province_en  = coalesce(province_en, province),
    proc_name_kh = coalesce(proc_name_kh, processing_typeprocessing),
    proc_name_en = coalesce(proc_name_en, processing_typeprocessing),
    area_en      = coalesce(area_en, area_code_pro)
  )


# FIX: Added unlist() to handle the list date column
patrol_raw <- patrol_df %>%
  mutate(date = unlist(date)) %>%
  mutate(
    date      = as.Date(date),
    province  = as.character(province),  # ensure codes are character first
    Year      = year(date),
    Month     = month(date),
    Quarter   = case_when(
      Month %in% 1:3   ~ 'Q1',
      Month %in% 4:6   ~ 'Q2',
      Month %in% 7:9   ~ 'Q3',
      Month %in% 10:12 ~ 'Q4'
    ),
    # --- Two groups by province code (strip possible leading zeros) ---
    Province_Group = factor(
      dplyr::if_else(gsub("^0+", "", province) %in% c("7","9","18","23"), "Marine", "Inland"),
      levels = c("Marine","Inland")
    ),
    week               = paste0("Week", ceiling(lubridate::day(date) / 7)),
    patrol_amountcrime = as.numeric(patrol_amount)
  ) %>%
  left_join(province_lookup, by = c("province" = "province_code")) %>%
  mutate(
    province_kh = coalesce(province_kh, province),
    province_en = coalesce(province_en, province)
  )


# --- COMBINED Dataset for Summary ---
# all_fish_data_raw <- bind_rows(
#   nat_raw %>%
#     select(Year, Month, Quarter, catch, province_en, area_en, area_code_nat) %>%
#     rename(province = province_en) %>%
#     mutate(area_group = case_when(
#       area_code_nat == "1" ~ "Freshwater Capture",
#       area_code_nat == "2" ~ "Marine Capture",
#       TRUE ~ "Other Capture"
#     )),
#   aqu_raw %>%
#     select(Year, Month, Quarter, catch, province_kh) %>%
#     rename(province = province_kh
#            ) %>%
#     mutate(area_group = "Aquaculture", area_en = NA_character_, area_code_nat = NA_character_)
# )


# --- Prepare `selectInput` Choices ---

# Years
all_years <- sort(unique(c(nat_raw$Year, aqu_raw$Year, processing_raw$Year, patrol_raw$Year)))
year_choices <- c("All" = "All", as.list(as.character(all_years)))

# Weeks (values like "Week1", "Week2", ...)
all_week <- unique(c(nat_raw$week, aqu_raw$week, processing_raw$week, patrol_raw$week))
all_week <- all_week[!is.na(all_week)]

# Enforce logical order Week1..Week5 (avoid alphabetical weirdness)
week_levels <- paste0("Week", 1:5)
all_week <- as.character(factor(all_week, levels = week_levels))
all_week <- all_week[!is.na(all_week)]
week_choices <- c("All" = "All", as.list(all_week))

# Quarters
all_quarter <- sort(unique(c(nat_raw$Quarter, aqu_raw$Quarter, processing_raw$Quarter, patrol_raw$Quarter)))
quarter_choices <- c("All" = "All", as.list(all_quarter))

# # Months (numeric 1..12 -> labels "January"...)
# all_month <- sort(unique(c(nat_raw$Month, aqu_raw$Month, processing_raw$Month, patrol_raw$Month)))
# all_month <- all_month[!is.na(all_month)]
# # Full names as labels; numeric (character) as values
# month_choices <- setNames(
#   as.character(all_month),                                # values returned (e.g., "1","2",...)
#   as.character(lubridate::month(all_month, label = TRUE, abbr = FALSE))  # labels shown
# )

# Months
all_month <- sort(unique(c(nat_raw$Month, aqu_raw$Month, processing_raw$Month, patrol_raw$Month)))
all_month <- all_month[!is.na(all_month)]

# Build choices for the Province_Group selector
groups <- unique(as.character(nat_raw$Province_Group, aqu_raw$Province_Group,processing_raw$Province_Group,patrol_raw$Province_Group))
groups <- groups[!is.na(groups)]

# Enforce canonical order (Marine first, then Inland)
order <- c("Marine", "Inland")
groups <- order[order %in% groups]

province_group_choices <- c("All" = "All", setNames(groups, groups))


# Add "All" at the beginning
month_choices <- c(
  "All" = "All",
  setNames(
    as.character(all_month),  # values returned (e.g., "1","2",...)
    as.character(lubridate::month(all_month, label = TRUE, abbr = FALSE))  # labels shown
  )
)

# If you prefer Jan, Feb, ...
# month_choices <- setNames(as.character(all_month), as.character(lubridate::month(all_month, label = TRUE, abbr = TRUE)))

# Provinces (English label shown, Khmer value returned)
all_provinces <- nat_raw %>%
  dplyr::bind_rows(aqu_raw, processing_raw, patrol_raw) %>%
  dplyr::distinct(province_kh, province_en) %>%
  dplyr::filter(!is.na(province_en), !is.na(province_kh)) %>%
  dplyr::arrange(province_en)

province_choices <- setNames(all_provinces$province_kh, all_provinces$province_en)
province_choices <- c("All" = "All", province_choices)

# Areas & processing sources
all_areas <- sort(unique(c(nat_raw$area_en, processing_raw$area_en, aqu_raw$area_en)))
area_choices <- c("All" = "All", as.list(all_areas))

processing_sources <- sort(unique(processing_raw$processing_source))
source_choices <- c("All" = "All", as.list(processing_sources))

