
# kobo_cleaner.R
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)

# -----------------------
# CONFIG
# -----------------------
ASSET_ID <- "aLQEf7RcyAYyquCRdDCr4J"
TOKEN <- "00ed4c22cb2cdc2bcd4ae1539c20aaa80c21b20d"

# -----------------------
# Helper: get Kobo JSON
# -----------------------
get_kobo_json <- function(asset_id = ASSET_ID, token = TOKEN) {
  url <- paste0("https://eu.kobotoolbox.org/api/v2/assets/", asset_id, "/data/?format=json")
  res <- GET(url, add_headers(Authorization = paste("Token", token)))
  content(res, as = "parsed", type = "application/json")
}

# -----------------------
# Robust date parser
# -----------------------
parse_date_safe <- function(x) {
  d <- suppressWarnings(ymd(x))
  if (all(is.na(d))) {
    d <- suppressWarnings(ymd_hms(x))
  }
  return(d)
}

# Function to clean main Kobo data (Python-equivalent)
clean_main <- function(data) {
  fishcatching_results <- data$results
  
  # Flatten JSON
  main_df <- fromJSON(toJSON(fishcatching_results), flatten = TRUE)
  
  # Drop nested repeat groups
  main_df <- main_df %>% select(-any_of(c("nat_fishcatch","aqu_fishcatch","processing","patrol")))
  
  # --- Custom name cleaning (closer to Python) ---
  names(main_df) <- names(main_df) %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "_", .) %>%   # replace symbols with _
    gsub("_+", "_", .) %>%           # collapse multiple _
    gsub("^_|_$", "", .)             # trim leading/trailing _
  
  # Type conversions
  if ("inspectorate" %in% names(main_df)) main_df$inspectorate <- as.integer(main_df$inspectorate)
  if ("province" %in% names(main_df)) main_df$province <- as.integer(main_df$province)
  if ("submission_time" %in% names(main_df)) {
    main_df$submission_time <- parse_date_safe(main_df$submission_time)
    main_df$submission_time <- as.character(main_df$submission_time) # keep as text for DB
  }
  
  # --- Drop unwanted columns (after custom names) ---
  drop_cols <- c(
    "formhub_uuid",
    "begin_group_v0pfmszve_nat_sum",
    "begin_group_v0pfmszve_aqu_sum",
    "begin_group_v0pfmszve_total_sum",
    "validation_status_color",
    "law_other_enforce",
    "validation_status_timestamp",
    "validation_status_uid",
    "validation_status_by_whom",
    "validation_status_label",
    "law_remarklaw",
    "notes",
    "meta_root_uuid",
    "meta_deprecatedid",
    "attachments",
    "submissiontime",
    "submittedby",
    "version",
    "meta_instanceid",
    "meta_rootuuid",
    "tags",
    "xform_id_string",
    "status"
  )
  main_df <- main_df %>% select(-any_of(drop_cols))
  
  # --- Fix NA/empty only for character columns ---
  main_df <- main_df %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.x) | .x == "", "null", .x)))
  
  return(main_df)
}



clean_nat_fishcatch <- function(data) {
  # Make sure results are there
  if (!"results" %in% names(data)) {
    stop("Input data does not contain $results. Did you call get_kobo_json()?")
  }
  
  results_ds <- fromJSON(toJSON(data$results), flatten = TRUE)
  
  if (!"nat_fishcatch" %in% names(results_ds)) {
    warning("No nat_fishcatch field found")
    return(tibble())
  }
  
  nat_df <- results_ds %>%
    select(any_of(c("_id", "province", "date", "nat_fishcatch"))) %>%
    unnest_longer(nat_fishcatch) %>%
    unnest_wider(nat_fishcatch)
  
  # Clean names like Python
  names(nat_df) <- names(nat_df) %>%
    tolower() %>%
    gsub("[^a-z0-9]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  # Drop unwanted columns
  drop_cols <- c(
    "natfishcatch_commentnat",
    "natfishcatch_natother",
    "natfishcatch_integeroak3bkt86",
    "natfishcatch_integermle6vfl0n"
  )
  nat_df <- nat_df %>% select(-any_of(drop_cols))
  
  # Flatten list columns to strings (fixes select/mutate issues)
  nat_df <- nat_df %>%
    mutate(across(where(is.list), ~ sapply(.x, function(v) paste(v, collapse = ","))))
  
  # Fill NA with "unspecified"
  nat_df <- nat_df %>% mutate(across(everything(), ~ ifelse(is.na(.x), "unspecified", .x)))
  
  # Convert numeric fields
  if ("natfishcatch_natcatch" %in% names(nat_df)) {
    nat_df$natfishcatch_natcatch <- as.numeric(nat_df$natfishcatch_natcatch)
  }
  if ("natfishcatch_natprice" %in% names(nat_df)) {
    nat_df$natfishcatch_natprice <- as.numeric(nat_df$natfishcatch_natprice)
  }
  
  # Replace "" with "null"
  nat_df <- nat_df %>% mutate(across(everything(), ~ ifelse(.x == "", "null", .x)))
  
  return(nat_df)
}

clean_aqu_fishcatch <- function(data) {
  # Make sure results are there
  if (!"results" %in% names(data)) {
    stop("Input data does not contain $results. Did you call get_kobo_json()?")
  }
  
  results_ds <- fromJSON(toJSON(data$results), flatten = TRUE)
  
  if (!"aqu_fishcatch" %in% names(results_ds)) {
    warning("No aqu_fishcatch field found")
    return(tibble())
  }
  
  aqu_df <- results_ds %>%
    select(any_of(c("_id", "province","date","aqu_fishcatch"))) %>%
    unnest_longer(aqu_fishcatch) %>%
    unnest_wider(aqu_fishcatch)
  
  # Clean names like Python
  names(aqu_df) <- names(aqu_df) %>%
    tolower() %>%
    gsub("[^a-z0-9]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  # Drop unwanted columns
  drop_cols <- c(
    "aqufishcatch_aquother",
    "aqufishcatch_comment"
  )
  aqu_df <- aqu_df %>% select(-any_of(drop_cols))
  
  # Flatten list columns to strings
  aqu_df <- aqu_df %>%
    mutate(across(where(is.list), ~ sapply(.x, function(v) paste(v, collapse = ","))))
  
  # Convert numeric fields first, before dealing with NAs
  # This is the crucial change to preserve numeric types
  if ("aqufishcatch_aqucatch" %in% names(aqu_df)) {
    aqu_df$aqufishcatch_aqucatch <- as.numeric(aqu_df$aqufishcatch_aqucatch)
  }
  if ("aqufishcatch_aquprice" %in% names(aqu_df)) {
    aqu_df$aqufishcatch_aquprice <- as.numeric(aqu_df$aqufishcatch_aquprice)
  }
  # Add the missing conversion for the 'area' column
  if ("aqufishcatch_aqucatch_aqu_fish_area" %in% names(aqu_df)) {
    aqu_df$aqu_fishcatch_area <- as.numeric(aqu_df$aqu_fishcatch_area)
  }
  
  # Fill NA with "unspecified" only for character columns
  aqu_df <- aqu_df %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.x), "unspecified", .x)))
  
  # Fill NA with 0 for numeric columns
  aqu_df <- aqu_df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))
  
  # Replace "" with "null"
  aqu_df <- aqu_df %>%
    mutate(across(where(is.character), ~ ifelse(.x == "", "null", .x)))
  
  return(aqu_df)
}

clean_processing <- function(data) {
  results_ds <- fromJSON(toJSON(data$results), flatten = TRUE)
  
  # Extract and expand processing group
  if (!"processing" %in% names(results_ds)) {
    warning("No processing data found")
    return(tibble())
  }
  
  processing_df <- results_ds %>%
    select(any_of(c("_id", "processing", "date", "province"))) %>%
    unnest_longer(processing) %>%
    unnest_wider(processing)
  
  # Clean names (Python-style)
  names(processing_df) <- names(processing_df) %>%
    tolower() %>%
    gsub("[^a-z0-9]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  # Drop unwanted columns
  drop_cols <- c("processing_otherprocessing", "processing_remark")
  processing_df <- processing_df %>% select(-any_of(drop_cols))
  
  # Flatten any list columns into plain text
  processing_df <- processing_df %>%
    mutate(across(where(is.list), ~ sapply(.x, function(v) paste(v, collapse = ","))))
  
  # Replace NA/empty with "null"
  processing_df <- processing_df %>%
    mutate(across(everything(), ~ ifelse(is.na(.x) | .x == "", "null", .x)))
  
  return(processing_df)
}

clean_patrol <- function(data) {
  results_ds <- fromJSON(toJSON(data$results), flatten = TRUE)
  # results_ds <- fromJSON(toJSON(fishery_data$results), flatten = TRUE)
  
  if (!"patrol" %in% names(results_ds)) {
    warning("No patrol data found")
    return(tibble())
  }
  
  patrol_df <- results_ds %>%
    select(any_of(c("_id", "patrol", "date", "province"))) %>%
    unnest_longer(patrol) %>%
    unnest_wider(patrol)
  
  # Clean names (Python-style)
  names(patrol_df) <- names(patrol_df) %>%
    tolower() %>%
    gsub("[^a-z0-9]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  # Parse multi-selects (like parse_list in Python)
  if ("patrol_enforcement" %in% names(patrol_df)) {
    patrol_df$patrol_enforcement <- sapply(patrol_df$patrol_enforcement, function(x) {
      if (is.null(x) || length(x) == 0) return("null")
      if (is.list(x)) return(paste(unlist(x), collapse = ","))
      return(as.character(x))
    })
  }
  
  # Drop unwanted columns
  drop_cols <- c("patrol_remark_crime", "patrol_other_crime", "patrol_other_enforce")
  patrol_df <- patrol_df %>% select(-any_of(drop_cols))
  
  # Flatten any other list-columns
  patrol_df <- patrol_df %>%
    mutate(across(where(is.list), ~ sapply(.x, function(v) paste(v, collapse = ","))))
  
  # Replace NA/empty with "null"
  patrol_df <- patrol_df %>%
    mutate(across(everything(), ~ ifelse(is.na(.x) | .x == "", "null", .x)))
  
  return(patrol_df)
}


clean_all_kobo <- function(asset_id = ASSET_ID, token = TOKEN) {
  data <- get_kobo_json(asset_id, token)
  list(
    main        = clean_main(data),
    aqu         = clean_aqu_fishcatch(data),
    nat         = clean_nat_fishcatch(data),
    processing  = clean_processing(data),
    patrol      = clean_patrol(data)
  )
}
