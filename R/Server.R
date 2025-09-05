server <- function(input, output, session) {
  

  ##############################################################################
  
  filtered_data <- reactive({
    req(nat_raw)
    
    # Step 1: Prepare data
    df <- nat_raw %>%
      mutate(
        date    = suppressWarnings(lubridate::as_date(date)),
        year    = suppressWarnings(lubridate::year(date)),
        month   = suppressWarnings(lubridate::month(date)),
        quarter = Quarter,                   # "Q1".."Q4"
        week    = as.character(week),        # "Week1".."Week5"
        catch   = suppressWarnings(as.integer(round(catch)))
      ) %>%
      filter(
        !is.na(date),
        !is.na(year),
        !is.na(catch),
        catch > 0,
        area_code_nat == "2",                # keep if you truly want marine area only
        province_kh != "កំពង់ធំ"
      )
    

    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df <- df %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        df <- df %>% dplyr::filter(as.character(area_code_nat) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      df <- df %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      df <- df %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
    
    
    
    if (!is.null(input$year) && input$year != "All") {
      df <- df %>% dplyr::filter(year == suppressWarnings(as.integer(input$year)))
    }
    if (!is.null(input$month) && input$month != "All") {
      df <- df %>% dplyr::filter(month == suppressWarnings(as.integer(input$month)))
    }
    if (!is.null(input$quarter) && input$quarter != "All") {
      df <- df %>% dplyr::filter(quarter == input$quarter)
    }
    if (!is.null(input$week) && input$week != "All") {
      df <- df %>% dplyr::filter(week == input$week)
    }
    
    # Step 3: Summarise catch by fish × province
    summary_table <- df %>%
      dplyr::group_by(fish_name_kh, province_kh) %>%
      dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = province_kh,
        values_from = catch,
        values_fill = 0
      )
    
    # Step 4: Total + Top 51
    catch_cols <- setdiff(names(summary_table), "fish_name_kh")
    final_table <- summary_table %>%
      dplyr::mutate(`សរុប` = rowSums(dplyr::select(., dplyr::all_of(catch_cols)), na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(`សរុប`)) %>%
      dplyr::slice_head(n = 51)
    
    # Step 5: Handle empty result
    if (nrow(final_table) == 0) {
      return(data.frame(
        fish_name_kh = character(),
        `សរុប` = integer(),
        check.names = FALSE
      ))
    }
    
    # Step 6: Return
    as.data.frame(final_table, stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  
  ##############################################################################
  
  filtered_data_detail <- reactive({
    req(nat_raw)
    
    # 0) Safety: coerce and prep once
    df <- nat_raw %>%
      mutate(
        date  = suppressWarnings(lubridate::as_date(date)),
        year  = suppressWarnings(lubridate::year(date)),
        month = suppressWarnings(lubridate::month(date)),
        week  = as.character(week),                      # "Week1", "Week2", ...
        quarter = Quarter,                               # already "Q1".."Q4"
        catch = suppressWarnings(as.integer(round(catch))),
        # area_nat = nat_fishcatch_area_nat               # "1" Freshwater, "2" Marine
      ) %>%
      filter(!is.na(date), !is.na(year), !is.na(catch), catch > 0)
    
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df <- df %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        df <- df %>% dplyr::filter(as.character(area_code_nat) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      df <- df %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      df <- df %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
  
    
    # 3) Year filter      ("All" keeps all)
    if (!is.null(input$year) && input$year != "All") {
      df <- df %>% filter(year == suppressWarnings(as.numeric(input$year)))
    }
    
    # 4) Month filter     (value should be numeric-as-string "1".."12"; "All" keeps all)
    if (!is.null(input$month) && input$month != "All") {
      df <- df %>% filter(month == suppressWarnings(as.integer(input$month)))
    }
    
    # 5) Quarter filter   (values "Q1".."Q4"; "All" keeps all)
    if (!is.null(input$quarter) && input$quarter != "All") {
      df <- df %>% filter(quarter == input$quarter)
    }
    
    # 6) Week-of-month filter (values "Week1".."Week5"; "All" keeps all)
    if (!is.null(input$week) && input$week != "All") {
      df <- df %>% filter(week == input$week)
    }
    
    # If you still want to exclude Kampong Thom globally, uncomment:
    # df <- df %>% filter(province_kh != "កំពង់ធំ")
    
    # 7) Summarise catch by fish × province (handles All vs single province)
    summary_table <- df %>%
      dplyr::group_by(fish_name_kh, province_kh) %>%
      dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = province_kh,
        values_from = catch,
        values_fill = 0
      )
    
    # 8) Add Total column across provinces
    catch_cols <- setdiff(names(summary_table), "fish_name_kh")
    final_table <- summary_table %>%
      dplyr::mutate(`សរុប` = rowSums(dplyr::select(., dplyr::all_of(catch_cols)), na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(`សរុប`))
    
    # 9) Empty-safe return with fixed columns if no data
    if (nrow(final_table) == 0) {
      return(data.frame(
        fish_name_kh = character(),
        `សរុប` = integer(),
        check.names = FALSE
      ))
    }
    
    as.data.frame(final_table, stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  ##############################################################################

  fishing_products <- reactive({
    req(nat_raw)
    
    # Step 1: Prepare data from nat_raw
    df <- nat_raw %>%
      mutate(
        province   = as.character(province),
        date       = suppressWarnings(lubridate::as_date(date)),  # ensure Date format
        year       = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        month      = suppressWarnings(lubridate::month(date)),
        quarter    = Quarter,            # Assumes Quarter is already in "Q1".."Q4"
        week       = as.character(week), # Assumes week is in "Week1".."Week5"
        catch      = suppressWarnings(as.integer(round(catch)))
      )
    
    # Step 2: Filter for Marine Capture and exclude "កំពង់ធំ"
    df_marine <- df %>%
      filter(
        nat_fishcatch_area_nat == "2",
        province_kh != "កំពង់ធំ"
      )
    
    
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df_marine <- df_marine %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        df_marine <- df_marine %>% dplyr::filter(as.character(area_code_nat) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      df_marine <- df_marine %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      df_marine <- df_marine %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
    # Step 3: Filter by year and determine target_years
    if (!is.null(input$year) && input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_marine <- df_marine %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_marine$year))
    }
    
    
    
    if (!is.null(input$quarter) && input$quarter != "All") {
      df_marine <- df_marine %>% filter(quarter == input$quarter)
    }
    
    if (!is.null(input$month) && input$month != "All") {
      df_marine <- df_marine %>% filter(month == as.integer(input$month))
    }
    
    if (!is.null(input$week) && input$week != "All") {
      df_marine <- df_marine %>% filter(week == input$week)
    }
    
    # Step 5: Clean and select
    df_cleaned <- df_marine %>%
      mutate(catch = as.integer(round(catch))) %>%
      select(province_kh, catch, year) %>%
      filter(!is.na(year))
    
    # Step 6: Summarise total catch by province and year
    summary_table <- df_cleaned %>%
      group_by(province_kh, year) %>%
      summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = total_catch,
        values_fill = 0
      )
    
    # Step 7: Ensure target year columns exist
    for (yr in as.character(target_years)) {
      if (!yr %in% colnames(summary_table)) {
        summary_table[[yr]] <- 0L
      }
    }
    
    # Step 8: Create province order dynamically from df_marine
    province_order <- df_marine %>%
      distinct(province_kh) %>%
      pull(province_kh)
    
    # Step 9: Add missing provinces
    missing_provs <- setdiff(province_order, summary_table$province_kh)
    if (length(missing_provs) > 0) {
      to_add <- tibble::tibble(province_kh = missing_provs)
      for (yr in as.character(target_years)) to_add[[yr]] <- 0L
      summary_table <- dplyr::bind_rows(summary_table, to_add)
    }
    
    # Step 10: Order provinces
    summary_table <- summary_table %>%
      arrange(match(province_kh, province_order))
    
    # Step 11: Return result or show error
    if (nrow(summary_table) == 0) {
      showNotification("No data available for the selected filters.", type = "error")
      return(NULL)
    }
    
    as.data.frame(summary_table, stringsAsFactors = FALSE)
  })
  
  ##############################################################################
  
  fishing_products_rice_field <- reactive({
    # Full official province list in correct order
    province_order <- c(
      "បន្ទាយមានជ័យ", "បាត់ដំបង", "កំពង់ចាម", "កំពង់ឆ្នាំង", "កំពង់ស្ពឺ",
      "កំពង់ធំ", "កំពត", "កណ្ដាល", "កោះកុង", "ក្រចេះ",
      "មណ្ឌលគិរី", "ភ្នំពេញ", "ព្រះវិហារ", "ព្រៃវែង", "ពោធិ៍សាត់",
      "រតនគិរី", "សៀមរាប", "ព្រះសីហនុ", "ស្ទឹងត្រែង", "ស្វាយរៀង",
      "តាកែវ", "ឧត្ដរមានជ័យ", "កែប", "ប៉ៃលិន", "ត្បូងឃ្មុំ"
    )
    
    df <- nat_raw %>%
      mutate(
        province   = as.character(province),
        date       = suppressWarnings(lubridate::as_date(date)),
        year       = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        month      = suppressWarnings(lubridate::month(date)),
        quarter    = Quarter,                      # Assume it's already Q1-Q4
        week       = as.character(week),           # Assume "Week1", "Week2", etc.
        catch      = suppressWarnings(as.integer(round(catch)))
      )
    
    # Step 2: Filter for Freshwater Rice Field Fisheries
    df_rice <- df %>%
      filter(
        nat_fishcatch_area_nat == "1",
        nat_fishcatch_type_fishing == "3"
      )
    
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df_rice <- df_rice %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        df_rice <- df_rice %>% dplyr::filter(as.character(area_code_nat) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      df_rice <- df_rice %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      df_rice <- df_rice %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
    
    # Step 3: Filter by year and define target_years
    if (!is.null(input$year) && input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_rice <- df_rice %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_rice$year))
    }
    
    if (!is.null(input$quarter) && input$quarter != "All") {
      df_rice <- df_rice %>% filter(quarter == input$quarter)
    }
    
    if (!is.null(input$month) && input$month != "All") {
      df_rice <- df_rice %>% filter(month == as.integer(input$month))
    }
    
    if (!is.null(input$week) && input$week != "All") {
      df_rice <- df_rice %>% filter(week == input$week)
    }
    
    # Step 5: Clean and select
    df_cleaned <- df_rice %>%
      mutate(catch = as.integer(round(catch))) %>%
      select(province_kh, catch, year) %>%
      filter(!is.na(year))
    
    # Step 6: Summarise total catch by province and year
    summary_table <- df_cleaned %>%
      group_by(province_kh, year) %>%
      summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop")
    
    # Step 7: Create all combinations of province × year
    all_provinces_df <- expand.grid(
      province_kh = province_order,
      year = target_years
    ) %>%
      mutate(year = as.integer(year))
    
    # Step 8: Left join with summarized data, fill NAs
    final_table <- all_provinces_df %>%
      left_join(summary_table, by = c("province_kh", "year")) %>%
      mutate(total_catch = tidyr::replace_na(total_catch, 0L)) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = total_catch,
        values_fill = 0
      ) %>%
      arrange(match(province_kh, province_order))
    
    # Step 9: Return
    if (nrow(final_table) == 0) {
      showNotification("No data available for the selected year and province.", type = "error")
      return(NULL)
    }
    
    as.data.frame(final_table, stringsAsFactors = FALSE)
  })
  
  ##############################################################################
  
  fishing_products_dai <- reactive({
    # ✅ Fixed province list for Dai fishing
    province_order <- c("កណ្ដាល", "ភ្នំពេញ", "ព្រៃវែង")
    
    df <- nat_raw %>%
      mutate(
        province   = as.character(province),
        date       = suppressWarnings(lubridate::as_date(date)),
        year       = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        month      = suppressWarnings(lubridate::month(date)),
        quarter    = Quarter,            # Assumes Quarter column exists (Q1 to Q4)
        week       = as.character(week), # e.g., Week1, Week2, etc.
        catch      = suppressWarnings(as.integer(round(catch)))
      )
    
    # Step 2: Filter for Dai fishing (Freshwater, type 1), only 3 provinces
    df_marine <- df %>%
      filter(
        nat_fishcatch_area_nat == "1",
        nat_fishcatch_type_fishing == "1",
        province_kh %in% province_order
      )
    
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df_marine <- df_marine %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        df_marine <- df_marine %>% dplyr::filter(as.character(area_code_nat) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      df_marine <- df_marine %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      df_marine <- df_marine %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
    
    # Step 3: Year filter
    if (!is.null(input$year) && input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_marine <- df_marine %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_marine$year))
    }
    
    # Step 5: Additional optional filters
    if (!is.null(input$quarter) && input$quarter != "All") {
      df_marine <- df_marine %>% filter(quarter == input$quarter)
    }
    
    if (!is.null(input$month) && input$month != "All") {
      df_marine <- df_marine %>% filter(month == as.integer(input$month))
    }
    
    if (!is.null(input$week) && input$week != "All") {
      df_marine <- df_marine %>% filter(week == input$week)
    }
    
    # Step 6: Clean & select
    df_cleaned <- df_marine %>%
      mutate(catch = as.integer(round(catch))) %>%
      select(province_kh, catch, year) %>%
      filter(!is.na(year))
    
    # Step 7: Summarise catch by province & year
    summary_table <- df_cleaned %>%
      group_by(province_kh, year) %>%
      summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = total_catch,
        values_fill = 0
      )
    
    # Step 8: Ensure year columns exist
    for (yr in as.character(target_years)) {
      if (!yr %in% colnames(summary_table)) {
        summary_table[[yr]] <- 0L
      }
    }
    
    # Step 9: Add any missing provinces
    missing_provs <- setdiff(province_order, summary_table$province_kh)
    if (length(missing_provs) > 0) {
      to_add <- tibble::tibble(province_kh = missing_provs)
      for (yr in as.character(target_years)) to_add[[yr]] <- 0L
      summary_table <- dplyr::bind_rows(summary_table, to_add)
    }
    
    # Step 10: Reorder provinces
    summary_table <- summary_table %>%
      arrange(match(province_kh, province_order))
    
    # Step 11: Return or notify
    if (nrow(summary_table) == 0) {
      showNotification("No data available for the selected filters.", type = "error")
      return(NULL)
    }
    
    as.data.frame(summary_table, stringsAsFactors = FALSE)
  })
  
  
  ##############################################################################
  
  fishing_products_fishery_domain <- reactive({
    # ✅ Full official province list
    province_order <- c(
      "បន្ទាយមានជ័យ", "បាត់ដំបង", "កំពង់ចាម", "កំពង់ឆ្នាំង", "កំពង់ស្ពឺ",
      "កំពង់ធំ", "កំពត", "កណ្ដាល", "កោះកុង", "ក្រចេះ",
      "មណ្ឌលគិរី", "ភ្នំពេញ", "ព្រះវិហារ", "ព្រៃវែង", "ពោធិ៍សាត់",
      "រតនគិរី", "សៀមរាប", "ព្រះសីហនុ", "ស្ទឹងត្រែង", "ស្វាយរៀង",
      "តាកែវ", "ឧត្ដរមានជ័យ", "កែប", "ប៉ៃលិន", "ត្បូងឃ្មុំ"
    )
    
    df <- nat_raw %>%
      mutate(
        province   = as.character(province),
        date       = suppressWarnings(lubridate::as_date(date)),
        year       = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        month      = suppressWarnings(lubridate::month(date)),
        quarter    = Quarter,             # Assumes existing column like "Q1"
        week       = as.character(week),  # "Week1".."Week5"
        catch      = suppressWarnings(as.integer(round(catch)))
      )
    
    # ✅ Step 2: Filter for Fishery Domain
    df_marine <- df %>%
      filter(
        nat_fishcatch_area_nat == "1",
        nat_fishcatch_type_fishing == "2",
        province_kh %in% province_order
      )
    
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df_marine <- df_marine %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        df_marine <- df_marine %>% dplyr::filter(as.character(area_code_nat) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      df_marine <- df_marine %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      df_marine <- df_marine %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    

    # ✅ Step 3: Year filter
    if (!is.null(input$year) && input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_marine <- df_marine %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_marine$year))
    }
    
    # ✅ Step 5: Quarter, Month, Week filters
    if (!is.null(input$quarter) && input$quarter != "All") {
      df_marine <- df_marine %>% filter(quarter == input$quarter)
    }
    
    if (!is.null(input$month) && input$month != "All") {
      df_marine <- df_marine %>% filter(month == as.integer(input$month))
    }
    
    if (!is.null(input$week) && input$week != "All") {
      df_marine <- df_marine %>% filter(week == input$week)
    }
    
    # ✅ Step 6: Clean
    df_cleaned <- df_marine %>%
      mutate(catch = as.integer(round(catch))) %>%
      select(province_kh, catch, year) %>%
      filter(!is.na(year))
    
    # ✅ Step 7: Summarise
    summary_table <- df_cleaned %>%
      group_by(province_kh, year) %>%
      summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = total_catch,
        values_fill = 0
      )
    
    # ✅ Step 8: Ensure year columns exist
    for (yr in as.character(target_years)) {
      if (!yr %in% colnames(summary_table)) {
        summary_table[[yr]] <- 0L
      }
    }
    
    # ✅ Step 9: Add missing provinces
    missing_provs <- setdiff(province_order, summary_table$province_kh)
    if (length(missing_provs) > 0) {
      to_add <- tibble::tibble(province_kh = missing_provs)
      for (yr in as.character(target_years)) to_add[[yr]] <- 0L
      summary_table <- dplyr::bind_rows(summary_table, to_add)
    }
    
    # ✅ Step 10: Order provinces
    summary_table <- summary_table %>%
      arrange(match(province_kh, province_order))
    
    # ✅ Step 11: Return or notify
    if (nrow(summary_table) == 0) {
      showNotification("No data available for the selected filters.", type = "error")
      return(NULL)
    }
    
    as.data.frame(summary_table, stringsAsFactors = FALSE)
  })

  #############################################################################
  
  aquaculture_summary <- reactive({
    req(aqu_raw)
    
    # --- Step 0: Define fish mapping ---
    target_fish_map <- c(
      "78" = "អន្ទង់",
      "77" = "កង្កែប",
      "86" = "កន្ធាយ",
      "84" = "បង្កង់",
      "FM046" = "បង្គា",
      "FM023" = "ក្តាមថ្ម",
      "FM027" = "គ្រែងឈាម",
      "22" = "គ្រុំចំពុះទាក្រៀម",
      "85" = "ក្រពើ"
    )
    target_fish_kh <- unname(target_fish_map)
    
    # --- Step 1: Prepare & clean data ---
    base_data <- aqu_raw %>%
      mutate(
        area_num = as.numeric(aqu_fishcatch_area_aqu),
        year = suppressWarnings(as.integer(Year)),
        month = suppressWarnings(as.integer(Month)),
        quarter = Quarter,
        week = week,
        catch_val = as.numeric(aqu_fishcatch_aqu_catch),
        fish_code = as.character(aqu_fishcatch_aqu_fishtype),
        fish_kh = target_fish_map[fish_code]
      ) %>%
      filter(!is.na(catch_val), catch_val > 0)
    
    # --- Step 2: Apply all filters BEFORE aggregation ---
    
    # ✅ Year filter (both current and previous)
    if (!is.null(input$year) && input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      base_data <- base_data %>% filter(year %in% target_years)
    }
    
   
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      base_data <- base_data %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        base_data <- base_data %>% dplyr::filter(as.character(area_code_aqu) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      base_data <- base_data %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      base_data <- base_data %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
    
    
    # ✅ Quarter filter
    if (!is.null(input$quarter) && input$quarter != "All") {
      base_data <- base_data %>% filter(quarter == input$quarter)
    }
    
    # ✅ Month filter
    if (!is.null(input$month) && input$month != "All") {
      base_data <- base_data %>% filter(month == as.integer(input$month))
    }
    
    # ✅ Week filter
    if (!is.null(input$week) && input$week != "All") {
      base_data <- base_data %>% filter(week == input$week)
    }
    
    # --- Step 3: Aggregate catch by fish type ---
    fish_summary <- base_data %>%
      filter(!is.na(fish_kh)) %>%
      group_by(province_kh, fish_kh) %>%
      summarise(catch = sum(catch_val, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = fish_kh,
        values_from = catch,
        values_fill = 0
      )
    
    # --- Step 4: Ensure all expected fish columns are present ---
    for (col in target_fish_kh) {
      if (!col %in% names(fish_summary)) {
        fish_summary[[col]] <- 0
      }
    }
    
    # --- Step 5: Final format ---
    final_data <- fish_summary %>%
      mutate(`សរុប` = rowSums(across(all_of(target_fish_kh)), na.rm = TRUE)) %>%
      rename(`ខេត្ត` = province_kh) %>%
      mutate(across(all_of(target_fish_kh), ~ as.integer(round(.x)))) %>%
      mutate(`សរុប` = as.integer(round(`សរុប`))) %>%
      select(`ខេត្ត`, all_of(target_fish_kh), `សរុប`) %>%
      arrange(desc(`សរុប`))
    
    as.data.frame(final_data, stringsAsFactors = FALSE)
  })
  
  
  ##############################################################################
  
  aquaculture_summary_detail <- reactive({
    req(aqu_raw)
    
    # Define target fish code-label mapping
    target_fish_map <- c(
      "78" = "អន្ទង់",
      "77" = "កង្កែប",
      "86" = "កន្ធាយ",
      "84" = "បង្កង់",
      "FM046" = "បង្គា",
      "FM023" = "ក្តាមថ្ម",
      "FM027" = "គ្រែងឈាម",
      "22" = "គ្រុំចំពុះទាក្រៀម",
      "85" = "ក្រពើ"
    )
    
    target_fish_kh <- unname(target_fish_map)
    
    # --- Step 1: Prepare clean base data ---
    base_data <- aqu_raw %>%
      mutate(
        year = suppressWarnings(as.integer(Year)),
        month = suppressWarnings(as.integer(Month)),
        quarter = Quarter,
        week = week,
        area_num = as.numeric(aqu_fishcatch_area_aqu),
        catch_val = as.numeric(aqu_fishcatch_aqu_catch),
        fish_code = as.character(aqu_fishcatch_aqu_fishtype),
        fish_kh = target_fish_map[fish_code],
        province_kh = trimws(province_kh)
      ) %>%
      filter(!is.na(catch_val), catch_val > 0)
    
    # --- Step 2: Apply filters before aggregation ---
    
    # ✅ Year filter (current + previous year)
    if (!is.null(input$year) && input$year != "All") {
      N <- as.integer(input$year)
      base_data <- base_data %>% filter(year %in% c(N - 1, N))
    }
    
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      base_data <- base_data %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        base_data <- base_data %>% dplyr::filter(as.character(area_code_aqu) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      base_data <- base_data %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      base_data <- base_data %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
    # ✅ Quarter filter
    if (!is.null(input$quarter) && input$quarter != "All") {
      base_data <- base_data %>% filter(quarter == input$quarter)
    }
    
    # ✅ Month filter
    if (!is.null(input$month) && input$month != "All") {
      base_data <- base_data %>% filter(month == as.integer(input$month))
    }
    
    # ✅ Week filter
    if (!is.null(input$week) && input$week != "All") {
      base_data <- base_data %>% filter(week == input$week)
    }
    
    # --- Step 3: Summarize Inland & Marine catch ---
    special_summary <- base_data %>%
      group_by(province_kh, year) %>%
      summarise(
        `ត្រីទឹកសាប` = sum(ifelse(area_num == 1, catch_val, 0), na.rm = TRUE),
        `ត្រីសមុទ្រ` = sum(ifelse(area_num == 2, catch_val, 0), na.rm = TRUE),
        .groups = "drop"
      )
    
    # --- Step 4: Summarize by Fish Type ---
    other_fish <- base_data %>%
      filter(!is.na(fish_kh)) %>%
      group_by(province_kh, year, fish_kh) %>%
      summarise(catch = sum(catch_val, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = fish_kh,
        values_from = catch,
        values_fill = 0
      )
    
    # --- Step 5: Merge both summaries ---
    summary_table <- special_summary %>%
      left_join(other_fish, by = c("province_kh", "year")) %>%
      mutate(across(-c(province_kh, year), ~ replace_na(.x, 0)))
    
    # --- Step 6: Ensure all columns exist ---
    expected_cols <- c("ត្រីទឹកសាប", "ត្រីសមុទ្រ", target_fish_kh)
    missing_cols <- setdiff(expected_cols, names(summary_table))
    for (col in missing_cols) {
      summary_table[[col]] <- 0
    }
    
    # --- Step 7: Final summary ---
    final_data <- summary_table %>%
      group_by(province_kh) %>%
      summarise(across(all_of(expected_cols), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(`សរុប` = rowSums(across(all_of(expected_cols)), na.rm = TRUE)) %>%
      rename(`ខេត្ត` = province_kh) %>%
      mutate(across(all_of(expected_cols), ~ as.integer(round(.x)))) %>%
      mutate(`សរុប` = as.integer(round(`សរុប`))) %>%
      select(`ខេត្ត`, all_of(expected_cols), `សរុប`) %>%
      arrange(desc(`សរុប`))
    
    as.data.frame(final_data, stringsAsFactors = FALSE)
  })
  
  

  
  ##############################################################################
  law_enforcement <- reactive({
    req(patrol_raw)  # no need to require inputs; allow "All"
    
    # Khmer labels for the 3 enforcement outcomes
    label_map  <- c(
      "2" = "បំផ្លាញចោល (ករណី)",
      "3" = "បញ្ជូនសំណុំរឿងទៅតុលាការ (ករណី)",
      "6" = "ពិន័យអន្តរការណ៍ (ករណី)"
    )
    keep_codes <- names(label_map)
    
    # --- Step 1: Prep base frame (no area used here) ---
    df <- patrol_raw %>%
      dplyr::mutate(
        province_kh = as.character(province_kh),
        date   = suppressWarnings(as.Date(as.character(date))),
        year   = suppressWarnings(dplyr::if_else(!is.na(Year), as.integer(Year), lubridate::year(date))),
        month  = lubridate::month(date),
        quarter= dplyr::case_when(
          month %in% 1:3   ~ "Q1",
          month %in% 4:6   ~ "Q2",
          month %in% 7:9   ~ "Q3",
          month %in% 10:12 ~ "Q4",
          TRUE ~ NA_character_
        ),
        week   = paste0("Week", ceiling(lubridate::day(date) / 7))
      )
    
    # --- Step 2: Expand multi-code field and keep only the 3 codes ---
    df_codes <- df %>%
      dplyr::mutate(code_raw = as.character(patrol_enforcement)) %>%
      tidyr::separate_rows(code_raw, sep = "\\D+") %>%     # split on non-digits
      dplyr::filter(code_raw != "") %>%
      dplyr::mutate(code_chr = code_raw) %>%
      dplyr::filter(code_chr %in% keep_codes) %>%
      dplyr::mutate(patrol_label_kh = dplyr::recode(code_chr, !!!label_map))
    
    # --- Step 3: Apply filters (no area filter here) ---
    # Year (this and previous year)
    if (!is.null(input$year) && input$year != "All") {
      N <- as.integer(input$year)
      df_codes <- df_codes %>% dplyr::filter(year %in% c(N - 1L, N))
    }
    
    # Province Group
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df_codes <- df_codes %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
    }
    
    # Province (Khmer)
    if (!is.null(input$province) && input$province != "All") {
      df_codes <- df_codes %>% dplyr::filter(province_kh == input$province)
    }
    
    # Quarter / Month / Week
    if (!is.null(input$quarter) && input$quarter != "All") {
      df_codes <- df_codes %>% dplyr::filter(quarter == input$quarter)
    }
    if (!is.null(input$month) && input$month != "All") {
      df_codes <- df_codes %>% dplyr::filter(month == as.integer(input$month))
    }
    if (!is.null(input$week) && input$week != "All") {
      df_codes <- df_codes %>% dplyr::filter(week == input$week)
    }
    
    # --- Step 4: Summarize counts by province × enforcement label ---
    summary_table <- df_codes %>%
      dplyr::group_by(province_kh, patrol_label_kh) %>%
      dplyr::summarise(total = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = patrol_label_kh,
        values_from = total,
        values_fill = 0L
      )
    
    # --- Step 5: Province ordering limited to the selected group ---
    # start from your global province_choices (values are Khmer names)
    province_order <- unname(province_choices[province_choices != "All"])
    
    # if a group is selected, restrict the order to that group's provinces only
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      marine_codes <- c("7","9","18","23")
      group_set_kh <- province_lookup %>%
        dplyr::mutate(code_norm = gsub("^0+", "", as.character(province_code)),
                      Province_Group = dplyr::if_else(code_norm %in% marine_codes, "Marine", "Inland")) %>%
        dplyr::filter(Province_Group == input$Province_Group) %>%
        dplyr::pull(province_kh)
      province_order <- province_order[province_order %in% group_set_kh]
    }
    
    # Add any missing provinces (from the order) with zeros
    missing_provs <- setdiff(province_order, summary_table$province_kh)
    if (length(missing_provs) > 0) {
      to_add <- tibble::tibble(province_kh = missing_provs)
      for (lbl in unname(label_map)) to_add[[lbl]] <- 0L
      summary_table <- dplyr::bind_rows(summary_table, to_add)
    }
    
    # Ensure all 3 enforcement columns exist
    for (lbl in unname(label_map)) {
      if (!lbl %in% names(summary_table)) summary_table[[lbl]] <- 0L
    }
    
    # Order rows by our province_order
    summary_table <- summary_table %>%
      dplyr::arrange(match(province_kh, province_order))
    
    # --- Step 6: Empty guard ---
    if (nrow(summary_table) == 0) {
      showNotification("No enforcement data available for the selected filters.", type = "error")
      return(NULL)
    }
    
    as.data.frame(summary_table, stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  # 
  # law_enforcement <- reactive({
  #   req(input$year, input$province)
  #   
  #   # Labels for the 3 enforcement codes we care about
  #   label_map <- c(
  #     "2" = "បំផ្លាញចោល (ករណី)",
  #     "3" = "បញ្ជូនសំណុំរឿងទៅតុលាការ (ករណី)",
  #     "6" = "ពិន័យអន្តរការណ៍ (ករណី)"
  #   )
  #   keep_codes <- names(label_map)
  #   
  #   # --- Step 1: Prepare and clean data ---
  #   df <- patrol_raw %>%
  #     mutate(
  #       province_kh = as.character(province_kh),
  #       date = suppressWarnings(as.Date(as.character(date))),
  #       year = suppressWarnings(ifelse(!is.na(Year), as.integer(Year), lubridate::year(date))),
  #       month = lubridate::month(date),
  #       quarter = dplyr::case_when(
  #         month %in% 1:3 ~ "Q1",
  #         month %in% 4:6 ~ "Q2",
  #         month %in% 7:9 ~ "Q3",
  #         month %in% 10:12 ~ "Q4",
  #         TRUE ~ NA_character_
  #       ),
  #       week = paste0("Week", ceiling(lubridate::day(date) / 7))
  #     )
  #   
  #   # --- Step 2: Expand enforcement codes ---
  #   df_codes <- df %>%
  #     mutate(code_raw = as.character(patrol_enforcement)) %>%
  #     tidyr::separate_rows(code_raw, sep = "\\D+") %>%
  #     filter(code_raw != "") %>%
  #     mutate(code_chr = code_raw) %>%
  #     filter(code_chr %in% keep_codes) %>%
  #     mutate(patrol_label_kh = dplyr::recode(code_chr, !!!label_map))
  #   
  #   # --- Step 3: Filters ---
  #   
  #   # ✅ Year filter (this and previous year)
  #   if (!is.null(input$year) && input$year != "All") {
  #     N <- as.integer(input$year)
  #     df_codes <- df_codes %>% filter(year %in% c(N - 1, N))
  #   }
  #   
  #   if (!is.null(input$Province_Group) && input$Province_Group != "All") {
  #     df_codes <- df_codes %>% dplyr::filter(as.character(Province_Group) == input$Province_Group)
  #   }
  #   
  #   # ✅ Province filter
  #   if (!is.null(input$province) && input$province != "All") {
  #     df_codes <- df_codes %>% filter(province_kh == input$province)
  #   }
  #   
  #   # ✅ Quarter filter
  #   if (!is.null(input$quarter) && input$quarter != "All") {
  #     df_codes <- df_codes %>% filter(quarter == input$quarter)
  #   }
  #   
  #   # ✅ Month filter
  #   if (!is.null(input$month) && input$month != "All") {
  #     df_codes <- df_codes %>% filter(month == as.integer(input$month))
  #   }
  #   
  #   # ✅ Week filter
  #   if (!is.null(input$week) && input$week != "All") {
  #     df_codes <- df_codes %>% filter(week == input$week)
  #   }
  #   
  #   # --- Step 4: Summarize counts ---
  #   summary_table <- df_codes %>%
  #     group_by(province_kh, patrol_label_kh) %>%
  #     summarise(total = n(), .groups = "drop") %>%
  #     tidyr::pivot_wider(
  #       names_from = patrol_label_kh,
  #       values_from = total,
  #       values_fill = 0L
  #     )
  #   
  #   # --- Step 5: Province ordering ---
  #   province_order <- unname(province_choices[province_choices != "All"])
  #   
  #   # Add missing provinces with 0s
  #   missing_provs <- setdiff(province_order, summary_table$province_kh)
  #   if (length(missing_provs) > 0) {
  #     to_add <- tibble::tibble(province_kh = missing_provs)
  #     for (lbl in unname(label_map)) to_add[[lbl]] <- 0L
  #     summary_table <- dplyr::bind_rows(summary_table, to_add)
  #   }
  #   
  #   # Ensure all 3 enforcement columns exist
  #   for (lbl in unname(label_map)) {
  #     if (!lbl %in% names(summary_table)) {
  #       summary_table[[lbl]] <- 0L
  #     }
  #   }
  #   
  #   # Order by province
  #   summary_table <- summary_table %>%
  #     arrange(match(province_kh, province_order))
  #   
  #   # --- Step 6: Handle empty result ---
  #   if (nrow(summary_table) == 0) {
  #     showNotification("No enforcement data available for the selected filters.", type = "error")
  #     return(NULL)
  #   }
  #   
  #   as.data.frame(summary_table, stringsAsFactors = FALSE, check.names = FALSE)
  # })
  
  
#################################################################################
  
  aquaculture_pond <- reactive({
    # Khmer province order
    province_order <- c(
      "បន្ទាយមានជ័យ","បាត់ដំបង","កំពង់ចាម","កំពង់ឆ្នាំង","កំពង់ស្ពឺ",
      "កំពង់ធំ","កំពត","កណ្ដាល","កោះកុង","ក្រចេះ",
      "មណ្ឌលគិរី","ភ្នំពេញ","ព្រះវិហារ","ព្រៃវែង","ពោធិ៍សាត់",
      "រតនគិរី","សៀមរាប","ព្រះសីហនុ","ស្ទឹងត្រែង","ស្វាយរៀង",
      "តាកែវ","ឧត្ដរមានជ័យ","កែប","ប៉ៃលិន","ត្បូងឃ្មុំ"
    )
    
    # detect type column
    type_candidates <- c("aqu_fishcatch_aqu_type", "aqu_type", "aquaculture_type", "type", "aqu_type_raw")
    type_col <- intersect(type_candidates, names(aqu_raw))[1]
    
    if (is.na(type_col) || length(type_col) == 0) {
      zero_tbl <- tibble(
        province_kh     = province_order,
        `plastic soung` = 0L, ponds = 0L, batches = 0L
      )
      return(as.data.frame(zero_tbl, stringsAsFactors = FALSE, check.names = FALSE))
    }
    
    normalize_types <- function(x) {
      if (is.null(x) || length(x) == 0) return(character(0))
      txt <- paste(as.character(x), collapse = " ")
      if (!nzchar(txt)) return(character(0))
      txt <- tolower(txt)
      txt <- gsub("[_\\-]+", " ", txt)
      txt <- gsub("\\s+", " ", txt)
      toks <- unique(strsplit(trimws(txt), " ", fixed = TRUE)[[1]])
      
      kept <- character(0)
      if (any(toks %in% c("pond", "ponds"))) kept <- c(kept, "ponds")
      if (any(toks %in% c("batch", "batches"))) kept <- c(kept, "batches")
      if (any(toks %in% c("plastic", "soung", "plastic soung"))) kept <- c(kept, "plastic soung")
      
      unique(kept)
    }
    
    df <- aqu_raw %>%
      mutate(
        province    = as.character(province),
        province_kh = dplyr::coalesce(province_kh, province),
        date = suppressWarnings(as.Date(as.character(date))),
        year = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else if (!is.null(date)) {
          lubridate::year(date)
        } else {
          NA_integer_
        },
        month = lubridate::month(date),
        quarter = dplyr::case_when(
          month %in% 1:3 ~ "Q1",
          month %in% 4:6 ~ "Q2",
          month %in% 7:9 ~ "Q3",
          month %in% 10:12 ~ "Q4",
          TRUE ~ NA_character_
        ),
        week = paste0("Week", ceiling(lubridate::day(date) / 7)),
        type_text = sapply(.data[[type_col]], function(v) paste(as.character(v), collapse = " "))
      ) %>%
      mutate(type_list = lapply(type_text, normalize_types)) %>%
      tidyr::unnest_longer(type_list, values_to = "type") %>%
      filter(!is.na(type)) %>%
      mutate(count = 1L)
    
    # --- ✅ Dynamic Filtering ---
    
    # Year: include current and previous
    if (!is.null(input$year) && input$year != "All") {
      N <- as.integer(input$year)
      df <- df %>% filter(year %in% c(N - 1, N))
    }
    
    # Group first
    if (!is.null(input$Province_Group) && input$Province_Group != "All") {
      df <- df %>% dplyr::filter(as.character(Province_Group) == as.character(input$Province_Group))
      if (input$Province_Group == "Inland") {
        # Inland group MUST NOT show marine area rows
        df <- df %>% dplyr::filter(as.character(area_code_aqu) %in% c("1","01") | grepl("fresh|inland", tolower(as.character(area_en))))
      }
      # For Marine group we do NOT restrict area (both inland & marine allowed)
    }
    
    # Province next
    if (!is.null(input$province) && input$province != "All") {
      df <- df %>% dplyr::filter(province_kh == input$province)
    }
    
    # Area last (respect current selection)
    if (!is.null(input$area_en) && input$area_en != "All") {
      df <- df %>% dplyr::filter(as.character(area_en) == as.character(input$area_en))
    }
    
    
    # Quarter
    if (!is.null(input$quarter) && input$quarter != "All") {
      df <- df %>% filter(quarter == input$quarter)
    }
    
    # Month
    if (!is.null(input$month) && input$month != "All") {
      df <- df %>% filter(month == as.integer(input$month))
    }
    
    # Week
    if (!is.null(input$week) && input$week != "All") {
      df <- df %>% filter(week == input$week)
    }
    
    # --- Group & Pivot ---
    summary_table <- df %>%
      group_by(province_kh, type) %>%
      summarise(total = sum(count), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = total,
        values_fill = 0L
      )
    
    # Add missing provinces if "All" selected
    if (!"province_kh" %in% names(summary_table)) {
      summary_table <- tibble(province_kh = character())
    }
    
    for (nm in c("plastic soung", "ponds", "batches")) {
      if (!nm %in% names(summary_table)) summary_table[[nm]] <- 0L
    }
    
    if (is.null(input$province) || input$province == "All") {
      missing_provs <- setdiff(province_order, summary_table$province_kh)
      if (length(missing_provs) > 0) {
        to_add <- tibble(province_kh = missing_provs,
                         `plastic soung` = 0L, ponds = 0L, batches = 0L)
        summary_table <- dplyr::bind_rows(summary_table, to_add)
      }
    }
    
    summary_table %>%
      arrange(match(province_kh, province_order)) %>%
      select(province_kh, `plastic soung`, ponds, batches) %>%
      as.data.frame(stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  
  # ---- Table preview ----
  
  # 
  output$aquaculture_pond_table <- DT::renderDataTable({
    datatable(aquaculture_pond(), options = list(dom = "t"), rownames = FALSE)
  })

  output$law_enforcement_table <- DT::renderDataTable({
    datatable(law_enforcement(), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$catch_table <- DT::renderDataTable({
    datatable(filtered_data(), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$aquaculture_table <- DT::renderDataTable({
    datatable(aquaculture_summary(), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$fishing_products_table <- DT::renderDataTable({
    datatable(fishing_products(), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$fishing_products_rice_field_table <- DT::renderDataTable({
    datatable(fishing_products_rice_field(), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$fishing_products_fishery_domain_table <- DT::renderDataTable({
    datatable(fishing_products_fishery_domain(), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$fishing_products_dai_table <- DT::renderDataTable({
    datatable(fishing_products_dai(), options = list(dom = "t"), rownames = FALSE)
  })
  
  
  observeEvent(input$updateFileButton, {
    tryCatch({
      # Prep data
      marine_df_detail  <- as.data.frame(filtered_data_detail(),        stringsAsFactors = FALSE)
      marine_df  <- as.data.frame(filtered_data(),        stringsAsFactors = FALSE)
      aquac_df   <- as.data.frame(aquaculture_summary(),  stringsAsFactors = FALSE)
      fishing_df <- as.data.frame(fishing_products(),     stringsAsFactors = FALSE)
      rice_field_df <- as.data.frame(fishing_products_rice_field(), stringsAsFactors = FALSE)
      fishery_domain_df <- as.data.frame(fishing_products_fishery_domain(), stringsAsFactors = FALSE)
      fishing_dai_df <- as.data.frame(fishing_products_dai(), stringsAsFactors = FALSE)
      law_enforcement_df <- as.data.frame(law_enforcement(), stringsAsFactors = FALSE)
      # aquaculture_pond_df <- as.data.frame(aquaculture_pond(), stringsAsFactors = FALSE)
      # Get the data.frame from your reactive (keep original column names with spaces)
      aquaculture_pond_df <- as.data.frame(aquaculture_pond(), stringsAsFactors = FALSE, check.names = FALSE)
      
      
      # Make everything character (and format dates nicely)
      to_chr <- function(df) {
        df[] <- lapply(df, function(x) {
          if (inherits(x, "Date"))   format(x, "%Y-%m-%d")
          else if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d %H:%M:%S")
          else as.character(x)
        })
        df
      }
      marine_df  <- to_chr(marine_df)
      aquac_df   <- to_chr(aquac_df)
      fishing_df <- to_chr(fishing_df)
      rice_field_df <- to_chr(rice_field_df)
      fishery_domain_df <- to_chr(fishery_domain_df)
      fishing_dai_df <- to_chr(fishing_dai_df)
      law_enforcement_df <- to_chr(law_enforcement_df)
      aquaculture_pond_df<- to_chr(aquaculture_pond_df)
      
      # Paths
      template_path <- "D:/DCX/FIMS/25_08_2025_Anual_Report_10_08_am/Report/Fish_Annual_Tamplate.xlsx"
      
      # Use Sys.Date() to get the current date, which is a common practice for file names.
      # We then format it to a nice string like "2023-10-27".
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      # Create the output file path using the formatted date.
      out_file <- sprintf("D:/DCX/FIMS/25_08_2025_Anual_Report_10_08_am/Report/Fish_Catch_Report_%s.xlsx", current_date)
      
      # Check if the template file exists before proceeding.
      if (!file.exists(template_path)) {
        stop("Template not found: ", template_path)
      }
      
      # Attempt to copy the template to the new output file.
      if (!file.copy(template_path, out_file, overwrite = TRUE)) {
        stop("Failed to copy template")
      }
      
      # Load & write
      wb <- openxlsx2::wb_load(out_file)
      
      wb$add_data(sheet = "T06Deail_ប្រភេទត្រីសមុទ្រ", x = marine_df_detail,  startRow = 9, startCol = 2, colNames = FALSE)
      wb$add_data(sheet = "T06_ប្រភេទត្រីសមុទ្រ", x = marine_df,  startRow = 9, startCol = 2, colNames = FALSE)
      wb$add_data(sheet = "T07_ផលវារីវប្បកម្ម",   x = aquac_df,   startRow = 8, startCol = 2, colNames = FALSE)
      
      ####  ផលនេសាទសមុទ្រ
      wb$add_data(
        sheet     = "T05_ផលនេសាទសមុទ្រ",
        x         = fishing_df[, 1:2],  # first two columns
        startRow  = 8,
        startCol  = 2,                   # B
        colNames  = FALSE
      )
      
      # Write to column E (skip D)
      wb$add_data(
        sheet     = "T05_ផលនេសាទសមុទ្រ",
        x         = fishing_df[, 3, drop = FALSE],  # third column only
        startRow  = 8,
        startCol  = 5,                   # E
        colNames  = FALSE
      )
      
      #### ផលក្នុងវាលស្រែ
      wb$add_data(
        sheet     = "T04_ផលក្នុងវាលស្រែ",
        x         = rice_field_df[, 1:2],  # first two columns
        startRow  = 10,
        startCol  = 2,                   # B
        colNames  = FALSE
      )
      
      # Write to column E (skip D)
      wb$add_data(
        sheet     = "T04_ផលក្នុងវាលស្រែ",
        x         = rice_field_df[, 3, drop = FALSE],  # third column only
        startRow  = 10,
        startCol  = 5,                   # E
        colNames  = FALSE
      )
      
      
      #### ផលក្នុងដែននេសាទ
      wb$add_data(
        sheet     = "T03_ផលក្នុងដែននេសាទ",
        x         = rice_field_df[, 1:2],  # first two columns
        startRow  = 9,
        startCol  = 2,                   # B
        colNames  = FALSE
      )
      
      # Write to column E (skip D)
      wb$add_data(
        sheet     = "T03_ផលក្នុងដែននេសាទ",
        x         = rice_field_df[, 3, drop = FALSE],  # third column only
        startRow  = 9,
        startCol  = 5,                   # E
        colNames  = FALSE
      )
      
      
      #### ផលឧបករណ៍ដាយ
      wb$add_data(
        sheet     = "T02_ផលឧបករណ៍ដាយ",
        x         = fishing_dai_df[, 1:2],  # first two columns
        startRow  = 11,
        startCol  = 2,                   # B
        colNames  = FALSE
      )
      
      # Write to column E (skip D)
      wb$add_data(
        sheet     = "T02_ផលឧបករណ៍ដាយ",
        x         = fishing_dai_df[, 3, drop = FALSE],  # third column only
        startRow  = 11,
        startCol  = 5,                   # E
        colNames  = FALSE
      )
      
      #### បទល្មើស
      wb$add_data(
        sheet     = "T08_បទល្មើស",
        x         = law_enforcement_df[, 1:4],  # first two columns
        startRow  = 7,
        startCol  = 2,                   # c
        colNames  = FALSE
      )


      # Make sure the needed columns exist (fill with 0 if missing)
      for (nm in c("province_kh", "ponds", "batches", "plastic soung")) {
        if (!nm %in% names(aquaculture_pond_df)) aquaculture_pond_df[[nm]] <- 0L
      }
      
      # Write to the sheet "ស្រះ បែ ស៊ង"
      # B (2) = province, E (5) = ponds, I (9) = batches, M (13) = plastic soung
      wb$add_data(sheet = "T10_ស្រះ បែ ស៊ង",
                  x = aquaculture_pond_df[, "province_kh", drop = FALSE],
                  startRow = 8, startCol = 2, colNames = FALSE)
      
      wb$add_data(sheet = "T10_ស្រះ បែ ស៊ង",
                  x = aquaculture_pond_df[, "ponds", drop = FALSE],
                  startRow = 8, startCol = 5, colNames = FALSE)
      
      wb$add_data(sheet = "T10_ស្រះ បែ ស៊ង",
                  x = aquaculture_pond_df[, "batches", drop = FALSE],
                  startRow = 8, startCol = 9, colNames = FALSE)
      
      wb$add_data(sheet = "T10_ស្រះ បែ ស៊ង",
                  x = aquaculture_pond_df[, "plastic soung", drop = FALSE],
                  startRow = 8, startCol = 13, colNames = FALSE)
      
      
      
      openxlsx2::wb_save(wb, out_file, overwrite = TRUE)
      
      showNotification(paste("Report exported successfully:", out_file), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error exporting report:", e$message), type = "error")
    })
  })
  
  # When Province Group changes, update Province and Area lists accordingly
  observeEvent(input$Province_Group, {
    grp <- input$Province_Group
    
    # Province list
    updateSelectInput(session, "province",
                      choices  = get_province_choices_for_group(grp),
                      selected = "All"
    )
    
    # Area list: Marine -> both areas; Inland -> only inland/fresh
    new_area_choices <- get_area_choices_for_group(grp, area_choices = area_choices)
    sel <- isolate(input$area_en)
    if (is.null(sel) || !(sel %in% unname(new_area_choices))) sel <- "All"
    updateSelectInput(session, "area_en",
                      choices  = new_area_choices,
                      selected = sel
    )
  })
  

  
  # Optional: ensure Province choices match initial Group at app start
  observeEvent(TRUE, {
    updateSelectInput(session, "province",
                      choices = get_province_choices_for_group(isolate(input$Province_Group %||% "All")),
                      selected = "All")
  }, once = TRUE)
  
  
  # 2. Event handler for the "OK" button inside the modal
  observeEvent(input$confirmSelection, {
    # Store the user's selected choices into the reactive value
    download_choices_list(input$modal_dl_choices)
    # Hide the modal
    removeModal()
  })
  
  # 3. Event handler for the "Cancel" button inside the modal
  observeEvent(input$cancelSelection, {
    removeModal()
  })
  
  sidebar_open <- reactiveVal(TRUE) # Initialize as open (default state)
  
  # 2. Observe the "close" button click (from within the sidebar)
  observeEvent(input$closeSidebarBtn, {
    sidebar_open(FALSE) # Set state to closed
  })
  
  # 3. Observe the "open" button click (from within the main content)
  observeEvent(input$openSidebarBtn, {
    sidebar_open(TRUE) # Set state to open
  })
  
  # 4. Observe changes in sidebar_open to update UI elements and button visibility
  observe({
    is_open <- sidebar_open() # Get the current state (TRUE for open, FALSE for closed)
    
    if (is_open) {
      # Sidebar is intended to be OPEN
      shinyjs::removeClass(id = "sidebarPanel", class = "hidden")
      shinyjs::removeClass(id = "mainContent", class = "full-width")
      shinyjs::show(id = "closeSidebarBtn") # Show the close button on the sidebar
      shinyjs::hide(id = "openSidebarBtn")  # Hide the open button on main content
      
    } else {
      # Sidebar is intended to be CLOSED
      shinyjs::addClass(id = "sidebarPanel", class = "hidden")
      shinyjs::addClass(id = "mainContent", class = "full-width")
      shinyjs::hide(id = "closeSidebarBtn") # Hide the close button on the sidebar
      shinyjs::show(id = "openSidebarBtn")  # Show the open button on main content
    }
  })
  
  # Avoid loops when quarter updates month and month updates quarter
  qm_lock <- reactiveVal(FALSE)
  
  # Quarter -> Month (narrow month list to the selected quarter)
  observeEvent(input$quarter, ignoreInit = TRUE, {
    if (qm_lock()) return()
    qm_lock(TRUE)
    
    new_month_choices <- get_month_choices_for_quarter(input$quarter, month_choices)
    current_month <- isolate(input$month)
    selected_month <- if (!is.null(current_month) && current_month %in% unname(new_month_choices)) {
      current_month
    } else {
      "All"
    }
    
    updateSelectInput(session, "month",
                      choices  = new_month_choices,
                      selected = selected_month
    )
    
    qm_lock(FALSE)
  })
  
  # Month -> Quarter (auto-set quarter; keep month list consistent)
  observeEvent(input$month, ignoreInit = TRUE, {
    if (qm_lock()) return()
    q <- month_to_quarter(input$month)
    if (is.null(q)) return()
    
    qm_lock(TRUE)
    
    updateSelectInput(session, "quarter", selected = q)
    
    # ensure month choices show only months in that quarter
    new_month_choices <- get_month_choices_for_quarter(q, month_choices)
    if (!(input$month %in% unname(new_month_choices))) {
      updateSelectInput(session, "month",
                        choices  = new_month_choices,
                        selected = "All"
      )
    } else {
      updateSelectInput(session, "month",
                        choices  = new_month_choices
      )
    }
    
    qm_lock(FALSE)
  })
  
  
  
}