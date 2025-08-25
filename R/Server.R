server <- function(input, output, session) {
  

  ##############################################################################
  
  # filtered_data <- reactive({
  #   req(nat_raw)
  #   
  #   # ——— Master list of provinces you want as columns (edit if you truly want to exclude any) ———
  #   province_levels <- nat_raw |>
  #     dplyr::distinct(province_kh) |>
  #     dplyr::pull(province_kh) |>
  #     sort()
  #   
  #   df <- nat_raw %>%
  #     dplyr::mutate(
  #       date    = suppressWarnings(lubridate::as_date(date)),
  #       year    = suppressWarnings(lubridate::year(date)),
  #       month   = suppressWarnings(lubridate::month(date)),
  #       quarter = Quarter,                  # already "Q1".."Q4"
  #       week    = as.character(week),       # "Week1".."Week5"
  #       area_nat = nat_fishcatch_area_nat,  # "1" Freshwater, "2" Marine
  #       catch   = suppressWarnings(as.integer(round(catch)))
  #     ) %>%
  #     dplyr::filter(!is.na(date), !is.na(year), !is.na(catch), catch > 0)
  #   
  #   # ——— UI filters ———
  #   if (!is.null(input$area_nat) && input$area_nat != "All") {
  #     df <- df %>% dplyr::filter(area_nat == input$area_nat)
  #   }
  #   if (!is.null(input$province) && input$province != "All") {
  #     df <- df %>% dplyr::filter(province_kh == input$province)
  #   }
  #   if (!is.null(input$year) && input$year != "All") {
  #     df <- df %>% dplyr::filter(year == suppressWarnings(as.integer(input$year)))
  #   }
  #   if (!is.null(input$month) && input$month != "All") {
  #     df <- df %>% dplyr::filter(month == suppressWarnings(as.integer(input$month)))
  #   }
  #   if (!is.null(input$quarter) && input$quarter != "All") {
  #     df <- df %>% dplyr::filter(quarter == input$quarter)
  #   }
  #   if (!is.null(input$week) && input$week != "All") {
  #     df <- df %>% dplyr::filter(week == input$week)
  #   }
  #   
  #   # ——— If you STILL want to force marine-only and exclude Kampong Thom, keep these lines. Otherwise delete them. ———
  #   # df <- df %>% dplyr::filter(area_nat == "2")
  #   # df <- df %>% dplyr::filter(province_kh != "កំពង់ធំ")
  #   
  #   # ——— Summarise; then COMPLETE missing (fish, province) combos with 0; then pivot ———
  #   summary_long <- df %>%
  #     dplyr::group_by(fish_name_kh, province_kh) %>%
  #     dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
  #     # ensure every fish has every province, filled with 0 where absent
  #     tidyr::complete(
  #       fish_name_kh,
  #       province_kh = province_levels,
  #       fill = list(catch = 0L)
  #     )
  #   
  #   final_table <- summary_long %>%
  #     # keep province order stable
  #     dplyr::mutate(province_kh = factor(province_kh, levels = province_levels)) %>%
  #     tidyr::pivot_wider(
  #       names_from  = province_kh,
  #       values_from = catch,
  #       values_fill = 0
  #     ) %>%
  #     # Add Total and keep Top 51 fish
  #     dplyr::mutate(`សរុប` = rowSums(dplyr::across(-fish_name_kh), na.rm = TRUE)) %>%
  #     dplyr::arrange(dplyr::desc(`សរុប`)) %>%
  #     dplyr::slice_head(n = 51)
  #   
  #   if (nrow(final_table) == 0) {
  #     return(data.frame(
  #       fish_name_kh = character(),
  #       `សរុប` = integer(),
  #       check.names = FALSE
  #     ))
  #   }
  #   
  #   as.data.frame(final_table, stringsAsFactors = FALSE, check.names = FALSE)
  # })
  
  
  filtered_data <- reactive({
    req(nat_raw)

    # Prep once
    df <- nat_raw %>%
      mutate(
        date    = suppressWarnings(lubridate::as_date(date)),
        year    = suppressWarnings(lubridate::year(date)),
        month   = suppressWarnings(lubridate::month(date)),
        quarter = Quarter,                  # already "Q1".."Q4" in your data
        week    = as.character(week),       # "Week1".."Week5"
        area_nat = nat_fishcatch_area_nat,  # "1" Freshwater, "2" Marine
        catch   = suppressWarnings(as.integer(round(catch)))
      ) %>%
      filter(!is.na(date), !is.na(year), !is.na(catch), catch > 0)

    # --- Filters (each respects "All") ---

    # Area (optional). If you want the old behavior (Marine-only), set default to "2"
    if (!is.null(input$area_nat) && input$area_nat != "All") {
      df <- df %>% filter(area_nat == input$area_nat)
    } else {
      # Uncomment next line to force Marine-only like before:
      # df <- df %>% filter(area_nat == "2")
    }

    # Province (Khmer value; "All" keeps all)
    if (!is.null(input$province) && input$province != "All") {
      df <- df %>% filter(province_kh == input$province)
    }

    # Year
    if (!is.null(input$year) && input$year != "All") {
      df <- df %>% filter(year == suppressWarnings(as.integer(input$year)))
    }

    # Month (expects "1".."12" from month_choices values)
    if (!is.null(input$month) && input$month != "All") {
      df <- df %>% filter(month == suppressWarnings(as.integer(input$month)))
    }

    # Quarter ("Q1".."Q4")
    if (!is.null(input$quarter) && input$quarter != "All") {
      df <- df %>% filter(quarter == input$quarter)
    }

    # Week ("Week1".."Week5")
    if (!is.null(input$week) && input$week != "All") {
      df <- df %>% filter(week == input$week)
    }

    # If you still need to exclude Kampong Thom globally, keep this:
    df <- df %>% filter(nat_fishcatch_area_nat == "2", province_kh != "កំពង់ធំ")


     # Summarise catch by fish × province
    summary_table <- df %>%
      group_by(fish_name_kh, province_kh) %>%
      summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = province_kh,
        values_from = catch,
        values_fill = 0
      )

    # Add Total column and keep Top 51
    catch_cols <- setdiff(names(summary_table), "fish_name_kh")
    final_table <- summary_table %>%
      mutate(`សរុប` = rowSums(dplyr::select(., dplyr::all_of(catch_cols)), na.rm = TRUE)) %>%
      arrange(desc(`សរុប`)) %>%
      slice_head(n = 51)

    # Empty-safe return
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
        area_nat = nat_fishcatch_area_nat               # "1" Freshwater, "2" Marine
      ) %>%
      filter(!is.na(date), !is.na(year), !is.na(catch), catch > 0)
    
    # 1) Area filter (optional control: input$area_nat in {"All","1","2"})
    # If you don't have an area filter input, keep only marine as before by uncommenting next line:
    # df <- dplyr::filter(df, area_nat == "2")
    if (!is.null(input$area_nat) && input$area_nat != "All") {
      df <- df %>% filter(area_nat == input$area_nat)
    }
    
    # 2) Province filter  (value is Khmer; "All" keeps all)
    if (!is.null(input$province) && input$province != "All") {
      df <- df %>% filter(province_kh == input$province)
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
  
  
  # filtered_data_detail <- reactive({
  #   req(nat_raw)
  #   
  #   # Step 1: Prepare data with year
  #   df <- nat_raw %>%
  #     mutate(
  #       date = suppressWarnings(lubridate::as_date(date)),
  #       year = suppressWarnings(lubridate::year(date)),
  #       catch = suppressWarnings(as.integer(round(catch)))
  #     ) %>%
  #     filter(!is.na(year), !is.na(catch), catch > 0)
  #   
  #   # Step 2: Keep only marine capture & exclude "កំពង់ធំ"
  #   df_marine <- df %>%
  #     filter(nat_fishcatch_area_nat == "2", province_kh != "កំពង់ធំ")
  #   
  #   # Step 3: Apply year filter (single year)
  #   if (!is.null(input$year) && input$year != "All") {
  #     df_marine <- df_marine %>% filter(year == as.numeric(input$year))
  #   }
  #   
  #   # Step 4: Apply province filter
  #   if (!is.null(input$province) && input$province != "All") {
  #     df_marine <- df_marine %>% filter(province_kh == input$province)
  #   }
  #   
  #   # Step 5: Get dynamic province order after filtering
  #   province_order <- df_marine %>%
  #     distinct(province_kh) %>%
  #     pull(province_kh)
  #   
  #   # Step 6: Summarise catch by fish × province
  #   summary_table <- df_marine %>%
  #     group_by(fish_name_kh, province_kh) %>%
  #     summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
  #     tidyr::pivot_wider(
  #       names_from = province_kh,
  #       values_from = catch,
  #       values_fill = 0
  #     )
  #   
  #   # Step 7: Add a "Total" column across provinces
  #   catch_cols <- setdiff(names(summary_table), "fish_name_kh")
  #   summary_table <- summary_table %>%
  #     mutate(`សរុប` = rowSums(dplyr::select(., all_of(catch_cols)), na.rm = TRUE))
  #   
  #   final_table <- summary_table %>%
  #     arrange(desc(`សរុប`))
  # 
  #   # Step 9: Return data frame
  #   as.data.frame(final_table, stringsAsFactors = FALSE)
  # })
  # 
  
  ##############################################################################
  fishing_products <- reactive({
    # Step 1: Prepare data from nat_raw
    df <- nat_raw %>%
      mutate(
        province = as.character(province),
        date = suppressWarnings(lubridate::as_date(date)),  # ensure Date format
        year = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        catch = suppressWarnings(as.integer(round(catch)))
      )
    
    # Step 2: Filter for Marine Capture and exclude "កំពង់ធំ"
    df_marine <- df %>%
      filter(nat_fishcatch_area_nat == "2") %>%
      filter(province_kh != "កំពង់ធំ") # This line will exclude the specified province
    
    # Step 3: Determine target years based on input
    if (input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_marine <- df_marine %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_marine$year))
    }
    
    # Step 4: Filter by province
    if (input$province != "All") {
      df_marine <- df_marine %>% filter(province_kh == input$province)
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
    
    # Step 8: Create province_order dynamically from df_marine
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
    
    # Step 11: Return
    if (nrow(summary_table) == 0) {
      showNotification("No data available for the selected year and province.", type = "error")
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
        province = as.character(province),
        date = suppressWarnings(lubridate::as_date(date)),
        year = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        catch = suppressWarnings(as.integer(round(catch)))
      )
    
    # Step 2: Filter for Marine Capture and exclude "កំពង់ធំ"
    df_marine <- df %>%
      filter(nat_fishcatch_area_nat == "1") %>%
      filter(nat_fishcatch_type_fishing == "3")
    
    # Step 3: Determine target years based on input
    if (input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_marine <- df_marine %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_marine$year))
    }
    
    # Step 4: Filter by province
    if (input$province != "All") {
      df_marine <- df_marine %>% filter(province_kh == input$province)
    }
    
    # Step 5: Clean and select
    df_cleaned <- df_marine %>%
      mutate(catch = as.integer(round(catch))) %>%
      select(province_kh, catch, year) %>%
      filter(!is.na(year))
    
    # Step 6: Summarise total catch by province and year
    summary_table <- df_cleaned %>%
      group_by(province_kh, year) %>%
      summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop")
    
    # Step 7: Create a data frame with all provinces and target years
    # This is a key change. We are creating a data frame with all provinces
    # and all target years, and setting the default catch to 0.
    
    all_provinces_df <- expand.grid(
      province_kh = province_order,
      year = target_years
    ) %>%
      mutate(year = as.integer(year))
    
    # Step 8: Join the summarized data with the full province list
    # This will ensure that all 25 provinces are included.
    # Provinces with no catch in the filtered data will have NA for total_catch.
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
    # ✅ Fixed province list
    province_order <- c("កណ្ដាល", "ភ្នំពេញ", "ព្រៃវែង")
    
    df <- nat_raw %>%
      mutate(
        province = as.character(province),
        date = suppressWarnings(lubridate::as_date(date)),  # ensure Date format
        year = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        catch = suppressWarnings(as.integer(round(catch)))
      )
    
    # Step 2: Filter for Marine Capture AND keep only 3 provinces
    df_marine <- df %>%
      filter(nat_fishcatch_area_nat == "1") %>%
      filter(nat_fishcatch_type_fishing == "1") %>%
      filter(province_kh %in% province_order)   # ✅ only 3 provinces
    
    # Step 3: Year filter
    if (input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_marine <- df_marine %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_marine$year))
    }
    
    # Step 4: Province filter (respect user selection)
    if (input$province != "All") {
      df_marine <- df_marine %>% filter(province_kh == input$province)
    }
    
    # Step 5: Clean
    df_cleaned <- df_marine %>%
      mutate(catch = as.integer(round(catch))) %>%
      select(province_kh, catch, year) %>%
      filter(!is.na(year))
    
    # Step 6: Summarise
    summary_table <- df_cleaned %>%
      group_by(province_kh, year) %>%
      summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = total_catch,
        values_fill = 0
      )
    
    # Step 7: Ensure year columns exist
    for (yr in as.character(target_years)) {
      if (!yr %in% colnames(summary_table)) {
        summary_table[[yr]] <- 0L
      }
    }
    
    # Step 8: Add missing provinces (if one of the 3 has no data)
    missing_provs <- setdiff(province_order, summary_table$province_kh)
    if (length(missing_provs) > 0) {
      to_add <- tibble::tibble(province_kh = missing_provs)
      for (yr in as.character(target_years)) to_add[[yr]] <- 0L
      summary_table <- dplyr::bind_rows(summary_table, to_add)
    }
    
    # Step 9: Order by fixed list
    summary_table <- summary_table %>%
      arrange(match(province_kh, province_order))
    
    # Step 10: Return
    if (nrow(summary_table) == 0) {
      showNotification("No data available for the selected year and province.", type = "error")
      return(NULL)
    }
    
    as.data.frame(summary_table, stringsAsFactors = FALSE)
  })
  
  
  ##############################################################################
  
  fishing_products_fishery_domain <- reactive({
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
        province = as.character(province),
        date = suppressWarnings(lubridate::as_date(date)),  # ensure Date format
        year = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else {
          suppressWarnings(lubridate::year(date))
        },
        catch = suppressWarnings(as.integer(round(catch)))
      )
    
    # Step 2: Filter for Marine Capture AND keep only 3 provinces
    df_marine <- df %>%
      filter(nat_fishcatch_area_nat == "1") %>%
      filter(nat_fishcatch_type_fishing == "2") %>%
      filter(province_kh %in% province_order)   # ✅ only 3 provinces
    
    # Step 3: Year filter
    if (input$year != "All") {
      N <- as.numeric(input$year)
      target_years <- c(N - 1, N)
      df_marine <- df_marine %>% filter(year %in% target_years)
    } else {
      target_years <- sort(unique(df_marine$year))
    }
    
    # Step 4: Province filter (respect user selection)
    if (input$province != "All") {
      df_marine <- df_marine %>% filter(province_kh == input$province)
    }
    
    # Step 5: Clean
    df_cleaned <- df_marine %>%
      mutate(catch = as.integer(round(catch))) %>%
      select(province_kh, catch, year) %>%
      filter(!is.na(year))
    
    # Step 6: Summarise
    summary_table <- df_cleaned %>%
      group_by(province_kh, year) %>%
      summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = total_catch,
        values_fill = 0
      )
    
    # Step 7: Ensure year columns exist
    for (yr in as.character(target_years)) {
      if (!yr %in% colnames(summary_table)) {
        summary_table[[yr]] <- 0L
      }
    }
    
    # Step 8: Add missing provinces (if one of the 3 has no data)
    missing_provs <- setdiff(province_order, summary_table$province_kh)
    if (length(missing_provs) > 0) {
      to_add <- tibble::tibble(province_kh = missing_provs)
      for (yr in as.character(target_years)) to_add[[yr]] <- 0L
      summary_table <- dplyr::bind_rows(summary_table, to_add)
    }
    
    # Step 9: Order by fixed list
    summary_table <- summary_table %>%
      arrange(match(province_kh, province_order))
    
    # Step 10: Return
    if (nrow(summary_table) == 0) {
      showNotification("No data available for the selected year and province.", type = "error")
      return(NULL)
    }
    
    as.data.frame(summary_table, stringsAsFactors = FALSE)
  })

  #############################################################################
  aquaculture_summary <- reactive({
    req(aqu_raw)
    
    target_fish_kh <- c(
      "ត្រីទឹកសាប", "ត្រីផ្ទក់",
      "អន្ទង់", "កង្កែប", "កន្ធាយ", "បង្កង់", "បង្គា",
      "ក្តាមថ្ម", "ត្រីគ្រុំ", "ត្រីសមុទ្រ", "គ្រែងឈាម", "ក្រពើ", 
      "សរុប"
    )
    
    # --- Clean data ---
    base_data <- aqu_raw %>%
      mutate(
        province_kh = trimws(province_kh),
        year = suppressWarnings(as.integer(Year)),
        area_num = as.numeric(aqu_fishcatch_area_aqu),
        catch_val = as.numeric(aqu_fishcatch_aqu_catch)
      )
    
    # --- Marine & inland summary ---
    special_summary <- base_data %>%
      group_by(province_kh, year) %>%
      summarise(
        `ត្រីទឹកសាប` = sum(ifelse(area_num == 1, catch_val, 0), na.rm = TRUE),
        `ត្រីសមុទ្រ` = sum(ifelse(area_num == 2, catch_val, 0), na.rm = TRUE),
        .groups = "drop"
      )
    
    # --- Other fish species ---
    other_fish <- base_data %>%
      filter(
        aqu_fishcatch_aqu_fishtype %in% target_fish_kh,
        !aqu_fishcatch_aqu_fishtype %in% c("ត្រីទឹកសាប", "ត្រីសមុទ្រ"),
        !is.na(catch_val), catch_val > 0
      ) %>%
      group_by(province_kh, year, aqu_fishcatch_aqu_fishtype) %>%
      summarise(catch = sum(catch_val, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = aqu_fishcatch_aqu_fishtype,
        values_from = catch,
        values_fill = 0
      )
    
    # --- Merge summaries ---
    summary_table <- special_summary %>%
      left_join(other_fish, by = c("province_kh", "year")) %>%
      mutate(across(-c(province_kh, year), ~ replace_na(.x, 0)))
    
    # --- Guarantee all fish columns exist ---
    missing_cols <- setdiff(target_fish_kh, names(summary_table))
    for (col in missing_cols) {
      summary_table[[col]] <- 0
    }
    
    # --- Filtering and final summary ---
    
    # Start with the full, prepared data
    final_data <- summary_table
    
    # Filter by year if a specific year is selected
    if (!is.null(input$year) && input$year != "All") {
      final_data <- final_data %>% filter(year == as.integer(input$year))
    }
    
    # Filter by province if a specific province is selected
    if (!is.null(input$province) && input$province != "All") {
      final_data <- final_data %>% filter(province_kh == input$province)
    }
    
    # Now, regardless of the filter, summarize to the desired output format
    final_data <- final_data %>%
      group_by(province_kh) %>%
      summarise(across(c(all_of(setdiff(target_fish_kh, "សរុប")), year), sum, na.rm = TRUE), .groups = "drop")
    
    # Remove the 'year' column if the year input is "All" or if a single year was filtered
    if (is.null(input$year) || input$year == "All" || !is.null(input$year) && input$year != "All") {
      final_data <- final_data %>% select(-year)
    }
    
    # Recalculate 'សរុប' (Total) after filtering and summarizing
    final_data <- final_data %>%
      mutate(`សរុប` = rowSums(across(all_of(setdiff(target_fish_kh, "សរុប"))), na.rm = TRUE)) %>%
      rename(`ខេត្ត` = province_kh) %>%
      mutate(across(all_of(target_fish_kh), ~ as.integer(round(as.numeric(.x))))) %>%
      select(`ខេត្ត`, all_of(target_fish_kh)) %>%
      arrange(desc(`សរុប`))
    
    as.data.frame(final_data, stringsAsFactors = FALSE)
  })
  ##############################################################################
  
  aquaculture_summary_detail <- reactive({
    req(aqu_raw)
    
    # --- Clean data ---
    base_data <- aqu_raw %>%
      mutate(
        province_kh = trimws(province_kh),
        year = suppressWarnings(as.integer(Year)),
        area_num = as.numeric(aqu_fishcatch_area_aqu),
        catch_val = as.numeric(aqu_fishcatch_aqu_catch)
      )
    
    # --- Marine & inland summary ---
    special_summary <- base_data %>%
      group_by(province_kh, year) %>%
      summarise(
        `ត្រីទឹកសាប` = sum(ifelse(area_num == 1, catch_val, 0), na.rm = TRUE),
        `ត្រីសមុទ្រ` = sum(ifelse(area_num == 2, catch_val, 0), na.rm = TRUE),
        .groups = "drop"
      )
    
    # --- Other fish species ---
    other_fish <- base_data %>%
      filter(
        aqu_fishcatch_aqu_fishtype %in% target_fish_kh,
        !aqu_fishcatch_aqu_fishtype %in% c("ត្រីទឹកសាប", "ត្រីសមុទ្រ"),
        !is.na(catch_val), catch_val > 0
      ) %>%
      group_by(province_kh, year, aqu_fishcatch_aqu_fishtype) %>%
      summarise(catch = sum(catch_val, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = aqu_fishcatch_aqu_fishtype,
        values_from = catch,
        values_fill = 0
      )
    
    # --- Merge summaries ---
    summary_table <- special_summary %>%
      left_join(other_fish, by = c("province_kh", "year")) %>%
      mutate(across(-c(province_kh, year), ~ replace_na(.x, 0)))
    
    # --- Guarantee all fish columns exist ---
    missing_cols <- setdiff(target_fish_kh, names(summary_table))
    for (col in missing_cols) {
      summary_table[[col]] <- 0
    }
    
    # --- Filtering and final summary ---
    
    # Start with the full, prepared data
    final_data <- summary_table
    
    # Filter by year if a specific year is selected
    if (!is.null(input$year) && input$year != "All") {
      final_data <- final_data %>% filter(year == as.integer(input$year))
    }
    
    # Filter by province if a specific province is selected
    if (!is.null(input$province) && input$province != "All") {
      final_data <- final_data %>% filter(province_kh == input$province)
    }
    
    # Now, regardless of the filter, summarize to the desired output format
    final_data <- final_data %>%
      group_by(province_kh) %>%
      summarise(across(c(all_of(setdiff(target_fish_kh, "សរុប")), year), sum, na.rm = TRUE), .groups = "drop")
    
    # Remove the 'year' column if the year input is "All" or if a single year was filtered
    if (is.null(input$year) || input$year == "All" || !is.null(input$year) && input$year != "All") {
      final_data <- final_data %>% select(-year)
    }
    
    # Recalculate 'សរុប' (Total) after filtering and summarizing
    final_data <- final_data %>%
      mutate(`សរុប` = rowSums(across(all_of(setdiff(target_fish_kh, "សរុប"))), na.rm = TRUE)) %>%
      rename(`ខេត្ត` = province_kh) %>%
      mutate(across(all_of(target_fish_kh), ~ as.integer(round(as.numeric(.x))))) %>%
      select(`ខេត្ត`, all_of(target_fish_kh)) %>%
      arrange(desc(`សរុប`))
    
    as.data.frame(final_data, stringsAsFactors = FALSE)
  })
  

  
  ##############################################################################
  law_enforcement <- reactive({
    req(input$year, input$province)
    
    # Labels for the 3 enforcement codes we keep
    label_map <- c(
      "2" = "បំផ្លាញចោល (ករណី)",
      "3" = "បញ្ជូនសំណុំរឿងទៅតុលាការ (ករណី)",
      "6" = "ពិន័យអន្តរការណ៍ (ករណី)"
    )
    keep_codes <- names(label_map)
    
    # --- Base df with safe parsing ---
    df <- patrol_raw %>%
      mutate(
        province_kh = as.character(province_kh),
        date = suppressWarnings(as.Date(as.character(date)))
      )
    
    # Year from 'year' / 'Year' / date
    if ("year" %in% names(df)) {
      df <- df %>% mutate(year = suppressWarnings(as.integer(.data$year)))
    } else if ("Year" %in% names(df)) {
      df <- df %>% mutate(year = suppressWarnings(as.integer(.data$Year)))
    } else {
      df <- df %>% mutate(year = suppressWarnings(lubridate::year(date)))
    }
    
    # --- Expand enforcement codes (handles "1 2", "2,3", etc.) ---
    df_codes <- df %>%
      mutate(code_raw = as.character(patrol_enforcement)) %>%
      tidyr::separate_rows(code_raw, sep = "\\D+") %>%  # split on any non-digit
      filter(code_raw != "") %>%
      mutate(code_chr = code_raw) %>%
      filter(code_chr %in% keep_codes) %>%
      mutate(patrol_label_kh = dplyr::recode(code_chr, !!!label_map))
    
    # --- Apply filters ---
    if (input$year != "All") {
      df_codes <- df_codes %>% filter(year == as.integer(input$year))
    }
    if (input$province != "All") {
      df_codes <- df_codes %>% filter(province_kh == input$province)
    }
    
    # --- Summarise counts by province x label ---
    summary_table <- df_codes %>%
      group_by(province_kh, patrol_label_kh) %>%
      summarise(total = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = patrol_label_kh,
        values_from = total,
        values_fill = 0L
      )
    
    # ✅ Correct province order: Khmer values (not names) from province_choices
    province_order <- unname(province_choices[province_choices != "All"])
    
    # Add any missing provinces with zeros
    missing_provs <- setdiff(province_order, summary_table$province_kh)
    if (length(missing_provs) > 0) {
      to_add <- tibble::tibble(province_kh = missing_provs)
      for (lbl in unname(label_map)) to_add[[lbl]] <- 0L
      summary_table <- dplyr::bind_rows(summary_table, to_add)
    }
    
    # Ensure all three enforcement columns exist
    for (lbl in unname(label_map)) {
      if (!lbl %in% names(summary_table)) summary_table[[lbl]] <- 0L
    }
    
    # Order by Khmer province list
    summary_table <- summary_table %>%
      arrange(match(province_kh, province_order))
    
    if (nrow(summary_table) == 0) {
      showNotification("No enforcement data available for the selected filters.", type = "error")
      return(NULL)
    }
    
    as.data.frame(summary_table, stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  
  ##############################################################################
  aquaculture_pond <- reactive({
    # Khmer province order
    province_order <- c(
      "បន្ទាយមានជ័យ","បាត់ដំបង","កំពង់ចាម","កំពង់ឆ្នាំង","កំពង់ស្ពឺ",
      "កំពង់ធំ","កំពត","កណ្ដាល","កោះកុង","ក្រចេះ",
      "មណ្ឌលគិរី","ភ្នំពេញ","ព្រះវិហារ","ព្រៃវែង","ពោធិ៍សាត់",
      "រតនគិរី","សៀមរាប","ព្រះសីហនុ","ស្ទឹងត្រែង","ស្វាយរៀង",
      "តាកែវ","ឧត្ដរមានជ័យ","កែប","ប៉ៃលិន","ត្បូងឃ្មុំ"
    )
    
    # detect the aquaculture type column robustly
    type_candidates <- c("aqu_fishcatch_aqu_type","aqu_type","aquaculture_type","type","aqu_type_raw")
    type_col <- intersect(type_candidates, names(aqu_raw))[1]
    
    # if no usable type column, return zeros
    if (is.na(type_col) || length(type_col) == 0) {
      zero_tbl <- tibble(
        province_kh     = province_order,
        `plastic soung` = 0L, ponds = 0L, batches = 0L
      )
      return(as.data.frame(zero_tbl, stringsAsFactors = FALSE, check.names = FALSE))
    }
    
    # normalize multi-select -> kept categories
    normalize_types <- function(x) {
      if (is.null(x) || length(x) == 0) return(character(0))
      txt <- paste(as.character(x), collapse = " ")
      if (!nzchar(txt)) return(character(0))
      txt  <- tolower(txt)
      txt  <- gsub("[_\\-]+", " ", txt)
      txt  <- gsub("\\s+", " ", txt)
      toks <- unique(strsplit(trimws(txt), " ", fixed = TRUE)[[1]])
      
      kept <- character(0)
      if (any(toks %in% c("pond","ponds")))    kept <- c(kept, "ponds")
      if (any(toks %in% c("batch","batches"))) kept <- c(kept, "batches")
      if (any(toks %in% c("plastic","soung","plastic soung"))) kept <- c(kept, "plastic soung")
      
      unique(kept)
    }
    
    df <- aqu_raw %>%
      mutate(
        province    = as.character(province),
        province_kh = dplyr::coalesce(province_kh, province),
        year = if ("year" %in% names(.)) {
          suppressWarnings(as.integer(.data$year))
        } else if ("date" %in% names(.)) {
          suppressWarnings(lubridate::year(date))
        } else {
          NA_integer_
        },
        type_text = sapply(.data[[type_col]], function(v) paste(as.character(v), collapse = " "))
      ) %>%
      mutate(type_list = lapply(type_text, normalize_types)) %>%
      tidyr::unnest_longer(type_list, values_to = "type") %>%
      filter(!is.na(type)) %>%
      mutate(count = 1L)
    
    # ---- Dynamic filtering ----
    if (input$year != "All") {
      df <- df %>% filter(year == as.integer(input$year))
    }
    
    if (input$province != "All") {
      df <- df %>% filter(province_kh == input$province)
    } else {
      df <- df %>% filter(province_kh %in% province_order)  # keep only known provinces
    }
    # ---------------------------
    
    # Province x Type counts
    summary_table <- df %>%
      group_by(province_kh, type) %>%
      summarise(total = sum(count), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = type,
        values_from = total,
        values_fill = 0L
      )
    
    # If pivot produced no province column (no rows), start from empty
    if (!"province_kh" %in% names(summary_table)) {
      summary_table <- tibble(province_kh = character())
    }
    
    # Ensure all 3 type columns exist
    for (nm in c("plastic soung","ponds","batches")) {
      if (!nm %in% names(summary_table)) summary_table[[nm]] <- 0L
    }
    
    # Add missing provinces as zero rows (so selection "All" still shows all provinces)
    if (input$province == "All") {
      missing_provs <- setdiff(province_order, summary_table$province_kh)
      if (length(missing_provs) > 0) {
        to_add <- tibble(province_kh = missing_provs,
                         `plastic soung` = 0L, ponds = 0L, batches = 0L)
        summary_table <- dplyr::bind_rows(summary_table, to_add)
      }
    }
    
    # Order rows & columns
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
      marine_df_detail  <- as.data.frame(filtered_data(),        stringsAsFactors = FALSE)
      marine_df  <- as.data.frame(filtered_data_detail(),        stringsAsFactors = FALSE)
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
      template_path <- "D:/DCX/FIMS/FIMS_ANNUAL_REPORT/Report/Fish_Annual_Tamplate.xlsx"
      
      # Use Sys.Date() to get the current date, which is a common practice for file names.
      # We then format it to a nice string like "2023-10-27".
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      # Create the output file path using the formatted date.
      out_file <- sprintf("D:/DCX/FIMS/FIMS_ANNUAL_REPORT/Report/Fish_Catch_Report_%s.xlsx", current_date)
      
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
  
  
  
  
}