# clean_all_kobo <- function() {
#   source("ConnectKobo2.R")  # or integrate content here
#   list(
#     main       = main_df,
#     nat        = nat_df,
#     aqu        = aqu_df,
#     processing = processing_df,
#     patrol     = patrol_df
#   )
# }


source("ConnectKobo2.R")

# --- Data Preparation (executed once when the app starts) ---
cleaned_data <- clean_all_kobo()
main_df       <- cleaned_data$main
nat_df        <- cleaned_data$nat
aqu_df        <- cleaned_data$aqu
processing_df <- cleaned_data$processing
patrol_df     <- cleaned_data$patrol