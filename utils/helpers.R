library(dplyr)
library(tidyr)

# Step 1: Prepare fish type name if needed (optional but recommended)
aqu_df <- aqu_df %>%
  mutate(
    catch = as.numeric(aqu_fishcatch_aqu_catch),
    fish_type = as.character(aqu_fishcatch_aqu_fishtype),
    province = as.character(province)
  ) %>%
  left_join(province_lookup, by = c("province" = "province_code")) %>%
  mutate(province_kh = coalesce(province_kh, province))

# Step 2: Summarize by province and fish type
aqu_summary <- aqu_df %>%
  filter(!is.na(catch), catch > 0) %>%
  group_by(province_kh, fish_type) %>%
  summarise(total = sum(catch, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = fish_type, values_from = total, values_fill = 0)

# Step 3: Add total column (safe ASCII name), then Khmer labels
aqu_summary <- aqu_summary %>%
  mutate(total_sum = rowSums(dplyr::select(., where(is.numeric)))) %>%
  arrange(desc(total_sum)) %>%
  mutate(row_num = row_number()) %>%
  select(row_num, province_kh, everything())

# Step 4: Rename to Khmer for display or export
colnames(aqu_summary)[1:2] <- c("ល.រ", "ខេត្ត/រាជធានី")
colnames(aqu_summary)[ncol(aqu_summary)] <- "សរុប"

# View final table
print(aqu_summary)
