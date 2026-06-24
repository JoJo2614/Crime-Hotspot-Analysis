# ============================================================
# Script 01: Data Cleaning & Harmonisation
# Crime Hotspot Analysis – India District-Level IPC Data
# Covers 2001-2014 across 3 raw CSV files
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(janitor)

cat("=== Crime Hotspot Analysis: Data Cleaning ===\n\n")

# ── 1. Load raw files ───────────────────────────────────────

raw_dir <- "data/raw"

f1 <- read_csv(file.path(raw_dir, "dstrIPC_2001_2012.csv"), show_col_types = FALSE)
f2 <- read_csv(file.path(raw_dir, "dstrIPC_2013.csv"),      show_col_types = FALSE)
f3 <- read_csv(file.path(raw_dir, "dstrIPC_2014.csv"),      show_col_types = FALSE)

cat(sprintf("File 1 (2001-2012): %d rows × %d cols\n", nrow(f1), ncol(f1)))
cat(sprintf("File 2 (2013):      %d rows × %d cols\n", nrow(f2), ncol(f2)))
cat(sprintf("File 3 (2014):      %d rows × %d cols\n\n", nrow(f3), ncol(f3)))

# ── 2. Harmonise File 1 & 2 (same 33-column schema) ─────────

harmonise_old <- function(df) {
  df %>%
    rename(
      state    = `STATE/UT`,
      district = DISTRICT,
      year     = YEAR
    ) %>%
    clean_names() %>%
    mutate(
      state    = str_to_title(str_trim(state)),
      district = str_to_title(str_trim(district))
    )
}

d1 <- harmonise_old(f1)
d2 <- harmonise_old(f2)

# ── 3. Harmonise File 3 (2014 – wider 91-column schema) ──────

d3 <- f3 %>%
  rename(
    state    = `States/UTs`,
    district = District,
    year     = Year
  ) %>%
  clean_names() %>%
  mutate(
    state    = str_to_title(str_trim(state)),
    district = str_to_title(str_trim(district))
  ) %>%
  # Keep only columns that exist in d1/d2 by matching core crimes
  select(
    state, district, year,
    murder,
    attempt_to_murder        = attempt_to_commit_murder,
    culpable_homicide_not_amounting_to_murder,
    rape,
    custodial_rape,
    other_rape               = rape_other_than_custodial,
    kidnapping_abduction     = kidnapping_abduction_total,
    dacoity,
    preparation_and_assembly_for_dacoity = making_preparation_and_assembly_for_committing_dacoity,
    robbery,
    burglary                 = criminal_trespass_burglary,
    theft,
    auto_theft,
    other_theft              = other_thefts,
    riots,
    criminal_breach_of_trust,
    cheating,
    counterfieting           = counterfeiting,
    arson,
    hurt_grevious_hurt       = grievous_hurt,
    dowry_deaths,
    assault_on_women_with_intent_to_outrage_her_modesty,
    insult_to_modesty_of_women,
    cruelty_by_husband_or_his_relatives,
    importation_of_girls_from_foreign_countries = importation_of_girls_from_foreign_country,
    causing_death_by_negligence,
    other_ipc_crimes,
    total_ipc_crimes         = total_cognizable_ipc_crimes
  )

# ── 4. Bind all years ────────────────────────────────────────

# Ensure d1 & d2 have matching column names
common_cols <- intersect(intersect(names(d1), names(d2)), names(d3))

crime_df <- bind_rows(
  d1 %>% select(all_of(common_cols)),
  d2 %>% select(all_of(common_cols)),
  d3 %>% select(all_of(common_cols))
)

cat(sprintf("Combined dataset: %d rows × %d cols\n", nrow(crime_df), ncol(crime_df)))
cat(sprintf("Years covered: %s\n\n", paste(sort(unique(crime_df$year)), collapse = ", ")))

# ── 5. Remove state-total rows (ZZ TOTAL / Total) ────────────

crime_df <- crime_df %>%
  filter(!str_detect(district, regex("ZZ TOTAL|^TOTAL$|^Total$", ignore_case = TRUE)))

cat(sprintf("After removing state totals: %d district-year rows\n", nrow(crime_df)))

# ── 6. Remove known duplicate / aggregate districts ──────────

crime_df <- crime_df %>%
  filter(!str_detect(district, regex("^D\\.T\\.|^Dt\\.", ignore_case = TRUE)))

# ── 7. Fix state name inconsistencies ────────────────────────

state_fix <- c(
  "A&N Islands"         = "Andaman & Nicobar Islands",
  "A & N Islands"       = "Andaman & Nicobar Islands",
  "Daman & Diu"         = "Daman & Diu",
  "D & N Haveli"        = "Dadra & Nagar Haveli",
  "Dadra & Nagar Haveli And Daman & Diu" = "Dadra & Nagar Haveli",
  "Delhi Ut"            = "Delhi",
  "Delhi"               = "Delhi",
  "Jammu & Kashmir"     = "Jammu & Kashmir",
  "J&K"                 = "Jammu & Kashmir"
)

crime_df <- crime_df %>%
  mutate(state = recode(state, !!!state_fix))

# ── 8. Add derived columns ────────────────────────────────────

violent_crimes <- c("murder", "attempt_to_murder",
                    "culpable_homicide_not_amounting_to_murder",
                    "rape", "kidnapping_abduction",
                    "dacoity", "robbery", "riots",
                    "hurt_grevious_hurt", "dowry_deaths",
                    "assault_on_women_with_intent_to_outrage_her_modesty",
                    "cruelty_by_husband_or_his_relatives")

property_crimes <- c("burglary", "theft", "auto_theft",
                     "other_theft", "arson",
                     "criminal_breach_of_trust", "cheating",
                     "counterfieting")

crime_df <- crime_df %>%
  mutate(
    violent_crimes_total  = rowSums(across(all_of(intersect(violent_crimes,  names(.)))), na.rm = TRUE),
    property_crimes_total = rowSums(across(all_of(intersect(property_crimes, names(.)))), na.rm = TRUE),
    women_crimes_total    = rape + dowry_deaths +
                            assault_on_women_with_intent_to_outrage_her_modesty +
                            cruelty_by_husband_or_his_relatives +
                            insult_to_modesty_of_women,
    crime_rate_per_lakh   = NA_real_   # placeholder – no population column in source
  )

# ── 9. Quality checks ─────────────────────────────────────────

cat("\n--- Quality Checks ---\n")
cat("Missing values per column:\n")
na_summary <- crime_df %>% summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0)

if (nrow(na_summary) == 0) {
  cat("  ✓ No missing values found.\n")
} else {
  print(na_summary)
}

cat(sprintf("\nStates: %d | Districts: %d | Years: %d\n",
            n_distinct(crime_df$state),
            n_distinct(crime_df$district),
            n_distinct(crime_df$year)))

cat("\nTop 5 districts by total IPC crimes (all years):\n")
crime_df %>%
  group_by(state, district) %>%
  summarise(avg_crimes = mean(total_ipc_crimes), .groups = "drop") %>%
  slice_max(avg_crimes, n = 5) %>%
  print()

# ── 10. Save processed data ───────────────────────────────────

out_path <- "data/processed/crime_cleaned.csv"
write_csv(crime_df, out_path)
cat(sprintf("\n✓ Cleaned data saved to %s\n", out_path))
cat(sprintf("  Final shape: %d rows × %d columns\n", nrow(crime_df), ncol(crime_df)))