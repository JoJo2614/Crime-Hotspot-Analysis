# ============================================================
# Script 02: Exploratory Data Analysis (EDA)
# Crime Hotspot Analysis – India District-Level IPC Data
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(forcats)
library(scales)
library(patchwork)
library(stringr)

cat("=== Crime Hotspot Analysis: EDA ===\n\n")

# ── 0. Setup ──────────────────────────────────────────────────

theme_crime <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, colour = "#1a1a2e"),
    plot.subtitle    = element_text(colour = "#555555", size = 11),
    axis.text        = element_text(colour = "#333333"),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    legend.position  = "bottom"
  )

PALETTE_CRIME  <- c("#e63946","#f4a261","#2a9d8f","#264653","#e9c46a","#a8dadc")
dir.create("output/maps", showWarnings = FALSE, recursive = TRUE)

# ── 1. Load data ──────────────────────────────────────────────

df <- read_csv("data/processed/crime_cleaned.csv", show_col_types = FALSE)
cat(sprintf("Loaded: %d rows × %d cols\n\n", nrow(df), ncol(df)))

# ── 2. National crime trend 2001-2014 ─────────────────────────

trend <- df %>%
  group_by(year) %>%
  summarise(
    total       = sum(total_ipc_crimes,    na.rm = TRUE),
    violent     = sum(violent_crimes_total, na.rm = TRUE),
    property    = sum(property_crimes_total, na.rm = TRUE),
    women       = sum(women_crimes_total,   na.rm = TRUE),
    .groups = "drop"
  )

p_trend <- trend %>%
  pivot_longer(-year, names_to = "type", values_to = "count") %>%
  mutate(type = factor(type,
    levels = c("total","violent","property","women"),
    labels = c("All IPC Crimes","Violent Crimes","Property Crimes","Crimes Against Women"))) %>%
  ggplot(aes(year, count, colour = type, group = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = label_comma()) +
  scale_colour_manual(values = PALETTE_CRIME) +
  scale_x_continuous(breaks = 2001:2014) +
  labs(title    = "National Crime Trends (2001–2014)",
       subtitle = "Annual totals across all Indian districts",
       x = "Year", y = "Number of Cases", colour = NULL) +
  theme_crime +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/maps/01_national_trend.png", p_trend, width = 10, height = 5, dpi = 150)
cat("✓ Saved: 01_national_trend.png\n")

# ── 3. Top 15 states by average total crimes ─────────────────

state_avg <- df %>%
  group_by(state, year) %>%
  summarise(total = sum(total_ipc_crimes, na.rm = TRUE), .groups = "drop") %>%
  group_by(state) %>%
  summarise(avg_total = mean(total), .groups = "drop") %>%
  slice_max(avg_total, n = 15)

p_states <- state_avg %>%
  mutate(state = fct_reorder(state, avg_total)) %>%
  ggplot(aes(avg_total, state, fill = avg_total)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(round(avg_total))),
            hjust = -0.05, size = 3.2, colour = "#333333") +
  scale_fill_gradient(low = "#f4a261", high = "#e63946") +
  scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Top 15 States by Average Annual IPC Crimes",
       subtitle = "Average across 2001–2014",
       x = "Average Annual Cases", y = NULL) +
  theme_crime

ggsave("output/maps/02_top_states.png", p_states, width = 10, height = 7, dpi = 150)
cat("✓ Saved: 02_top_states.png\n")

# ── 4. Crime type breakdown (latest year = 2014) ─────────────

crime_cols <- c("murder","rape","kidnapping_abduction","dacoity","robbery",
                "burglary","theft","auto_theft","riots","cheating",
                "dowry_deaths","assault_on_women_with_intent_to_outrage_her_modesty",
                "cruelty_by_husband_or_his_relatives","arson",
                "criminal_breach_of_trust")

latest <- df %>% filter(year == max(year))

breakdown <- latest %>%
  summarise(across(all_of(crime_cols), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "crime", values_to = "count") %>%
  mutate(
    crime = str_replace_all(crime, "_", " "),
    crime = str_to_title(crime),
    crime = str_wrap(crime, 30),
    crime = fct_reorder(crime, count)
  )

p_breakdown <- breakdown %>%
  ggplot(aes(count, crime, fill = count)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#a8dadc", high = "#e63946") +
  scale_x_continuous(labels = label_comma()) +
  labs(title    = "Crime Category Breakdown (2014)",
       subtitle = "Total reported cases by crime type across all districts",
       x = "Total Cases", y = NULL) +
  theme_crime

ggsave("output/maps/03_crime_breakdown_2014.png", p_breakdown, width = 10, height = 8, dpi = 150)
cat("✓ Saved: 03_crime_breakdown_2014.png\n")

# ── 5. Top 20 districts (all years combined) ─────────────────

top_districts <- df %>%
  group_by(state, district) %>%
  summarise(total_crimes = sum(total_ipc_crimes, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total_crimes, n = 20) %>%
  mutate(label = paste0(district, "\n(", state, ")"),
         label = fct_reorder(label, total_crimes))

p_districts <- top_districts %>%
  ggplot(aes(total_crimes, label, fill = total_crimes)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#f4a261", high = "#e63946") +
  scale_x_continuous(labels = label_comma()) +
  labs(title    = "Top 20 Crime Districts (2001–2014 Cumulative)",
       subtitle = "Summed IPC cases across all available years",
       x = "Total Cases", y = NULL) +
  theme_crime +
  theme(axis.text.y = element_text(size = 8))

ggsave("output/maps/04_top_districts.png", p_districts, width = 10, height = 9, dpi = 150)
cat("✓ Saved: 04_top_districts.png\n")

# ── 6. Women-specific crimes trend ───────────────────────────

women_trend <- df %>%
  group_by(year) %>%
  summarise(
    Rape                = sum(rape,                na.rm = TRUE),
    `Dowry Deaths`      = sum(dowry_deaths,        na.rm = TRUE),
    `Cruelty by Husband`= sum(cruelty_by_husband_or_his_relatives, na.rm = TRUE),
    `Assault on Women`  = sum(assault_on_women_with_intent_to_outrage_her_modesty, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "crime", values_to = "count")

p_women <- women_trend %>%
  ggplot(aes(year, count, colour = crime, group = crime)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_comma()) +
  scale_colour_manual(values = PALETTE_CRIME) +
  scale_x_continuous(breaks = 2001:2014) +
  labs(title    = "Crimes Against Women – National Trend (2001–2014)",
       x = "Year", y = "Cases", colour = NULL) +
  theme_crime +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/maps/05_women_crimes_trend.png", p_women, width = 10, height = 5, dpi = 150)
cat("✓ Saved: 05_women_crimes_trend.png\n")

# ── 7. Correlation heatmap of crime categories ───────────────

cor_cols <- c("murder","rape","kidnapping_abduction","dacoity","robbery",
              "burglary","theft","riots","cheating","dowry_deaths",
              "cruelty_by_husband_or_his_relatives","causing_death_by_negligence")

cor_mat <- df %>%
  select(all_of(intersect(cor_cols, names(df)))) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr") %>%
  mutate(across(c(var1, var2), ~ str_to_title(str_replace_all(., "_", " "))))

p_corr <- cor_mat %>%
  ggplot(aes(var1, var2, fill = corr)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(corr, 2)), size = 2.5, colour = "white") +
  scale_fill_gradient2(low = "#264653", mid = "white", high = "#e63946",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Crime Category Correlations",
       subtitle = "Pearson correlation across all district-years",
       x = NULL, y = NULL, fill = "r") +
  theme_crime +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))

ggsave("output/maps/06_correlation_heatmap.png", p_corr, width = 9, height = 8, dpi = 150)
cat("✓ Saved: 06_correlation_heatmap.png\n")

# ── 8. Summary statistics ─────────────────────────────────────

cat("\n--- Summary Statistics (all years) ---\n")
df %>%
  select(murder, rape, theft, robbery, total_ipc_crimes) %>%
  summary() %>%
  print()

cat("\n✓ EDA complete – all charts saved to output/maps/\n")