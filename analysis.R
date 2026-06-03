# ============================================================
# Ebola Outbreak Trends — Exploratory Analysis
# Author: [Your Name]
# Date: 2024
# Data Source: WHO Situation Reports
# ============================================================

# ── 1. Load Libraries ──────────────────────────────────────
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(patchwork)

# ── 2. Load Data ───────────────────────────────────────────
df <- read_csv("data/ebola_trends.csv") |>
  mutate(date = as.Date(date))

# ── 3. Overview ────────────────────────────────────────────
glimpse(df)
summary(df)

# ── 4. Total Cases and Deaths by Outbreak ─────────────────
df |>
  group_by(outbreak) |>
  summarise(
    peak_total_cases = max(total_cases, na.rm = TRUE),
    peak_deaths      = max(deaths, na.rm = TRUE),
    avg_cfr          = mean(case_fatality_rate, na.rm = TRUE)
  )

# ── 5. Plot: Cumulative Cases Over Time by Country ─────────
p1 <- df |>
  ggplot(aes(x = date, y = total_cases, colour = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~outbreak, scales = "free_x") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(
    title    = "Cumulative Ebola Cases Over Time",
    subtitle = "By country and outbreak period",
    x        = NULL,
    y        = "Cumulative Total Cases",
    colour   = "Country",
    caption  = "Source: WHO Situation Reports"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title   = element_text(face = "bold")
  )

print(p1)

# ── 6. Plot: Case Fatality Rate Over Time ─────────────────
p2 <- df |>
  ggplot(aes(x = date, y = case_fatality_rate, colour = country)) +
  geom_line(linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~outbreak, scales = "free_x") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(
    title    = "Case Fatality Rate (CFR) Over Time",
    subtitle = "Smoothed trend shown as dashed line",
    x        = NULL,
    y        = "CFR (%)",
    colour   = "Country",
    caption  = "Source: WHO Situation Reports"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title   = element_text(face = "bold")
  )

print(p2)

# ── 7. Plot: Monthly New Cases (West Africa 2014-2016) ─────
west_africa <- df |>
  filter(outbreak == "West Africa 2014-2016") |>
  arrange(country, date) |>
  group_by(country) |>
  mutate(new_cases = total_cases - lag(total_cases, default = 0)) |>
  ungroup()

p3 <- west_africa |>
  ggplot(aes(x = date, y = new_cases, fill = country)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(
    title    = "Monthly New Ebola Cases — West Africa (2014-2016)",
    subtitle = "Guinea, Liberia, Sierra Leone",
    x        = NULL,
    y        = "New Cases per Month",
    fill     = "Country",
    caption  = "Source: WHO Situation Reports"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title   = element_text(face = "bold")
  )

print(p3)

# ── 8. Save Plots ──────────────────────────────────────────
ggsave("figures/cumulative_cases.png",  plot = p1, width = 12, height = 6, dpi = 300)
ggsave("figures/cfr_over_time.png",     plot = p2, width = 12, height = 6, dpi = 300)
ggsave("figures/monthly_new_cases.png", plot = p3, width = 12, height = 6, dpi = 300)

cat("Analysis complete. Figures saved to /figures\n")
