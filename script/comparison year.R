setwd("C:/Users/dhihr/OneDrive/bahan who/kerjaan who/influenza")

library(tidyverse)
library(dplyr)
library(lubridate)
library(MMWRweek)
library(formattable)
library(tidyr)
library(ggplot2)

data <- read.csv('https://xmart-api-public.who.int/FLUMART/VIW_FNT?$format=csv') %>%
  filter(WHOREGION == 'SEAR')

flu_dat2 <- data %>%
  mutate(
    week_start = ymd(MMWR_WEEKSTARTDATE),
    year = year(week_start),
    week = isoweek(week_start),
    season = ifelse(week >= 30, paste0(year, "-", year + 1), paste0(year - 1, "-", year))
  ) %>%
  group_by(COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE) %>%
  summarise(
    flu_all = sum(INF_ALL, na.rm = TRUE),
    specimen = sum(SPEC_PROCESSED_NB, na.rm = TRUE),
    season = last(season),
    MMWR_WEEK = last(MMWR_WEEK),
    .groups = 'drop'
  ) %>%
  mutate(
    flu_all = replace_na(flu_all, 0),
    specimen = replace_na(specimen, 0),
    pos_rate = ifelse(specimen > 0, round((flu_all / specimen) * 100, 2), 0),
    MMWR_WEEKSTARTDATE = as.Date(MMWR_WEEKSTARTDATE)
  ) 

grouping_flu <- flu_dat2 %>%
  group_by(MMWR_WEEKSTARTDATE) %>%
  summarise(COUNTRY_AREA_TERRITORY = 'All Country', 
            flu_all = sum(flu_all, na.rm = TRUE),
            specimen = sum(specimen, na.rm = TRUE), 
            season = last(season),
            MMWR_WEEK = last(MMWR_WEEK), .groups = "drop") %>% 
  select(COUNTRY_AREA_TERRITORY, everything()) %>%
  mutate(
    flu_all = replace_na(flu_all, 0),
    specimen = replace_na(specimen, 0),
    pos_rate = ifelse(specimen > 0, round((flu_all / specimen) * 100, 2), 0),
    MMWR_WEEKSTARTDATE = as.Date(MMWR_WEEKSTARTDATE)
  ) %>%
  as.data.frame()

flu_dat <- rbind(flu_dat2, grouping_flu)
# to get last submit
date_of_last_submit <- max(flu_dat$MMWR_WEEKSTARTDATE)

# Set fixed max date
#change this
today <- Sys.Date()
first_day_epiweek <- floor_date(today, "week", week_start = 7) # Sunday
max_target_date <- first_day_epiweek-7

# Get the earliest and latest dates per country
date_bounds <- flu_dat %>%
  group_by(COUNTRY_AREA_TERRITORY) %>%
  summarise(
    start_date = min(MMWR_WEEKSTARTDATE, na.rm = TRUE),
    last_data_date = max(MMWR_WEEKSTARTDATE, na.rm = TRUE),
    .groups = "drop"
  )

# Expand weekly dates per country
expanded_dates <- date_bounds %>%
  rowwise() %>%
  mutate(
    week_seq = list(seq(start_date, max_target_date, by = "7 days"))
  ) %>%
  unnest(week_seq) %>%
  rename(MMWR_WEEKSTARTDATE = week_seq)

# Join with original data
flu_dat_expanded <- expanded_dates %>%
  select(COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, last_data_date) %>%
  left_join(flu_dat, by = c("COUNTRY_AREA_TERRITORY", "MMWR_WEEKSTARTDATE"))

# Replace numeric columns: 0 up to last_data_date - NA after last_data_date
flu_dat <- flu_dat_expanded %>%
  mutate(
    after_last = MMWR_WEEKSTARTDATE > last_data_date,
    across(where(is.numeric), ~ifelse(after_last, NA, replace_na(.x, 0)))
  ) %>%
  select(-last_data_date, -after_last) %>%
  arrange(COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE)

flu_dat <- flu_dat %>% filter(MMWR_WEEKSTARTDATE >= as.Date('2019-11-11'))

#select country
clean_dat <- flu_dat %>% mutate(year = year(MMWR_WEEKSTARTDATE))

#trend
# Prepare the data
# Wide

# Order week factor
pivot_long_influenza_a <- clean_dat %>%
  select(COUNTRY_AREA_TERRITORY, MMWR_WEEK, year, flu_all) %>%
  rename(influenza = flu_all) %>%
  complete(
    COUNTRY_AREA_TERRITORY,
    year,
    MMWR_WEEK,
    fill = list(influenza = NA)
  ) %>% 
  mutate(
    influenza = ifelse(year == 2025 & MMWR_WEEK >= 46, NA, influenza),
    year = as.factor(year),
    MMWR_WEEK = as.factor(MMWR_WEEK)
    )

# Helper for axis labels
everyother <- function(x) x[seq_along(x) %% 4 == 0]

# Plot
p <- ggplot(
  pivot_long_influenza_a,
  aes(
    x = MMWR_WEEK,
    y = influenza,
    color = year,
    group = year
  )
) +
  geom_line(aes(size = ifelse(year == "2025", 1.8, 1))) +
  scale_size_identity() +
  scale_x_discrete(
    name = "Week number",
    breaks = everyother(levels(pivot_long_influenza_a$MMWR_WEEK))
  ) +
  scale_color_manual(
    name = "Year",
    values = c(
      "2019" = "#b1b2b3",
      "2020" = "#99bbf2",
      "2021" = "#b2f7f2",
      "2022" = "#dbbdf2",
      "2023" = "#cfd19f",
      "2024" = "#eb9691",
      "2025" = "red"
    )
  ) +
  facet_wrap(~ COUNTRY_AREA_TERRITORY, scales = "free_y") +   # <— add this
  theme_minimal(base_size = 16) +
  labs(
    title = "Influenza Trend by Year — SEARO Region",
    y = "Cases"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 22),
    legend.text  = element_text(size = 22),
    legend.spacing.x = unit(1, "cm"),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 16, face = "bold"),  
    plot.title = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 1))

p

ggsave("country_comp_facet.png", p, width = 26, height = 11, dpi = 300)
