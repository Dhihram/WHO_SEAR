# FLUID data

library(tidyverse)
library(dplyr)
library(lubridate)
library(MMWRweek)


data <- read.csv('https://xmart-api-public.who.int/FLUMART/VIW_FID_EPI?$format=csv') %>%
  filter(WHOREGION == 'SEAR') %>% filter(AGEGROUP_CODE == 'ALL') %>% 
  select(COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE,ILI_CASE, ILI_OUTPATIENTS, SARI_CASE, SARI_INPATIENTS)

#alt opening
#setwd("C:/Users/dhihr/OneDrive/bahan who/kerjaan who/influenza")
#data <- read.csv('VIW_FID_EPI.csv') %>%
#filter(WHOREGION == 'SEAR')
data <- data %>%
  mutate(
    ili_1000 = round(ifelse(ILI_OUTPATIENTS > 0, (ILI_CASE / ILI_OUTPATIENTS) * 100, 0), 2),
    sari_100= round(ifelse(SARI_INPATIENTS > 0, (SARI_CASE / SARI_INPATIENTS) * 100, 0), 2),
    across(where(is.numeric), ~replace(., is.infinite(.) | is.na(.), 0)),
    COUNTRY_AREA_TERRITORY = ifelse(COUNTRY_AREA_TERRITORY == "Democratic People's Republic of Korea", "DPR Korea", COUNTRY_AREA_TERRITORY)) 
data$MMWR_WEEKSTARTDATE <- as.Date(data$MMWR_WEEKSTARTDATE)
grouping_data <- data %>%
  group_by(MMWR_WEEKSTARTDATE) %>%
  summarise(
    COUNTRY_AREA_TERRITORY = 'All Country',
    ILI_CASE = sum(ILI_CASE, na.rm = TRUE),
    ILI_OUTPATIENTS = sum(ILI_OUTPATIENTS, na.rm = TRUE),
    SARI_CASE = sum(SARI_CASE, na.rm = TRUE),
    SARI_INPATIENTS = sum(SARI_INPATIENTS, na.rm = TRUE)) %>% 
  mutate(
    ili_1000 = round(ifelse(ILI_OUTPATIENTS > 0, (ILI_CASE / ILI_OUTPATIENTS) * 100, 0), 2),
    sari_100= round(ifelse(SARI_INPATIENTS > 0, (SARI_CASE / SARI_INPATIENTS) * 100, 0), 2),
    across(where(is.numeric), ~replace(., is.infinite(.) | is.na(.), 0)))

fluid_dat_agg <- rbind(data, grouping_data)


fluid_dat_full <- expand.grid(
  COUNTRY_AREA_TERRITORY = unique(fluid_dat_agg$COUNTRY_AREA_TERRITORY),
  MMWR_WEEKSTARTDATE = seq(min(fluid_dat_agg$MMWR_WEEKSTARTDATE), max(fluid_dat_agg$MMWR_WEEKSTARTDATE), by = "week")
) %>%
  left_join(fluid_dat_agg, by = c("COUNTRY_AREA_TERRITORY", "MMWR_WEEKSTARTDATE")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  as.data.frame()

fluid_dat_full <- fluid_dat_full %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% mutate(MMWR_WEEK = MMWRweek(fluid_dat_full$MMWR_WEEKSTARTDATE)$MMWRweek)



setwd("~/GitHub/WHO_SEAR/Dat")
write.csv(fluid_dat_full, "fluid_dat_full.csv", row.names = FALSE)

# Format filename as dd_mm-yy
date_str <- format(Sys.time(), "%d_%m-%y_%H%M")
log_path <- paste0("C:/Users/dhihr/Documents/automate/data_influenza_fluid", date_str, ".csv")

# Write or append to the dated file
if (!file.exists(log_path)) {
  write_csv(fluid_dat_full, log_path)
} else {
  existing <- read_csv(log_path)
  updated <- bind_rows(existing, flu_dat)
  write_csv(updated, log_path)
}


