library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(glue)
#gemini 
library(gemini.R)
setAPI(" ")

#manage file
setwd("C:/Users/dhihr/Documents/automate/attack_healthcare")


# List files starting with "DashboardReport_"
xlsx_files <- list.files(
  pattern = "^DashboardReport_.*\\.xlsx$",
  full.names = TRUE
)

# Read the first sheet of each file
data_list <- lapply(xlsx_files, function(file) {
  message("Reading: ", basename(file))
  read_excel(file, sheet = 1)
})

# Combine all into one dataframe (optional)
combined_data <- dplyr::bind_rows(data_list) %>% filter (`Country / Territory` == 'Myanmar') %>%
  mutate(`Attack Date` = as.Date(`Attack Date`)) 


#read from database
url <- "https://raw.githubusercontent.com/Dhihram/WHO_SEAR/refs/heads/main/Dat/attack_healthcare_dat.csv"
df <- read_csv(url, col_names = TRUE) %>% filter(`Country / Territory` %in% c('Myanmar', 'Thailand')) %>%
  mutate(`Attack Date` = as.Date(`Attack Date`, format = "%m/%d/%Y")) %>% filter(`Attack Date` < as.Date('2025-01-01'))

dat_full <- rbind(combined_data,df)
dat_full$`Attack Date` <- format(dat_full$`Attack Date`, "%m/%d/%Y")

#save database
file.remove(list.files(pattern = "\\.xlsx$"))
date_str <- format(Sys.time(), "%d_%m-%y_%H%M")
log_path <- paste0("C:/Users/dhihr/Documents/automate/attack_healthcare/attack_healthcare_dat", date_str, ".csv")
write_csv(dat_full, log_path)

#save Github
setwd("~/GitHub/WHO_SEAR/Dat")
write.csv(dat_full, "attack_healthcare_dat.csv", row.names = FALSE)


#### TEXT for Bulletin
dat_full$`Attack Date` <- as.Date(dat_full$`Attack Date`, "%m/%d/%Y")
dat2 <- dat_full %>%
  filter(`Attack Date` >= as.Date("2025-01-01"))
total_incidence <- as.numeric(nrow(dat2))
total_impacting_health_personel <- dat2 %>% filter(`HC Personnel` == 'YES') %>% nrow() %>% as.numeric()
total_impacting_patient <- dat2 %>% filter(`HC Patients` == 'YES') %>% nrow() %>% as.numeric()
total_injuries <- sum(dat2$`Total Injured`)
total_death <- sum(dat2$`Total Death`)
date_extracted <- as.Date(Sys.Date())
start_date <- as.Date(paste0(format(date_extracted, "%Y"), "-01-01"))

#date format
fmt_day_mon_yr <- function(x) {
  d <- sub("^0", "", format(x, "%d"))  # drop leading zero on Windows
  paste(d, format(x, "%B %Y"))
}

#prompt
gemini_prompt <- glue(
  "Paraphrase the facts below into ONE bullet in clear, neutral English.
- Start the line with '•'.
- Use exactly this timeframe phrasing: 'Since the beginning of {format(start_date, '%Y')} up to {fmt_day_mon_yr(date_extracted)}'.
- Keep the organization wording exactly as: “WHO’s Surveillance System for Attacks on Health Care (SSA)”.
- Keep the numbers unchanged.
- Combine into a single sentence.
- End with 'injuries' and 'deaths' counts.

Facts:
- Incidents total: {total_incidence}
- Incidents directly impacting health personnel: {total_impacting_health_personel}
- Incidents affecting patients: {total_impacting_patient}
- Total injuries: {total_injuries}
- Total deaths: {total_death}

Output only the bullet, nothing else."
)

# 2) Call Gemini
paraphrased <- gemini(gemini_prompt)

# Define output path (adjust as needed)
out_path <- "C:/Users/dhihr/Documents/automate/attack_healthcare/attack_healthcare_summary.txt"

# Write to text file
writeLines(paraphrased, out_path)
