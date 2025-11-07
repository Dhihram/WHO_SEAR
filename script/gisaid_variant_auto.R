library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(tidyverse)
library(readr)
#gemini 
library(gemini.R)
setAPI("AIzaSyAFYzJTKe4Hur_nl9eve_fSzXwtrtfpjHw")

setwd("C:/Users/dhihr/Documents/automate")

#delete file
setwd("C:/Users/dhihr/Documents/automate/gisaid_summaries")
unlink(list.files(pattern = "*", full.names = TRUE), recursive = FALSE)
setwd("C:/Users/dhihr/Documents/automate/gisaid_plots")
unlink(list.files(pattern = "*", full.names = TRUE), recursive = FALSE)


#cleaning data
setwd("C:/Users/dhihr/Documents/automate/data")
data <- read_tsv("gisaid.tsv") %>% select(-`AA Substitutions`)
summary(data)

date_select <- as.Date(Sys.Date())
data$`Collection date` <- as.Date(data$`Collection date`)


data <- data %>%
  separate(col = Location, into = c("Continent", "Country", "City"), sep = " / ")
table(data$Lineage)
data <- data %>% filter(Country %in% c('Bangladesh', 'India', 'Nepal', 'Maldives', 
                                       'Sri Lanka', 'Bhutan', 'Myanmar', 'Thailand'))
data$diff <- date_select-data$`Collection date`
data <- data %>% filter(`Collection date`>= (date_select-90))
data <- data %>% filter(data$diff <= 90)
data$diff_cat <- ifelse(data$diff <= 30, 
                        '30 days',
                        ifelse(data$diff <= 60, 
                               '31-60 days','61-90 days'))
data$diff_cat <- factor(data$diff_cat, levels = c('61-90 days', '31-60 days', '30 days'))

data <- data %>%
  mutate(
    lineage_group = case_when(
      str_detect(Lineage, "JN\\.1\\.6|JN\\.1\\.40|JN\\.1\\.16|XFG") ~ "XFG*",
      str_detect(Lineage, "JN\\.1|MV\\.1|PC\\.2\\.1|LF\\.7\\.1|LF\\.7\\.7\\.1|MB\\.1\\.1|LF\\.7|XFC") ~ "JN.1*",
      str_detect(Lineage, "XEC") ~ "XEC*",
      str_detect(Lineage, "XDV|BA.2|NB.1.8.1|PQ") ~ "NB.1.8.1*",
      TRUE ~ "Other"
    ),
    lineage_group = factor(lineage_group, levels = c("JN.1*", "XEC*", "NB.1.8.1*", "XFG*", "Other"))
  )


# === Define common parameters ===
lineage_levels <- c("Other", "XFG*", "NB.1.8.1*", "XEC*", "JN.1*")

lineage_colors <- c(
  "Other" = "#999999",
  "XFG*" = "firebrick",
  "NB.1.8.1*" = "#E69F00",
  "XEC*" = "#56B4E9",
  "JN.1*" = "#009E73"
)

countries <- unique(data$Country)  # list of countries to loop through

# === Set working directory ===
setwd("C:/Users/dhihr/Documents/automate")

# === Define timestamp for filenames ===
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

# === Loop through each country ===
for (country in countries) {
  
  message("Processing ", country, "...")
  
  # --- Subset data for this country ---
  data2 <- data %>%
    filter(Country == country) %>%
    select(`Collection date`, diff_cat, Lineage, lineage_group)
  
  if (nrow(data2) == 0) next
  
  # --- Summarize counts for stacked bar chart ---
  summary_data2 <- data2 %>%
    count(diff_cat, lineage_group)
  
  summary_data2$lineage_group <- factor(summary_data2$lineage_group, levels = lineage_levels)
  
  # === PLOT 1: Count Bar Chart ===
  if (length(unique(summary_data2$lineage_group)) > 1) {
    # ---- Stacked bar ----
    p1 <- ggplot(summary_data2, aes(x = diff_cat, y = n, fill = lineage_group)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n),
                position = position_stack(vjust = 0.5),
                color = "white", size = 10)
  } else {
    # ---- Single bar (no stacking) ----
    p1 <- ggplot(summary_data2, aes(x = diff_cat, y = n, fill = lineage_group)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n, y = n / 2),
                color = "white", size = 10)
  }
  
  # ---- Continue with the rest of your theme, labels, etc. ----
  p1 <- p1 +
    scale_fill_manual(values = lineage_colors) +
    scale_y_continuous(breaks = seq(0, max(summary_data2$n), by = 1)) +
    labs(
      x = " ",
      y = "Number of Sequences",
      fill = "Lineage Group",
      title = country,
      subtitle = paste0("Total Sequences = ", nrow(data2))
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 27),
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 32),
      axis.text.y = element_text(size = 28),
      axis.title.x = element_text(size = 26),
      axis.title.y = element_text(size = 26),
      plot.subtitle = element_text(size = 26)
    )
  
  # --- Summarize total by lineage group ---
  summary_grouped <- summary_data2 %>%
    group_by(lineage_group) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    mutate(percent = round(100 * total / sum(total), 0))
  summary_grouped$country <- country
  
  # === Save summary table with timestamp ===
  write.csv(
    summary_grouped,
    paste0("gisaid_summaries/", country, "_summary_", timestamp, ".csv"),
    row.names = FALSE
  )
  
  # === PLOT 2: Percentage Bar Chart ===
  summary_data <- data2 %>%
    group_by(diff_cat, lineage_group) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(diff_cat) %>%
    mutate(
      prop = n / sum(n),
      label_pos = cumsum(prop) - 0.5 * prop
    ) %>%
    ungroup()
  
  summary_data$lineage_group <- factor(summary_data$lineage_group, levels = lineage_levels)
  
  p2 <- ggplot(summary_data, aes(x = diff_cat, y = prop, fill = lineage_group)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = label_pos, label = percent(prop, accuracy = 1)), color = "white", size = 10) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = lineage_colors) +
    labs(x = " ", y = "Proportion of Sequences", fill = "Lineage Group", title = " ") +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 27),
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 32),    
      axis.text.y = element_text(size = 28),
      axis.title.x = element_text(size = 26),
      axis.title.y = element_text(size = 26),
      plot.subtitle = element_text(size = 26)
    )
  
  p3 <- p1 + p2
  
  # === Save combined plot with timestamp ===
  ggsave(
    filename = paste0("gisaid_plots/", country, "_proportions_", timestamp, ".png"),
    plot = p3,
    width = 12, height = 8, units = "in", dpi = 300
  )
}

#make summary
setwd("C:/Users/dhihr/Documents/automate/gisaid_summaries")

#Generate text for bulletin
# List all CSV files in the folder
files <- list.files(pattern = "\\.csv$", full.names = TRUE)

combined_data <- files %>%
  lapply(read_csv) %>%   # Read all files
  bind_rows() %>%
  group_by(country) %>%
  arrange(country, desc(percent), .by_group = TRUE)


# 1) Total sequences per country
totals <- combined_data %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(total_sequences = sum(total, na.rm = TRUE), .groups = "drop")

# 2) Predominant lineage per country (break ties by higher count, then alphabetically)
tops <- combined_data %>%
  ungroup() %>%
  group_by(country) %>%
  arrange(desc(percent), desc(total), lineage_group, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  select(country,
         top_lineage = lineage_group,
         top_percent = percent,
         top_count = total)

# 3) Join and format bullet lines

# generate date of extraction 
info <- file.info("C:/Users/dhihr/Documents/automate/data/gisaid.tsv")
date_extracted <- as.Date(info$mtime, tz = Sys.timezone())
last_submit <- max(data$`Collection date`)

bullets2 <- sprintf(
  "• The data were extracted on %s, last submission on %s.",
  format(date_extracted, "%d %b %Y"),
  format(last_submit,   "%d %b %Y")
)

bullets_df <- totals %>%
  left_join(tops, by = "country") %>%
  arrange(country) %>%
  mutate(
    bullet = sprintf("• %s submitted %d sequences with %s being predominant at %s%% (%d sequences).",
                     country, total_sequences, top_lineage,
                     ifelse(is.na(top_percent), "NA", format(round(top_percent, 1), nsmall = 1)),
                     top_count)
  )


paraphrase_prompt <- paste(
  "Paraphrase the following bullets in clear, neutral English.",
  "Keep the meaning, numbers, weeks, and dates EXACTLY the same.",
  "Make numeric zero decimal places",
  "Make space as separator of thousand",
  "Return bullets only (no intro/outro), each starting with '•'.",
  "",
  paste(c(bullets_df$bullet, bullets2), collapse = "\n"),
  sep = "\n"
)

# 2) Call Gemini
paraphrased <- gemini(paraphrase_prompt)

# Define output path (adjust as needed)
out_path <- "C:/Users/dhihr/Documents/automate/gisaid_summaries/gisaid_summary.txt"

# Write to text file
writeLines(paraphrased, out_path)

#for dashboard database
setwd("C:/Users/dhihr/Documents/automate/data")
data2 <- read_tsv('gisaid.tsv') %>% select(-`AA Substitutions`)
data2$`Collection date` <- ifelse(data2$`Collection date` == '2025','2025-01-01',data2$`Collection date`)
summary(data2)

data2 <- data2 %>%
  separate(col = Location, into = c("Continent", "Country", "City"), sep = " / ")
table(data2$Lineage)
data2 <- data2 %>% filter(Country %in% c('Bangladesh', 'India', 'Nepal', 'Maldives', 
                                         'Sri Lanka', 'Bhutan', 'Myanmar', 'Thailand'))
data2$`Collection date` <- as.Date(data2$`Collection date`)

data2 <- data2 %>%
  mutate(
    lineage_group = case_when(
      str_detect(Lineage, "JN\\.1\\.6|JN\\.1\\.40|JN\\.1\\.16|XFG") ~ "XFG*",
      str_detect(Lineage, "JN\\.1|MV\\.1|PC\\.2\\.1|LF\\.7\\.1|LF\\.7\\.7\\.1|MB\\.1\\.1|LF\\.7|XFC") ~ "JN.1*",
      str_detect(Lineage, "XEC") ~ "XEC*",
      str_detect(Lineage, "XDV|BA.2|NB.1.8.1|PQ") ~ "NB.1.8.1*",
      TRUE ~ "Other"
    ),
    lineage_group = factor(lineage_group, levels = c("JN.1*", "XEC*", "NB.1.8.1*", "XFG*", "Other"))
  )
data2 <- data2 %>% select(Country, `Collection date`, lineage_group)

# Calculate the last day (Sunday) of the ISO week
data2 <- data2 %>%
  mutate(ISO_week = isoweek(`Collection date`),
         ISO_week_end = `Collection date` + days(7 - wday(`Collection date`, week_start = 1)))

summary_data <- data2 %>%
  group_by(Country, ISO_week, lineage_group, ISO_week_end) %>%
  summarise(n = n(), .groups = "drop") %>%  
  group_by(Country, ISO_week, ISO_week_end) %>%
  mutate(percent = (n / sum(n)) * 100
  ) %>% arrange(Country, ISO_week_end)


group_data <- summary_data %>%
  group_by(ISO_week, lineage_group, ISO_week_end) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ISO_week, ISO_week_end) %>%
  mutate(
    Country = 'All Country',
    percent = (n / sum(n)) * 100
  ) %>%
  select(Country, ISO_week, lineage_group, ISO_week_end, n, percent)

data2 <- rbind(group_data, summary_data)

#save to github
setwd("~/GitHub/WHO_SEAR/Dat")
write.csv(data2, 'covid_variant.csv', row.names = FALSE)

#save file database
# Format filename as dd_mm-yy
date_str <- format(Sys.time(), "%d_%m-%y_%H%M")
log_path <- paste0("C:/Users/dhihr/Documents/automate/covid_variant_", date_str, ".csv")


# Write or append to the dated file
if (!file.exists(log_path)) {
  write_csv(data2, log_path)
} else {
  existing <- read_csv(log_path)
  updated <- bind_rows(existing, flu_dat)
  write_csv(updated, log_path)
}

