## ----influenza----

data_flu_long <- reactive({
  df <- readRDS(
    url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/flu_long.rds")
  )
  if (is.null(df) || nrow(df) == 0) {
    return(tibble())
  }
  df$MMWR_WEEKSTARTDATE <- as.Date(df$MMWR_WEEKSTARTDATE)
  df <- df %>%  mutate(
    iso_year = isoyear(MMWR_WEEKSTARTDATE + 1),
    iso_week = isoweek(MMWR_WEEKSTARTDATE + 1),
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df
})

data_flu_tab <- reactive({
  df <- readRDS(
    url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/flu_dat_full.rds")
  )
  if (is.null(df) || nrow(df) == 0) {
    return(tibble())
  }
  df$MMWR_WEEKSTARTDATE <- as.Date(df$MMWR_WEEKSTARTDATE)
  df <- df %>%  mutate(
    iso_year = isoyear(MMWR_WEEKSTARTDATE + 1),
    iso_week = isoweek(MMWR_WEEKSTARTDATE + 1),
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df
})

data_flu_id <- reactive({
  df <- readRDS(
    url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/fluid_dat_full.rds")
  )
  if (is.null(df) || nrow(df) == 0) {
    return(tibble())
  }
  df$MMWR_WEEKSTARTDATE <- as.Date(df$MMWR_WEEKSTARTDATE)
  df <- df %>%  mutate(
    iso_year = isoyear(MMWR_WEEKSTARTDATE + 1),
    iso_week = isoweek(MMWR_WEEKSTARTDATE + 1),
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df
})


## COVID-19

data_covid <- reactive({
  df <- readRDS(
    url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/covid_searo.rds")
  )
  if (is.null(df) || nrow(df) == 0) return(tibble())
  df$date <- as.Date(df$date)
  df <- df %>%  mutate(
    iso_year = isoyear(date - 6),
    iso_week = isoweek(date - 6),
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df
})

data_covid_sentinel <- reactive({
  df <- readRDS(url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/covid_sentinel.rds"))
  if (is.null(df) || nrow(df) == 0) {
    return(tibble())
  }
  df$Week.start.date <- as.Date(df$Week.start.date)
  df <- df %>%  mutate(
    iso_year = ISO_year,
    iso_week = ISO_week,
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df <- df %>% select(Country, ISO_week, ISO_year, SARS_COV_2_POS, `%.Pos.Rate`, yearweek, yearweek2)
  df
})

data_covid_var <- reactive({
  df <- readRDS(url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/covid_variant.rds"))
  if (is.null(df) || nrow(df) == 0) return(tibble())
  df$ISO_week_end <- as.Date(df$ISO_week_end)
  df <- df %>%  mutate(
    iso_year = isoyear(ISO_week_end - 6),
    iso_week = isoweek(ISO_week_end - 6),
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df
})

# MPOX DATA
data_mpox <- reactive({
  df <- readRDS(url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/mpox_dat_full.rds"))
  if (is.null(df) || nrow(df) == 0) return(tibble())
  df$date <- as.Date(df$date)
  df <- df %>%  mutate(
    iso_year = isoyear(date),
    iso_week = isoweek(date),
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df
})

data_mpox_clade <- reactive({
  df <- readRDS(url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/mpox_clade.rds"))
  if (is.null(df) || nrow(df) == 0) return(tibble())
  df$date <- as.Date(df$date)
  df <- df %>%  mutate(
    iso_year = isoyear(date),
    iso_week = isoweek(date),
    yearweek = iso_year * 100 + iso_week,
    yearweek2 = paste0(iso_year, "-W", sprintf("%02d", iso_week))
  )
  df
})  

data_mpox_tab <- reactive({
  df <- readRDS(url("https://github.com/Dhihram/WHO_SEAR/raw/refs/heads/main/Dat/mpox_demog.rds"))
  # Return NULL or empty early if data fails to load
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  return(df)
})