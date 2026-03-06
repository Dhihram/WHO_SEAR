filtered_data <- reactive({
  df <- data_flu_long()
  if (nrow(df) == 0) return(tibble())
  
  # input range as YYYYWW
  start_yw <- as.integer(input$start_year) * 100 + as.integer(input$start_week)
  end_yw   <- as.integer(input$end_year)   * 100 + as.integer(input$end_week)
  
  # ensure start <= end
  if (end_yw < start_yw) {
    tmp <- start_yw
    start_yw <- end_yw
    end_yw <- tmp
  }
  
  df %>%
    filter(yearweek >= start_yw, yearweek <= end_yw) %>%
    filter(COUNTRY_AREA_TERRITORY == input$country_filter)
})

filtered_data2 <- reactive({
  df <- data_flu_tab()
  if (nrow(df) == 0) return(tibble())
  
  # input range as YYYYWW
  start_yw <- as.integer(input$start_year) * 100 + as.integer(input$start_week)
  end_yw   <- as.integer(input$end_year)   * 100 + as.integer(input$end_week)
  
  # ensure start <= end
  if (end_yw < start_yw) {
    tmp <- start_yw
    start_yw <- end_yw
    end_yw <- tmp
  }
  
  
  df %>%
    filter(yearweek >= start_yw, yearweek <= end_yw) %>%
    filter(COUNTRY_AREA_TERRITORY == input$country_filter)
})

filtered_data3 <- reactive({
  df <- data_flu_tab()
  if (nrow(df) == 0) {
    return(tibble(Country = character(), `Total Samples Tested` = numeric()))
  }
  
  # input range as YYYYWW
  start_yw <- as.integer(input$start_year) * 100 + as.integer(input$start_week)
  end_yw   <- as.integer(input$end_year)   * 100 + as.integer(input$end_week)
  
  # ensure start <= end
  if (end_yw < start_yw) {
    tmp <- start_yw
    start_yw <- end_yw
    end_yw <- tmp
  }
  
  df %>%
    filter(yearweek >= start_yw, yearweek <= end_yw) %>%
    group_by(COUNTRY_AREA_TERRITORY) %>%
    summarise(
      `Total Samples Tested` = sum(specimen, na.rm = TRUE),
      `Total Subtyped` = sum(flu_all, na.rm = TRUE),
      `% Positive` = round((`Total Subtyped`/`Total Samples Tested`)*100,0),
      `A (H1) %` = sum(flu_ah1, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (H3) %` = sum(flu_ah3, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (H5) %` = sum(flu_ah5, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (H1N1)pdm09 %` = sum(flu_ah1n12009, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (Unsubtype) %` = sum(flu_anot, na.rm = TRUE) / `Total Subtyped` * 100,
      `B (Yamagata) %` = sum(flu_byam, na.rm = TRUE) / `Total Subtyped` * 100,
      `B (Victoria) %` = sum(flu_vic, na.rm = TRUE) / `Total Subtyped` * 100,
      `B (Lineage not Determined) %` = sum(flu_bnot, na.rm = TRUE) / `Total Subtyped` * 100
    ) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), 0, .))) %>%
    arrange(COUNTRY_AREA_TERRITORY) %>%
    rename(Country = COUNTRY_AREA_TERRITORY, `Number of Influenza Positive` = `Total Subtyped`) 
})

filtered_data3b <- reactive({
  df <- data_flu_tab()
  if (nrow(df) == 0) {
    return(tibble(Country = character(), `Total Samples Tested` = numeric()))
  }
  
  # input range as YYYYWW
  start_yw <- as.integer(input$start_year) * 100 + as.integer(input$start_week)
  end_yw   <- as.integer(input$end_year)   * 100 + as.integer(input$end_week)
  
  # ensure start <= end
  if (end_yw < start_yw) {
    tmp <- start_yw
    start_yw <- end_yw
    end_yw <- tmp
  }
  
  df %>%
    filter(yearweek >= start_yw, yearweek <= end_yw) %>%
    group_by(COUNTRY_AREA_TERRITORY) %>%
    summarise(
      `Total Samples Tested` = sum(specimen, na.rm = TRUE),
      `Total Subtyped` = sum(flu_all, na.rm = TRUE),
      `% Positive` = round((`Total Subtyped`/`Total Samples Tested`)*100,0),
      `A (H1)_x` = sum(flu_ah1, na.rm = TRUE),
      `A (H3)_x` = sum(flu_ah3, na.rm = TRUE),
      `A (H5)_x` = sum(flu_ah5, na.rm = TRUE),
      `A (H1N1)pdm09_x` = sum(flu_ah1n12009, na.rm = TRUE),
      `A (Unsubtype)_x` = sum(flu_anot, na.rm = TRUE),
      `B (Yamagata)_x` = sum(flu_byam, na.rm = TRUE),
      `B (Victoria)_x` = sum(flu_vic,na.rm = TRUE),
      `B (Lineage not Determined)_x` = sum(flu_bnot,na.rm = TRUE),
      `% Positive` = round((`Total Subtyped`/`Total Samples Tested`)*100,0),
      `A (H1) %` = sum(flu_ah1, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (H3) %` = sum(flu_ah3, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (H5) %` = sum(flu_ah5, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (H1N1)pdm09 %` = sum(flu_ah1n12009, na.rm = TRUE) / `Total Subtyped` * 100,
      `A (Unsubtype) %` = sum(flu_anot, na.rm = TRUE) / `Total Subtyped` * 100,
      `B (Yamagata) %` = sum(flu_byam, na.rm = TRUE) / `Total Subtyped` * 100,
      `B (Victoria) %` = sum(flu_vic, na.rm = TRUE) / `Total Subtyped` * 100,
      `B (Lineage not Determined) %` = sum(flu_bnot, na.rm = TRUE) / `Total Subtyped` * 100
    ) %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), 0, .))) %>%
    arrange(COUNTRY_AREA_TERRITORY) %>%
    rename(Country = COUNTRY_AREA_TERRITORY, `Number of Influenza Positive` = `Total Subtyped`) 
})

filtered_data4 <- reactive({
  df <- data_flu_id()
  if (nrow(df) == 0) return(tibble())
  
  # input range as YYYYWW
  start_yw <- as.integer(input$start_year) * 100 + as.integer(input$start_week)
  end_yw   <- as.integer(input$end_year)   * 100 + as.integer(input$end_week)
  
  # ensure start <= end
  if (end_yw < start_yw) {
    tmp <- start_yw
    start_yw <- end_yw
    end_yw <- tmp
  }
  
  df %>%
    filter(yearweek >= start_yw, yearweek <= end_yw) %>%
    filter(COUNTRY_AREA_TERRITORY == input$country_filter)
})
