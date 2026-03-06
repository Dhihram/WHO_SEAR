## PLOTS INFLUENZA

output$cases_plot <- renderPlotly({
  data  <- filtered_data()
  data2 <- filtered_data2()
  
  req(data)
  if (nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = "No data available for selected filters"))
  }
  
  # Colors + labels
  flu_colors <- c(
    "flu_ah1" = "#C9C9C9", "flu_ah1n12009" = "#8FE6E7", "flu_ah3" = "#00A1D5",
    "flu_ah5" = "#74F907", "flu_anot" = "#104f82", "flu_bnot" = "#9c4b30",
    "flu_byam" = "#FFB86D", "flu_vic" = "#F79700"
  )
  
  flu_labels <- c(
    "flu_ah1" = "A(H1)", "flu_ah1n12009" = "A(H1N1)pdm2009", "flu_ah3" = "A(H3)",
    "flu_ah5" = "A(H5)", "flu_anot" = "A not Subtyped", "flu_bnot" = "B not Determined",
    "flu_byam" = "B Yamagata", "flu_vic" = "B Victoria"
  )
  
  # ---- stable x ordering (so bars + line align) ----
  x_levels <- data %>%
    distinct(yearweek2) %>%
    arrange(yearweek2) %>%
    pull(yearweek2)
  
  data <- data %>% mutate(yearweek2 = factor(yearweek2, levels = x_levels))
  if (!is.null(data2) && nrow(data2) > 0) {
    data2 <- data2 %>% mutate(yearweek2 = factor(yearweek2, levels = x_levels))
  }
  
  p <- plot_ly()
  
  # ---- stacked bars (hover shows ONLY "Subtype: cases") ----
  for (type in unique(na.omit(data$Flu_Type))) {
    df_sub <- dplyr::filter(data, Flu_Type == type)
    lbl <- unname(flu_labels[type])
    if (is.na(lbl) || is.null(lbl)) lbl <- type
    
    p <- p %>%
      add_trace(
        data = df_sub,
        x = ~yearweek2,
        y = ~Cases,
        type = "bar",
        name = lbl,
        marker = list(color = if (!is.null(flu_colors[[type]])) flu_colors[[type]] else "#999999"),
        hovertemplate = "%{fullData.name}: %{y:,}<extra></extra>"
      )
  }
  
  # ---- line (shows in unified hover as "Positive Rate: xx%") ----
  if (!is.null(data2) && nrow(data2) > 0) {
    p <- p %>%
      add_lines(
        data = data2,
        x = ~yearweek2,
        y = ~pos_rate,
        yaxis = "y2",
        name = "% Positive",
        line = list(color = "red"),
        hovertemplate = "% Positive: %{y:.1f}%<extra></extra>"
        # If pos_rate is 0-1 instead of 0-100, use:
        # hovertemplate = "Positive Rate: %{y:.1%}<extra></extra>"
      )
  }
  
  p %>%
    layout(
      title = " ",
      barmode = "stack",
      hovermode = "x unified",
      separators = ". ",
      xaxis = list(title = "Week", fixedrange = TRUE),
      yaxis = list(title = "Influenza Cases", fixedrange = TRUE),
      
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        title = "% Positive",
        titlefont = list(color = "red"),
        tickfont = list(color = "red"),
        showgrid = FALSE,
        rangemode = "nonnegative",
        automargin = TRUE,
        fixedrange = TRUE
      ),
      
      legend = list(orientation = "h", x = 0.5, y = 9, xanchor = "center",  font = list(size = 8)),
      margin = list(t = 50)
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "zoom2d","pan2d","select2d","lasso2d",
        "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
        "hoverClosestCartesian","hoverCompareCartesian","toggleSpikelines"
      ),
      toImageButtonOptions = list(
        format = "png",
        filename = "influenza_plot1",
        width = 1200,
        height = 600,
        scale = 2
      )
    )
})



output$ili_plot <- renderPlotly({
  df <- filtered_data4()
  if (nrow(df) == 0) return(plotly_empty() %>% layout(title = "No ILI data available"))
  
  plot_ly() %>%     # <-- IMPORTANT: empty plot
    add_bars(
      data = df,
      x = ~yearweek2,
      y = ~ILI_CASE,
      name = "ILI",
      marker = list(color = "blue"),
      customdata = ~ili_1000,
      hovertemplate = paste0(
        "<b>ISO Week: %{x}</b>",
        "<br>ILI cases: %{y:,.0f}",
        "<br>ILI per 1000: %{customdata:.2f}",
        "<extra></extra>"
      )
    )  %>%    add_lines(
      data = df,
      x = ~yearweek2,
      y = ~ili_1000,
      name = "ILI per 1000 Outpatients",
      yaxis = "y2",
      line = list(color = "red"),
      hoverinfo = "skip"
    ) %>%
    layout(
      hovermode = "x",
      separators = ". ",
      spikedistance = -1,
      xaxis = list(
        title = "ISO Year–Week",
        type = "category",
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "data",
        spikethickness = 1.5,
        spikedash = "dot",
        spikecolor = "rgba(80,80,80,0.7)",
        fixedrange = TRUE
      ),
      yaxis = list(title = "ILI Cases", 
                   fixedrange = TRUE),
      yaxis2 = list(
        title = "ILI per 1000 Outpatients",
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        titlefont = list(color = "red"),
        tickfont = list(color = "red"),
        rangemode = "nonnegative",
        automargin = TRUE,
        fixedrange = TRUE
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.1,
        xanchor = "center"
      )
    ) %>% config(
      displaylogo = FALSE,                      # hide Plotly logo
      modeBarButtonsToRemove = c(               # remove all but 'toImage'
        "zoom2d","pan2d","select2d","lasso2d",
        "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
        "hoverClosestCartesian","hoverCompareCartesian","toggleSpikelines"
      ),
      toImageButtonOptions = list(              # optional: control export
        format = "png", filename = "influenza_plot2",
        width = 1200, height = 600, scale = 2
      )
    )
})

output$sari_plot <- renderPlotly({
  df <- filtered_data4()
  if (nrow(df) == 0) return(plotly_empty() %>% layout(title = "No SARI data available"))
  plot_ly() %>%
    add_bars(
      data = df,
      x = ~yearweek2,
      y = ~SARI_CASE,
      name = "SARI",
      marker = list(color = "darkgreen"),
      customdata = ~sari_100,
      hovertemplate = paste0(
        "<b>ISO Week: %{x}</b>",
        "<br>SARI Cases: %{y:,.0f}",
        "<br>SARI per 100 Inpatients: %{customdata:.2f}",
        "<extra></extra>"
      )
    ) %>%
    add_lines(
      data = df,
      x = ~yearweek2,
      y = ~sari_100,
      name = "SARI per 100 Inpatients",
      yaxis = "y2",
      line = list(color = "red"),
      hoverinfo = "skip"
    ) %>%
    layout(
      hovermode = "x",
      separators = ". ",
      spikedistance = -1,
      xaxis = list(
        title = "ISO Year–Week",
        type = "category",          
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "data",
        spikethickness = 1.5,
        spikedash = "dot",
        spikecolor = "rgba(80,80,80,0.7)",
        fixedrange = TRUE
      ),
      yaxis = list(title = "SARI Cases",
                   fixedrange = TRUE),
      yaxis2 = list(
        title = "SARI per 100 Inpatients",
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        titlefont = list(color = "red"),
        tickfont = list(color = "red"),
        rangemode = "nonnegative",
        automargin = TRUE,
        fixedrange = TRUE
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.1,
        xanchor = "center"
      )
    ) %>% config(
      displaylogo = FALSE,                      # hide Plotly logo
      modeBarButtonsToRemove = c(               # remove all but 'toImage'
        "zoom2d","pan2d","select2d","lasso2d",
        "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
        "hoverClosestCartesian","hoverCompareCartesian","toggleSpikelines"
      ),
      toImageButtonOptions = list(              # optional: control export
        format = "png", filename = "influenza_plot3",
        width = 1200, height = 600, scale = 2
      )
    )
})


## Table
output$summary_table_ui <- renderUI({
  summary_table <- filtered_data3()
  
  if (nrow(summary_table) == 0) {
    return(tags$p("No data available for the selected filters."))
  }
  
  table_html <- formattable::formattable(
    summary_table,
    list(
      Country = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
      area(col = 2:3) ~ formatter("span", x ~ prettyNum(x, big.mark = " ", preserve.width = "none")),
      `% Positive` = function(x) percent(x / 100),
      area(col = 5:12) ~ function(x) percent(x / 100, digits = 1),
      area(col = 5:12) ~ color_tile("#fcedf2", "#d66389")
    )
  )
  
  # ✅ Wrap the rendered HTML table inside a scrollable div
  div(
    style = "overflow-x:auto; -webkit-overflow-scrolling: touch; white-space:nowrap;",
    HTML(as.character(table_html))
  )
})

output$summary_table_uib <- renderUI({
  summary_table <- filtered_data3b()
  
  for (i in 5:12) {
    # original column name
    old_name <- names(summary_table)[i]
    
    # new column name: remove _x if it exists at the end
    new_name <- sub("_x$", "", old_name)
    
    # create new column with formatted values
    summary_table[[new_name]] <- comma(summary_table[[i]], big.mark = " ")
  }
  
  
  table_html <- formattable::formattable(
    summary_table %>%
      select(Country,`Total Samples Tested`, `Number of Influenza Positive`, `% Positive`,
             `A (H1)`, `A (H3)`, `A (H5)`, `A (H1N1)pdm09`, `A (Unsubtype)`,
             `B (Yamagata)`, `B (Victoria)`, `B (Lineage not Determined)`),
    list(
      Country = formatter("span", style = ~ style(
        color = "black",
        font.weight = "bold"
      )),
      
      area(col = 2:3) ~ formatter("span",
                                  x ~ prettyNum(x, big.mark = " ", preserve.width = "none")),
      `% Positive` = function(x) percent(x / 100),
      `A (H1)` = formatter("span",
                           style = ~ style(
                             display = "block",
                             `border-radius` = "4px",
                             `padding-right` = "4px",
                             `background-color` = csscolor(gradient(summary_table$`A (H1) %`, "#fcedf2", "#d66389"))
                           )),
      `A (H3)` = formatter("span",
                           style = ~ style(
                             display = "block",
                             `border-radius` = "4px",
                             `padding-right` = "4px",
                             `background-color` = csscolor(gradient(summary_table$`A (H3) %`, "#fcedf2", "#d66389"))
                           )),
      `A (H5)` = formatter("span",
                           style = ~ style(
                             display = "block",
                             `border-radius` = "4px",
                             `padding-right` = "4px",
                             `background-color` = csscolor(gradient(summary_table$`A (H5) %`, "#fcedf2", "#d66389"))
                           )),
      `A (H1N1)pdm09` = formatter("span",
                                  style = ~ style(
                                    display = "block",
                                    `border-radius` = "4px",
                                    `padding-right` = "4px",
                                    `background-color` = csscolor(gradient(summary_table$`A (H1N1)pdm09 %`, "#fcedf2", "#d66389"))
                                  )),
      `A (Unsubtype)` = formatter("span",
                                  style = ~ style(
                                    display = "block",
                                    `border-radius` = "4px",
                                    `padding-right` = "4px",
                                    `background-color` = csscolor(gradient(summary_table$`A (Unsubtype) %`, "#fcedf2", "#d66389"))
                                  )),
      `B (Yamagata)` = formatter("span",
                                 style = ~ style(
                                   display = "block",
                                   `border-radius` = "4px",
                                   `padding-right` = "4px",
                                   `background-color` = csscolor(gradient(summary_table$`B (Yamagata) %`, "#fcedf2", "#d66389"))
                                 )),
      `B (Victoria)` = formatter("span",
                                 style = ~ style(
                                   display = "block",
                                   `border-radius` = "4px",
                                   `padding-right` = "4px",
                                   `background-color` = csscolor(gradient(summary_table$`B (Victoria) %`, "#fcedf2", "#d66389"))
                                 )),
      `B (Lineage not Determined)` = formatter("span",
                                               style = ~ style(
                                                 display = "block",
                                                 `border-radius` = "4px",
                                                 `padding-right` = "4px",
                                                 `background-color` = csscolor(gradient(summary_table$`B (Lineage not Determined) %`, "#fcedf2", "#d66389"))
                                               ))
    )
  )
  
  div(
    style = "overflow-x:auto; -webkit-overflow-scrolling: touch; white-space:nowrap;",
    HTML(as.character(table_html))
  )
})

#Table Switch
output$table_switch <- renderUI({
  if (input$table_view == "pct") {
    uiOutput("summary_table_ui")
  } else {
    uiOutput("summary_table_uib")
  }
})
