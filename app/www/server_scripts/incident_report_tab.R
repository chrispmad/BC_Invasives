# # # # # # # # INCIDENT REPORT TRACKING DASHBOARD # # # # # # # # #

data_colour = 'darkgreen'
hl_colour = 'darkred'

inc_dat = openxlsx::read.xlsx('Master Incidence Report Records.xlsx',
                              sheet = 'Aquatic Reports') |>
  dplyr::mutate(Date = openxlsx::convertToDate(Date)) |>
  dplyr::mutate(year = lubridate::year(Date))

inc_dat_sf = inc_dat |>
  dplyr::mutate(lon = as.numeric(Longitude),
                lat = as.numeric(Latitude)) |>
  dplyr::filter(!is.na(lon),!is.na(lat)) |>
  sf::st_as_sf(coords = c("lon","lat"),
               crs = 4326)

# 1. Number of Reports per year
output$reports_over_time = renderPlot({

  num_reports_per_year = inc_dat |>
    dplyr::group_by(year) |>
    dplyr::summarise(number_reports = n()) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::mutate(running_total = cumsum(number_reports))

  ggplot(data = num_reports_per_year) +
    geom_line(aes(x = year,
                  y = running_total),
              group = 1) +
    geom_point(
      aes(x = year,
          y = running_total
      )
    ) +
    theme_minimal()
})

# 2. Top Reported Species
output$top_reported_species = renderPlot({
  inc_dat |>
    dplyr::count(Submitted_Common_Name) |>
    dplyr::filter(!is.na(Submitted_Common_Name),
                  Submitted_Common_Name != '?') |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::slice(c(1:10)) |>
    dplyr::mutate(Submitted_Common_Name = as.factor(Submitted_Common_Name)) |>
    dplyr::mutate(Submitted_Common_Name = forcats::fct_inorder(Submitted_Common_Name)) |>
    ggplot() +
    geom_col(
      aes(x = Submitted_Common_Name,
          y = n
      ),
      col = data_colour,
      fill = data_colour
    ) +
    labs(x = 'Submitted Common Name',
         y = 'Count') +
    coord_flip()
})

# 3. Confirmation status bar-plot
output$confirmation_status = renderPlot({
  d_count = inc_dat |>
    count(ID_Confirmation) |>
    filter(!is.na(ID_Confirmation))

  ggplot(d_count) +
    geom_col(aes(x = ID_Confirmation,
                 y = n,
                 fill = ID_Confirmation)) +
    geom_text(
      aes(x = ID_Confirmation,
          y = n/2,
          label = lapply(
            paste0(
              ID_Confirmation,
              "\n",
              n),
            shiny::HTML
          )),
      col = 'white') +
    theme_minimal() +
    scale_fill_brewer(palette = 'Dark2') +
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank())
})

# 4. Leaflet map of incident reports
output$inc_report_leaflet = renderLeaflet({
  leaflet() |>
    addTiles() |>
    addCircleMarkers(
      data = inc_dat_sf,
      label = ~Submitted_Common_Name,
      radius = 2
    )
})

# 5. Summary number of total reports
output$total_reports = renderText({
  nrow(inc_dat)
})

# 6. Summary number of distinct species
output$distinct_sp_reports = renderText({
  length(unique(inc_dat$Submitted_Common_Name))
})

# 7. Incident Tree Plot
output$outcome_tree_plot = renderPlotly({
  outcome_summary = inc_dat |>
    count(Outcome_and_or_Action)

  plot_ly(
    type='treemap',
    labels= outcome_summary$Outcome_and_or_Action,
    parents= ~rep('',length( outcome_summary$Outcome_and_or_Action)),#,
    values= ~ outcome_summary$n,
    # textinfo="label+value+percent parent+percent entry+percent root",
    # domain=list(column=0)
  ) |>
    plotly::layout(margin=list(l=0, r=0, t=0, b=0))
})

# 8. Region Column Bar
output$region_column_bar = renderPlotly({
  p = inc_dat |>
    dplyr::rename(reg = Natural_Resource_Region) |>
    dplyr::count(reg) |>
    dplyr::filter(!is.na(reg)) |>
    dplyr::mutate(reg = stringr::str_replace_all(reg, '-', '\n')) |>
    dplyr::mutate(reg = factor(reg)) |>
    dplyr::arrange(dplyr::desc(n)) |>
    mutate(reg = forcats::fct_inorder(reg)) |>
    mutate(reg = forcats::fct_lump(reg, n = 4, w = n)) |>
    ggplot() +
    geom_col(aes(x = reg, y = n, fill = reg)) +
    scale_fill_brewer(palette = 'Dark2') +
    labs(x = '', y = '') +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  ggplotly(p) |>
    plotly::layout(margin=list(l=0, r=0, t=0, b=0))
})
