# # # Find Connected Waterbodies # # #

waterbody_graph = reactiveVal()

observeEvent(input$run_wb_connectivity_search, {
  cat('starting wb connectivity search')
  results = tryCatch(
    bcinvadeR::get_connected_waterbodies(
      waterbody_coordinates = c(as.numeric(input$wb_for_downst_lng),
                                as.numeric(input$wb_for_downst_lat)),
      in_shiny = T,
      the_session = session
    ),
    error = function(e) ("No waterbodies connected to target waterbody")
  )
  waterbody_graph(results)
  cat('Finished finding waterbody network.')
})

output$waterbody_graph_as_image = renderPlotly({
  req(!is.null(waterbody_graph()))
  if('No waterbodies connected to target waterbody' %in% waterbody_graph()){
    plotly::ggplotly(
      ggplot2::ggplot() +
        ggplot2::geom_text(aes(x = 1, y = 1),
                           label = 'No waterbodies connected to target waterbody')
    )
  } else {
    wbs = waterbody_graph() |>
      sf::st_transform(crs = 4326)

    p = ggplot2::ggplot() +
      ggplot2::geom_sf(data = wbs,
                       fill = 'darkblue',
                       aes(tooltip = name)) +
      ggplot2::geom_sf_label(
        data = wbs,
        aes(label = name)
      ) +
      ggthemes::theme_map()

    plotly::ggplotly(p)
  }
})

output$waterbody_graph_as_table = renderTable({
  req(!is.null(waterbody_graph()))
  if('No waterbodies connected to target waterbody' %in% waterbody_graph()){
    data.frame(results = waterbody_graph())
  } else {
    # Find number of named lakes in resultin graph.
    waterbody_graph() |>
      sf::st_drop_geometry() |>
      dplyr::mutate(is_lake = stringr::str_detect(name, 'Lake')) |>
      dplyr::count(is_lake) |>
      dplyr::mutate(is_lake = ifelse(is_lake, 'Named Lakes','Other Waterbodies')) |>
      dplyr::mutate(is_lake = tidyr::replace_na(is_lake, 'Other Waterbodies')) |>
      dplyr::group_by(is_lake) |>
      dplyr::summarise(n = sum(n)) |>
      tidyr::pivot_wider(names_from = is_lake, values_from = n)
  }
})

output$waterbody_graph_named_lake_list = renderText({
  req(!is.null(waterbody_graph()))
  if('No waterbodies connected to target waterbody' %in% waterbody_graph()){
    ''
  } else {
    lake_names = waterbody_graph() |>
      sf::st_drop_geometry() |>
      dplyr::filter(stringr::str_detect(name, 'Lake')) |>
      dplyr::summarise(lake_names = paste0(name, collapse = ', ')) |>
      dplyr::pull(lake_names)
    paste0("Lake Names include: ",lake_names,".")
  }
})
