function(input, output, session) {
  # Update bcinvadeR package, if necessary!

  # Probably unnecessary once the app is published...
  if(!stringr::str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }

  # Define function to print out prioritization model console outpupt.
  print_p_model_console_feedback = function(){
    withCallingHandlers({
      shinyjs::html("p_model_feedback", "")
      p_model$print()
    },
    message = function(m) {
      shinyjs::html(id = "p_model_feedback", html = m$message, add = TRUE)
    })
  }

  bc = sf::read_sf('bc_simple.shp')

  observeEvent(input$update_bcinvadeR, {
    showModal(
      modalDialog(
        "Are you sure you would like to update the {bcinvadeR} package?",
        title = 'Update {bcinvadeR}',
        easyClose = TRUE,
        fade = F,
        footer = actionButton(inputId = 'yes_update',
                              label = "I'm sure!")
      )
    )
  })

  observeEvent(input$yes_update, {
    withProgress(message = 'Updating {bcinvadeR} package...',
                 value = 0.5,
                 expr = {
                   detach("package:bcinvadeR", unload = TRUE)
                   tryCatch(expr = devtools::install_github('chrispmad/bcinvadeR',
                                                            upgrade = 'never'),
                            error = function(e) cat("No need to update - no new changes!"))
                   incProgress(amount = 0.5, message = 'Updated!')
                 })
    session$reload()
    library(bcinvadeR)
  })

  # Respond to upload of up-to-date master excel tracking sheet.
  observeEvent(input$inc_report_xlsx, {
    req(!is.null(input$inc_report_xlsx))
    file.copy(input$inc_report_xlsx$datapath, "Master Incidence Report Records.xlsx")
  })

  # # # # # # # # SPECIES SEARCH TAB # # # # # # # # #

  # # # All Records of Select Species in Province # # #

  # Module to search for species in Species Search Tab.
  all_rec_of_sp = spec_search_Server("sp_search_bar")

  output$all_rec_of_sp_results = DT::renderDT({
    DT::datatable(
      all_rec_of_sp()
    )
  })

  searched_species = reactive({
    unique(all_rec_of_sp()$Species)
  })

  output$all_recs_of_sp_enum = renderText({
    req(!is.null(all_rec_of_sp()))
    paste0("The search returned ",nrow(all_rec_of_sp())," results.")
  })

  output$all_recs_of_sp_dl = downloadHandler(
    filename = function() {
      paste0('All_records_of_',searched_species(),'.xlsx')
    },
    content = function(file) {
      output = all_rec_of_sp() |>
        dplyr::mutate(latitude = sf::st_coordinates(geometry)[,2],
                      longitude = sf::st_coordinates(geometry)[,1]) |>
        sf::st_drop_geometry()
      openxlsx::write.xlsx(output, file)
    }
  )

  # # # All Species in 1 Waterbody # # #
  all_sp_in_wbs = reactiveVal(data.frame(results = 'No search run yet.'))

  observeEvent(input$search_for_all_sp_in_wb, {
    req(!is.null(input$all_sp_in_wbs_name) | (!is.null(input$all_sp_in_wb_lat) & !is.null(input$all_sp_in_wb_lng)))

    buffer_size = input$waterbody_poly_buffer_size

    withProgress(
      message = 'Getting waterbody polygon',
      detail = 'Combining records...',
      value = 0/5,
      expr = {

        wb_poly = get_waterbody_polygon(focus_wb_name = input$all_sp_in_wbs_name,
                                        focus_wb_coordinates = c(input$all_sp_in_wb_lng,input$all_sp_in_wb_lat))

        # Buffer the waterbody polygon by, say, 100 meters.
        wb_poly = sf::st_buffer(wb_poly, dist = buffer_size)

        all_species_results = tryCatch(
          find_all_species_in_waterbody(
            wb_poly,
            in_shiny = T,
            sources = input$all_sp_in_wbs_sources,
            exclude = c('Fungi','Plantae'),
            excel_path = 'Master Incidence Report Records.xlsx',
            sheet_name = 'Aquatic Reports',
            excel_species_var = 'Submitted_Common_Name',
            output_crs = 4326,
            quiet = T),
          error = function(e) (data.frame(result = "No records found for target waterbody"))
        )

        all_sp_in_wbs(all_species_results)
      })
  })

  output$all_sp_in_wb_enum = renderText({
    paste0("The search returned ",nrow(all_sp_in_wbs())," results.")
  })

  output$all_sp_in_wb_results = DT::renderDT({
    DT::datatable(
      all_sp_in_wbs()
    )
  })

  output$all_sp_in_wb_dl = downloadHandler(
    filename = function() {
      paste0('All_species_in_',input$all_sp_in_wbs_name,'.xlsx')
    },
    content = function(file) {
      output = all_sp_in_wbs() |>
        dplyr::mutate(latitude = sf::st_coordinates(geometry)[,2],
                      longitude = sf::st_coordinates(geometry)[,1]) |>
        sf::st_drop_geometry()
      openxlsx::write.xlsx(output, file)
    }
  )

  # Make leaflet map for finding lag/long coords
  output$wb_finder_map = renderLeaflet({
    leaflet() |>
      addTiles() |>
      # addPolygons(
      #   weight = 2,
      #   color = 'black',
      #   data = bc
      # ) |>
      setView(lng = -124, lat = 54, zoom = 5) |>
      leaflet.extras::addResetMapButton() |>
      leaflet.extras::addSearchGoogle()
  })

  clicked_lng = reactiveVal(NULL)
  clicked_lat = reactiveVal(NULL)

  observeEvent(input$wb_finder_map_click, {
    # Which tab is clicked? Update that waterbody coordinate section.

    clicked_lng(input$wb_finder_map_click$lng)
    clicked_lat(input$wb_finder_map_click$lat)
    # Apply the clicked lat and long to any text input asking for lat / lng.

    # All species in 1 waterbody
    updateTextInput(inputId = 'all_sp_in_wb_lng',
                    value = clicked_lng())
    updateTextInput(inputId = 'all_sp_in_wb_lat',
                    value = clicked_lat())

    # Connected Waterbodies
    updateTextInput(inputId = 'wb_for_downst_lng',
                    value = clicked_lng())
    updateTextInput(inputId = 'wb_for_downst_lat',
                    value = clicked_lat())
  })

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

  # Reactive form of data - what for mouse events and add selected data to
  # a reactiveVal object. Keep track of what data filter has been selected
  # (e.g. date from the running total figure) - if the user clicks on a different
  # filter type, switch to that one.

  # filter_type_selected = reactiveVal()
  #
  # observeEvent(
  #   c(input$reports_over_time_selected,
  #     input$top_reported_species_selected),
  #   {
  #
  #     browser()
  #     filter_update = NULL
  #
  #     # Is there already a value for filter_type_selected() ?
  #     # If so, find new filter_update selection and then
  #     # wipe
  #     if(!is.null(filter_type_selected())){
  #       filter_type_selected(NULL)
  #     }
  #     # If one of these filters is not null, record filter type in filter_update object
  #     if(!is.null(input$reports_over_time_selected)) filter_update = 'reports_over_time'
  #     if(!is.null(input$top_reported_species_selected)) filter_update = 'top_reported_species'
  #
  #     if(!is.null(filter_update)){
  #       # If filter_type_selected() is NULL, adopt the new filter right away.
  #       if(is.null(filter_type_selected())) {
  #         filter_type_selected(filter_update)
  #       } else {
  #         # Otherwise, test to see if its the same as the (new) filter type.
  #         # test to see if filter_update object is distinct from current value (or NULL) of filter_type_selected
  #         # If it is, apply filter type to reactiveVal.
  #         if(filter_type_selected() != filter_update) filter_type_selected(filter_update)
  #         # If it is the same, clear the filter reactiveVal.
  #         if(filter_type_selected() == filter_update) filter_type_selected(NULL)
  #       }
  #     }
  #     print(filter_type_selected())
  #   })

  # inc_dat_sel = reactive({
  #   # Initially, all data is included.
  #   d = inc_dat
  #
  #   # Depending on which type of filter has been activated by the user's mouse clicks,
  #   # apply a filter to the data.
  #   if(!is.null(filter_type_selected())){
  #
  #     # Year filter
  #     if(filter_type_selected() == '')
  #     reports_over_time
  #   }
  #   if(!is.null(input$reports_over_time_selected)){
  #   d = d |>
  #     dplyr::filter(year %in% input$reports_over_time_selected)
  #   return(d)
  #   }
  #   if(!is.null(input$top_reported_species_selected)){
  #     d = d |>
  #       dplyr::filter(year %in% input$reports_over_time_selected)
  #     return(d)
  #   }
  #   d
  # })

  # 1. Number of Reports per year
  output$reports_over_time = renderPlot({

    # browser()

    num_reports_per_year = inc_dat |>
      dplyr::group_by(year) |>
      dplyr::summarise(number_reports = n()) |>
      dplyr::filter(!is.na(year)) |>
      dplyr::mutate(running_total = cumsum(number_reports))

    # if(!is.null(inc_dat_sel()))
    # num_reports_per_year_hl = inc_dat_sel() |>
    #   dplyr::group_by(year) |>
    #   dplyr::summarise(number_reports = n()) |>
    #   dplyr::filter(!is.na(year)) |>
    #   dplyr::mutate(running_total = cumsum(number_reports))

    ggplot(data = num_reports_per_year) +
      geom_line(aes(x = year,
                    y = running_total),
                group = 1) +
      # geom_point(data = num_reports_per_year_hl,
      #            aes(x = year,
      #                y = running_total),
      #            col = hl_colour,
      #            fill = hl_colour) +
      geom_point(
        aes(x = year,
            y = running_total#,
            # tooltip = lapply(paste0('Year: ',year, '<br>Reports to date: ',running_total),shiny::HTML),
            # data_id = lapply(paste0('Year: ',year, '<br>Reports to date: ',running_total),shiny::HTML)
            # data_id = year
        )
      ) +
      theme_minimal()

    # girafe(ggobj = gg_point)
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
            y = n#,
            # tooltip = Submitted_Common_Name,
            # data_id = Submitted_Common_Name
            ),
        col = data_colour,
        fill = data_colour
        ) +
      # geom_col(
      #   data = inc_dat_sel(),
      #   aes(x = Submitted_Common_Name,
      #       y = n),
      #   col = hl_colour,
      #   fill = hl_colour
      # ) +
      labs(x = 'Submitted Common Name',
           y = 'Count') +
      coord_flip()

    # girafe(ggobj = top_reported_species_g)
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

  # # # Find Connected Waterbodies # # #

  waterbody_graph = reactiveVal()

  observeEvent(input$run_wb_connectivity_search, {
    cat('starting wb connectivity search')
    # if(input$downstream_wb_only){
    #   results = tryCatch(
    #     bcinvadeR::find_downstream_waterbodies(
    #       input$wb_for_downst,
    #       c(as.numeric(input$wb_for_downst_lng),
    #         as.numeric(input$wb_for_downst_lat)),
    #       the_session = session
    #     ),
    #     error = function(e) cat("Error connecting to BC Data Catalogue!")
    #   )
    #   if(results == "Error connecting to BC Data Catalogue!"){stop("Sorry, Downstream analysis failed!")}
    # } else {
    # browser()
      results = tryCatch(
        bcinvadeR::get_connected_waterbodies(
          # waterbody_name = input$wb_for_downst,
          waterbody_coordinates = c(as.numeric(input$wb_for_downst_lng),
            as.numeric(input$wb_for_downst_lat)),
          in_shiny = T,
          the_session = session
        ),
        error = function(e) ("No waterbodies connected to target waterbody")
      )
    # }
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

  # # # AIS Rangemaps # # #

  # Read in priority list of AIS species (excel file), use it to update selectInput
  pr_sp = vroom::vroom('priority_species_table.csv')

  # Read in occurrence data for species (this is updated each time the 'publish_app.R'
  # script is run)
  occ_dat = sf::read_sf('occ_dat.gpkg')

  # Calculate number of rows of data per species; add as label to species selector.
  pr_sp = pr_sp |>
    left_join(
      occ_dat |>
        sf::st_drop_geometry() |>
        dplyr::count(Species) |>
        dplyr::rename(name = Species)
    ) |>
    mutate(n = replace_na(n, 0)) |>
    dplyr::mutate(label = paste0(name,' (',n,' records)'))


  # Update species selector
  shinyWidgets::updatePickerInput(session = session,
                                  inputId = 'ais_rangemap_sp',
                                  choices = pr_sp$label)

  selected_species = reactive({
    stringr::str_squish(stringr::str_remove(input$ais_rangemap_sp, ' \\(.*'))
  })

  occ_dat_sp = reactive({
    occ_dat |>
      dplyr::filter(Species %in% selected_species() | Species %in% stringr::str_to_title(selected_species()))
  })

  # build-in color palette
  my_colours = glasbey.colors(length(unique(occ_dat$Species)))

  # Create leaflet palette
  occ_dat_pal = reactive({
    leaflet::colorFactor(
      palette = my_colours,
      domain = unique(occ_dat$Species)
    )
  })

  output$ais_rangemap_leaf = renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::clearGroup(group = 'selected_species_circles') |>
      leaflet::removeControl('selected_species_legend') |>
      leaflet::addPolygons(
        data = bcmaps::bc_bound() |>
          sf::st_transform(4326),
        col = 'black',
        weight = 2,
        fillColor = 'transparent'
      ) |>
      leaflet.extras::addResetMapButton()
  })

  observe({
    req(!is.null(input$ais_rangemap_sp))
    # Once a species is selected, add to map
    l = leaflet::leafletProxy('ais_rangemap_leaf') |>
      leaflet::clearGroup(group = 'selected_species_circles') |>
      leaflet::clearGroup(group = 'selected_species_buffer') |>
      leaflet::removeControl('selected_species_legend') |>
      leaflet::addCircleMarkers(
        data = occ_dat_sp(),
        color = ~occ_dat_pal()(Species),
        fillColor = ~occ_dat_pal()(Species),
        label = ~Species,
        group = 'selected_species_circles'
      ) |>
      leaflet::addLegend(
        pal = occ_dat_pal(),
        values = occ_dat_sp()$Species,
        layerId = 'selected_species_legend',
        title = 'Invasive Aquatic Species'
      )

    # If a buffer has been made, add to map.
    if(!is.null(occ_dat_buffer())){
      l = l |>
        clearGroup('selected_species_buffer') |>
        addPolygons(
          data = occ_dat_buffer(),
          color = ~occ_dat_pal()(Species),
          fillColor = ~occ_dat_pal()(Species),
          label = ~Species,
          group = 'selected_species_buffer'
        )
    }
    # If a raster has been made, add to map.
    if(!is.null(occ_dat_raster())){
      l = l |>
        clearGroup('selected_species_raster') |>
        leaflet::removeControl('selected_species_raster_legend') |>
        addRasterImage(
          x = occ_dat_raster(),
          group = 'selected_species_raster'
        ) |>
        addLegend(pal = colorNumeric('Spectral',values(occ_dat_raster())),
                  values = values(occ_dat_raster()),
                  layerId = 'selected_species_raster_legend')
    }
    # Print out map
    l
  })

  occ_dat_buffer = reactiveVal()
  occ_dat_raster = reactiveVal()

  # Button that generates buffer around points.
  observeEvent(input$make_ais_buffer, {

    req(nrow(occ_dat_sp()) > 0)

    species_to_buffer = unique(occ_dat_sp()$Species)

    # Buffer x kilometers around the selected species.
    result = sf::st_buffer(occ_dat_sp() |>
                             dplyr::filter(Species %in% species_to_buffer) |>
                             dplyr::group_by(Species) |>
                             dplyr::summarise(),
                  dist = 1000*input$ais_buffer_radius)

    # Pop into the reactiveVal.
    occ_dat_buffer(result)

  })

  # Download for AIS rangemap buffer polygon
  output$rangemap_dl = downloadHandler(
    filename = function() {
      paste0(paste0(unique(occ_dat_sp()$Species),collapse='_'),'range_polygon.gpkg')
    },
    content = function(file) {
        sf::write_sf(occ_dat_buffer(), file)
    }
  )

  # Button that generates raster heatmap.
  observeEvent(input$make_ais_heatmap_raster, {

    req(nrow(occ_dat_sp()) > 0)

    # make blank raster of bc extent with user's chosen resolution
    blank_rast = terra::rast(terra::vect(bc), res = 1000000 * input$ais_raster_heatmap_res)
    blank_rast$val = 1
    blank_rast = terra::project(blank_rast, terra::crs('WGS84'))
    blank_rast = terra::mask(blank_rast, terra::vect(sf::st_transform(bc,4326)))

    filled_rast = terra::rasterize(terra::vect(occ_dat_sp()),
                                   blank_rast,
                                   fun = length)

    # Pop into the reactiveVal.
    occ_dat_raster(filled_rast)
  })

  # Download for AIS rangemap raster
  output$heatmap_dl = downloadHandler(
    filename = function() {
      paste0(paste0(unique(occ_dat_sp()$Species),collapse='_'),'range_raster.tif')
    },
    content = function(file) {
      terra::writeRaster(occ_dat_raster(), file)
    }
  )

#   # # # Prioritization model # # #
#
  occ_dat_for_p_model = spec_search_Server('p_model_layer')

  # As the user proceeds through each step of the prioritization model,
  # use a reactive to keep track of which step is the one to focus on.
  p_model_current_step = reactiveVal('focal_species_input_card')

  all_card_ids = c('focal_species_input_card',
                   'geog_units_input_card',
                   'risk_layers_input_card',
                   'run_model_card')
  observe({
    lapply(
      all_card_ids,
      \(x) shinyjs::removeClass(id = x, class = 'rainbow-btn')
    )
    shinyjs::addClass(id = p_model_current_step(),
                      class = 'rainbow-btn')
  })

  output$occ_data_results_text = renderText({
    if(sf::st_drop_geometry(occ_dat_for_p_model()[1,1]) == 'No search run yet.'){
      'No records to add yet.'
    } else {
      paste0(nrow(occ_dat_for_p_model()), ' records to add')
    }
  })

  # shinyjs::removeClass('add_occ_data_to_p_model', c('btn','btn-default','unhooked-btn'))
  # shinyjs::addClass('rainbow-btn', 'rainbow-btn')

  p_model = pmodel$new()

  # This isn't working... try here for solution:
  # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui
  observeEvent(input$add_occ_data_to_p_model, {
    # Make sure user has searched successfully for a species before clicking button.
    req(!sf::st_drop_geometry(occ_dat_for_p_model()[1,1]) == 'No search run yet.')

    p_model$add(role = 'occurrence', occ_dat_for_p_model())

    print_p_model_console_feedback()

    # Update which card has highlighted border
    p_model_current_step('geog_units_input_card')
    # withCallingHandlers({
    #   shinyjs::html("p_model_feedback", "")
    #   p_model$print()
    # },
    # message = function(m) {
    #   shinyjs::html(id = "p_model_feedback", html = m$message, add = TRUE)
    # })

    # bslib::accordion_panel_close(id = 'p_model_accordion', values = 'occ_dat_accordion')
    # bslib::accordion_panel_open(id = 'p_model_accordion', values = 'area_of_int_accordion')
  })

  output$geog_unit_upload_ui = renderUI({
    req(input$geog_units_presets == 'custom_file')
    fileInput('custom_geog_units',
              label = 'Upload File (.gpkg, .zip)',
              accept = c('gpkg','.zip'))
  })

  geog_units_choice = reactive({
    # Preset geographic units selection logic.
    if(input$geog_units_presets == 'nr_regs'){
      output = bcmaps::nr_regions()
    }
    if(input$geog_units_presets == 'ecosecs'){
      output = bcmaps::ecosections()
    }
    # Custom geographic units file upload.
    if(input$geog_units_presets == 'custom_file'){
      req(!is.null(input$custom_geog_units))
      output = map2(
        input$risk_layers_input$name,
        input$risk_layers_input$datapath, ~ {
          if(stringr::str_detect(.x,'.zip$')){
            dat_unzip = unzip(.y)
            dat = sf::read_sf(dat_unzip[stringr::str_detect(dat_unzip,'.shp')])
          } else {
            dat = sf::read_sf(.y)
          }
          # Confirm same projection system as geographic units of interest.
          dat |> sf::st_transform(crs = sf::st_crs(occ_dat_for_p_model()))
        })

      # name elements in list.
      names(output) = input$risk_layers_input$name
    }
  output |> sf::st_transform(crs = sf::st_crs(occ_dat_for_p_model()))
  })

  observeEvent(input$add_geog_units_to_p_model, {
    p_model$add(role = 'geog_units', geog_units_choice())

    print_p_model_console_feedback()

    # Update which card has highlighted border.
    p_model_current_step('risk_layers_input_card')
  })

  risk_layers = reactive({
    output = map2(input$risk_layers_input$name,
         input$risk_layers_input$datapath, ~ {
           if(stringr::str_detect(.x,'.zip$')){
             dat_unzip = unzip(.y)
             dat = sf::read_sf(dat_unzip[stringr::str_detect(dat_unzip,'.shp')])
           } else {
             dat = sf::read_sf(.y)
           }
           # Confirm same projection system as geographic units of interest.
           dat |> sf::st_transform(crs = sf::st_crs(occ_dat_for_p_model()))
         })

    # name elements in list.
    names(output) = input$risk_layers_input$name
    output
  })

  output$risk_layer_weights_ui <- renderUI({
    req(length(risk_layers()) != 0)

    n <- length(risk_layers())
    name_titles = stringr::str_to_title(stringr::str_replace_all(names(risk_layers()),"_"," "))

    numericInputs <- lapply(1:n, function(i) {
      numericInput(paste0("risk_layer_weight_",i),
                   label = paste0(name_titles[i]," Weight"),
                   min = 0, max = 5,
                   value = 1)
    })
    do.call(tagList, numericInputs)
  })

  risk_layer_weight_inputs = reactive({
    req(length(risk_layers()) != 0)
    # Get number of risk layers uploaded.
    n <- length(risk_layers())
    # Concatenate the risk layer weight inputs.
    as.numeric(paste0(lapply(1:n, function(i) {
      input[[paste0("risk_layer_weight_", i)]]
    })))
  })

observeEvent(input$add_risk_layers_to_p_model, {
  req(!is.null(risk_layers()))
  p_model$add(role = 'risk', risk_layers())
  p_model$add(role = 'weights', risk_layer_weight_inputs())

  print_p_model_console_feedback()

  # Update which card has highlighted border.
  p_model_current_step('run_model_card')
  })

  p_model_output = reactiveVal()

  observeEvent(input$run_p_model, {
    p_model_output(p_model$run())
  })

  p_model_output_DT = DT::renderDT({
    p_model_output()
  })

  p_model_feedback = reactiveVal()

  observeEvent(
    c(input$add_occ_data_to_p_model,
      input$add_geog_units_to_p_model,
      input$add_risk_layers_to_p_model), {
        p_model_feedback(p_model$print())
      })

  output$p_model_feedback = shiny::renderText({
    # p_model$print()
    p_model_feedback()
  })

  # To NOT trigger the shinyjs toggle on app launch, cache whether or
  # not we are in the 'initial_load'
  initial_load = reactiveVal(T)

  observeEvent(input$show_coord_map, {
    if(!initial_load()){
      shinyjs::toggle(id = 'coord_finder_panel_div',anim = T,
                      animType = 'slide')
    }
    initial_load(F) # Update value of 'initial_load' to be FALSE,
    # which allows the above toggle to work upon click.
  })

  # The below version works, but doesn't slide out pleasantly.
  # output$coord_finder_panel = renderUI({
  #   req(input$show_coord_map)
  #     absolutePanel(
  #       top = '100px',
  #       right = '0px',
  #       width = 400,
  #       height = 400,
  #       card(
  #       p("(Click on the map to grab latitude / longitude)"),
  #       leafletOutput('wb_finder_map'),
  #       style = 'background-color: white;z-index:100;'
  #       )
  #     )
  # })
}
