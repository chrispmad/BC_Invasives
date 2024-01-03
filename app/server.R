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


  bc = sf::read_sf('bc_shapefile.gpkg')

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

    withProgress(
      message = 'Getting waterbody polygon',
      detail = 'Combining records...',
      value = 0/5,
      expr = {

        wb_poly = get_waterbody_polygon(focus_wb_name = input$all_sp_in_wbs_name,
                                        focus_wb_coordinates = c(input$all_sp_in_wb_lng,input$all_sp_in_wb_lat))

        all_species_results = find_all_species_in_waterbody(
          wb_poly,
          in_shiny = T,
          sources = c("SPI","Old Aquatic","Incident Reports","iNaturalist"),
          exclude = c('Fungi','Plantae'),
          excel_path = 'Master Incidence Report Records.xlsx',
          sheet_name = 'Aquatic Reports',
          excel_species_var = 'Submitted_Common_Name',
          output_crs = 4326,
          quiet = T)

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
    clicked_lng(input$wb_finder_map_click$lng)
    clicked_lat(input$wb_finder_map_click$lat)
    updateTextInput(inputId = 'wb_for_downst_lng',
                    value = clicked_lng())
    updateTextInput(inputId = 'wb_for_downst_lat',
                    value = clicked_lat())
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
      results = tryCatch(
        bcinvadeR::get_connected_waterbodies(
          input$wb_for_downst,
          c(as.numeric(input$wb_for_downst_lng),
            as.numeric(input$wb_for_downst_lat)),
          in_shiny = T,
          the_session = session
        ),
        error = function(e) ("No waterbodies connected to target waterbody")
      )
    # }
    waterbody_graph(results)
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

  # # # Prioritization model # # #

  occ_dat_for_p_model = spec_search_Server('p_model_layer')

  # observe({
  #   print(occ_dat_for_p_model())
  # })

  output$occ_data_results_text = renderText({
    if(sf::st_drop_geometry(occ_dat_for_p_model()[1,1]) == 'No search run yet.'){
      'No records to add yet.'
    } else {
      paste0(nrow(occ_dat_for_p_model()), ' records to add')
    }
  })

  shinyjs::removeClass('add_occ_data_to_p_model', c('btn','btn-default','unhooked-btn'))
  shinyjs::addClass('rainbow-btn', 'rainbow-btn')

  p_model = pmodel$new()

  # This isn't working... try here for solution:
  # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui
  observeEvent(input$add_occ_data_to_p_model, {
    # Make sure user has searched successfully for a species before clicking button.
    req(!sf::st_drop_geometry(occ_dat_for_p_model()[1,1]) == 'No search run yet.')

    p_model$add(role = 'occurrence', occ_dat_for_p_model())

    print_p_model_console_feedback()
    # withCallingHandlers({
    #   shinyjs::html("p_model_feedback", "")
    #   p_model$print()
    # },
    # message = function(m) {
    #   shinyjs::html(id = "p_model_feedback", html = m$message, add = TRUE)
    # })

    bslib::accordion_panel_close(id = 'p_model_accordion', values = 'occ_dat_accordion')
    bslib::accordion_panel_open(id = 'p_model_accordion', values = 'area_of_int_accordion')
  })

  geog_unit_upload_ui = renderUI({
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
}
