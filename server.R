function(input, output, session) {
  # Update bcinvadeR package, if necessary!

  # Probably unnecessary once the app is published...
  if(!stringr::str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
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
  # Get All Records of Species
  all_rec_of_sp = reactiveVal()

  names_of_sp_to_search_for = reactive({
    stringr::str_split(input$all_rec_sp_name, pattern = ', ') |>
      unlist()
  })
  # all_rec_of_sp = reactive({
  observeEvent(input$search_for_all_rec_sp, {


    withProgress(
      message = "Starting search...",
      detail = 'Testing BC Data Catalogue connection',
      value = 1/3, expr = {

        test_search = tryCatch(
          expr = bcdata::bcdc_query_geodata('aca81811-4b08-4382-9af7-204e0b9d2448') |>
            bcdata::filter(SPECIES_NAME == 'goldfish') |>
            bcdata::collect(),
          error = function(e) NULL)

        if(!is.null(nrow(test_search))){
          incProgress(amount = 1/3, message = 'Connection to BC Data Catalogue made')
          cat("BC Data Catalogue connection successful.")
        }

        occ_dat = tryCatch(
          expr = bcinvadeR::grab_aq_occ_data(
          common_names = names_of_sp_to_search_for(),
          sources = input$all_rec_sp_sources,
          excel_path = 'Master Incidence Report Records.xlsx',
          quiet = F),
          error = function(e) cat("Unfortunately, there was an error."))
        incProgress(amount = 1/3, message = 'Species search complete')
      })

    all_rec_of_sp(occ_dat)

  })

  output$all_rec_of_sp_results = DT::renderDT({
    head(all_rec_of_sp())
  })

  output$all_rec_of_sp_res_dl = downloadHandler({
    filename = function() {
      paste0('All_recs_of_',names_of_sp_to_search_for())
    }
  })
  # All Species in 1+ Waterbodies
  all_sp_in_wbs = reactiveVal()

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

  output$all_sp_in_wb_results = DT::renderDT({
    all_sp_in_wbs()
  })

  # Make leaflet map for finding lag/long coords
  output$wb_finder_map = renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(
        weight = 2,
        color = 'black',
        data = bc
      ) |>
      leaflet.extras::addResetMapButton()
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

  # Prioritization model
  p_model = reactive({
    pmodel$new()
  })

  output$p_model_feedback = shiny::renderText({
    p_model()
  })
}
