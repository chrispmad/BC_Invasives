# =======================
#  Static Calculations
# =======================

section_title_margin = 'margin-bottom:-1.5rem;'
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

# Add groups of species, like Asian Carp
Asian_carp_n = sum(pr_sp[pr_sp$name %in% c("Bighead carp","Black carp","Grass carp","Silver carp"),]$n)

pr_sp = pr_sp |>
  dplyr::add_row(group = 'Fish', status = 'Prevent', name = 'Asian carp',
                 genus = 'Family: Cyprinidae', species = 'sp.', n = Asian_carp_n,
                 label = paste0('Asian carp (',Asian_carp_n,' records)')) |>
  dplyr::arrange(name)

# =======================
#  Render UI for AIS Rangemap
# =======================

# Render UI to be EITHER a search by species name OR a search by waterbody location.
output$search_ui_output = renderUI({

  if(input$search_type_input){
    tagList(
      h5("Select Species", style = section_title_margin),
      shinyWidgets::pickerInput('ais_rangemap_sp',
                                '',
                                choices = c('None'),
                                multiple = T,
                                options = list(
                                  `live-search` = TRUE)),
      h5("Select Region", style = section_title_margin),
      shinyWidgets::pickerInput('ais_rangemap_reg',
                                '',
                                choices = c('None'),
                                multiple = F,
                                options = list(
                                  `live-search` = TRUE
                                )),
      h5("Rangemap Buffer"),
      bslib::layout_column_wrap(
        1/2,
        actionButton('make_ais_buffer',
                     'Apply buffer!'),
        div(
          numericInput('ais_buffer_radius',
                       'Buffer radius (km)',
                       min = 0.01,
                       max = 500,
                       value = 10),
          style = 'margin-top:-2rem;position:relative;'
        ),
      ),
      h5("Raster Heatmap", style = 'margin-top:-0.5rem;'),
      bslib::layout_column_wrap(
        1/2,
        actionButton('make_ais_heatmap_raster',
                     'Make heatmap!'),
        div(
          numericInput('ais_raster_heatmap_res',
                       'Pixel resolution (km^2)',
                       min = 0.001,
                       max = 0.1,
                       step = 0.005,
                       value = 0.01),
          style = 'margin-top:-2rem;position:relative;'
        )
      ),
      actionButton('reset_buffer_raster',"Reset Buffer / Raster")
    )
  } else {
    tagList(
      h5("Search Waterbody for all species", style = 'margin-bottom:-2rem;text-align:center;'),
      shinyWidgets::pickerInput('all_sp_in_wb_wb_name',
                                '',
                                choices = c('None'),
                                multiple = F,
                                options = list(
                                  `live-search` = TRUE
                                )),
      bslib::layout_column_wrap(
        width = 1/2,
        textInput('all_sp_in_wb_lat','Latitude',placeholder = '(e.g. 51.2)',value = NULL),
        textInput('all_sp_in_wb_lng','Longitude', placeholder = '(e.g. -121.2)', value = NULL)
      ),
      numericInput('waterbody_poly_buffer_size','Expand waterbody polygon by margin (m)', value = 50, min = 0, max = 1000, step = 25),
      actionButton('search_for_all_sp_in_wb', 'Search!', width = '100%')
    )
  }
})

output$date_filter_of_inspection_records = renderUI({

  req(!is.null(occ_dat_sp()) | 'results' %in% names(all_sp_in_wbs()))

  if(!is.null(occ_dat_sp())){
    occ_dat = occ_dat_sp()
  }
  if(!'results' %in% names(all_sp_in_wbs())){
    occ_dat = all_sp_in_wbs()
  }

  occ_dat = occ_dat |>
    dplyr::mutate(Date = ifelse(!stringr::str_detect(Date,"-"),paste0(Date,"-01-01"),Date)) |>
    dplyr::mutate(Date = lubridate::ymd(Date))

  min_date = min(occ_dat$Date, na.rm=T)
  max_date = max(occ_dat$Date, na.rm=T)

  if(min_date != Inf & max_date != -Inf){
    return(
      sliderInput(inputId = 'date_filter_for_occ_dat',
                  'Date Filter',
                  min = min_date,
                  max = max_date,
                  value = c(min_date, max_date))
    )
  } else {
    return(NULL)
  }
})

output$all_sp_in_wb_enum = renderText({
  paste0("The search returned ",nrow(all_sp_in_wbs())," results.")
})

# Call all the leaflet map code
source('server_scripts/ais_rangemap_leaflet_code.R', local = TRUE)$value

# =======================
#  Reactives for AIS Rangemap Tab
# =======================

dat_in_pane = reactive({
  req(input$map_pane_filter_sel)
  leafc = input$ais_rangemap_leaf_bounds
  leaf_pane = sf::st_as_sf(data.frame(lat = as.numeric(leafc[c(1,3)]),
                                      lng = as.numeric(leafc[c(2,4)])),
                           coords = c("lng","lat"),
                           crs = 4326) |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  rows_to_display = sapply(sf::st_intersects(plot_dat(), leaf_pane), \(x) length(x) != 0)

  return(rows_to_display)
})

searched_species = reactive({
  unique(plot_dat()$Species)
})

all_sp_in_wbs = reactiveVal(data.frame(results = 'No search run yet.'))

selected_species = reactive({
  stringr::str_squish(stringr::str_remove(input$ais_rangemap_sp, ' \\(.*'))
})

# Make one dataset name that gets informed by EITHER the dropdown
# menu of species or natural resource region,
# OR by the waterbody search.
plot_dat = reactiveVal()

occ_dat_buffer = reactiveVal()
occ_dat_raster = reactiveVal()

last_clicked_resource = reactiveVal(NA)
ais_selected_rows = reactiveVal(NA)
leafmap_clicked_lat = reactiveVal(NA)
leafmap_clicked_lng = reactiveVal(NA)

# =======================
#  Data Table for AIS Rangemap
# =======================

output$records_as_table = DT::renderDT({

  dat = plot_dat()

  if(!is.null(dat)){
    dat = dat |>
      dplyr::mutate(rows_to_keep = TRUE)

    # Check to see if the user wants to filter datatable
    # records by the map bounds.
    if(input$map_pane_filter_sel){
      dat = dat |>
        dplyr::mutate(rows_to_keep = dat_in_pane())
    }

    # Show datatable.
    DT::datatable(
      dat |>
        dplyr::filter(rows_to_keep) |>
        dplyr::select(-rows_to_keep) |>
        sf::st_drop_geometry()
    )
  }
})

output$all_sp_in_wb_results = DT::renderDT({
  DT::datatable(
    all_sp_in_wbs()
  )
})

# =======================
#  Downloads for AIS Rangemap
# =======================

output$all_recs_of_sp_dl = downloadHandler(
  filename = function() {
    paste0('All_records_of_',searched_species(),'.xlsx')
  },
  content = function(file) {
    output = plot_dat() |>
      dplyr::mutate(latitude = sf::st_coordinates(geometry)[,2],
                    longitude = sf::st_coordinates(geometry)[,1]) |>
      sf::st_drop_geometry()
    openxlsx::write.xlsx(output, file)
  }
)

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

# Download for AIS occurrence data as excel file for selected species
output$records_excel = downloadHandler(
  filename = function() {
    paste0(paste0(unique(plot_dat()$Species),collapse='_'),'occurrence_data.xlsx')
  },
  content = function(file) {
    openxlsx::write.xlsx(plot_dat() |> sf::st_drop_geometry(), file)
  }
)

# Download for AIS occurrence data as geopackage for selected species
output$records_gpkg = downloadHandler(
  filename = function() {
    paste0(paste0(unique(plot_dat()$Species),collapse='_'),'occurrence_data.gpkg')
  },
  content = function(file) {
    openxlsx::write.xlsx(occ_dat_buffer(), file)
  }
)

# Download for AIS rangemap buffer polygon
output$rangemap_dl = downloadHandler(
  filename = function() {
    paste0(paste0(unique(plot_dat()$Species),collapse='_'),'range_polygon.gpkg')
  },
  content = function(file) {
    sf::write_sf(occ_dat_buffer(), file)
  }
)

# Download for AIS rangemap raster
output$heatmap_dl = downloadHandler(
  filename = function() {
    paste0(paste0(unique(plot_dat()$Species),collapse='_'),'range_raster.tif')
  },
  content = function(file) {
    terra::writeRaster(occ_dat_raster(), file)
  }
)

# # # All Species in 1 Waterbody # # #

# =======================
#  Observers and Reactions
# =======================

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

      if(nrow(all_species_results) == 1){
        if(all_species_results == data.frame(result = "No records found for target waterbody")){
          all_sp_in_wbs(data.frame(results = 'No search run yet.'))
        }
      } else {
        # Filter these results for species in our list of target AIS species.
        target_species_results = all_species_results |>
          dplyr::mutate(Species = stringr::str_to_sentence(Species)) |>
          dplyr::filter(Species %in% stringr::str_to_sentence(pr_sp$name))

        all_sp_in_wbs(target_species_results)
      }
    })
})

# Update species selector
shinyWidgets::updatePickerInput(session = session,
                                inputId = 'ais_rangemap_sp',
                                choices = pr_sp$label,
                                selected = pr_sp$label[1])

# Inform choices for natural resource selector
shinyWidgets::updatePickerInput(session = session,
                                inputId = 'ais_rangemap_reg',
                                choices = c('None',nr_regs$reg_name),
                                selected = 'None')

# Update species selector and region selector
# every time the toggle is switched back to species search.
observeEvent(input$search_type_input, {
  if(input$search_type_input){
    shinyWidgets::updatePickerInput(session = session,
                                    inputId = 'ais_rangemap_sp',
                                    choices = pr_sp$label,
                                    selected = pr_sp$label[1])
    shinyWidgets::updatePickerInput(session = session,
                                    inputId = 'ais_rangemap_reg',
                                    choices = c('None',nr_regs$reg_name),
                                    selected = 'None')
  }
})

# Update natural resource region selector to 'None' whenever a species
# has been selected by name.
observe({
  if(!is.null(input$ais_rangemap_sp)){
    if(!'None' %in% input$ais_rangemap_sp){
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = 'ais_rangemap_reg',
                                      selected = 'None')
    }
  }
})

# Update species selector to 'None' whenever a region
# has been selected by name.
observe({
  if(!is.null(input$ais_rangemap_reg)){
    if(input$ais_rangemap_reg != 'None'){
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = 'ais_rangemap_sp',
                                      # selected = 'None')
                                      selected = NULL)
    }
  }
})

# When the search type is 'all species in waterbody', update the options
# availabe from the waterbody name coordinate lookup table.
# observe(
#   if(!input$search_type_input){
#
# }
observe({
  if(!input$search_type_input){
    shinyWidgets::updatePickerInput(
      inputId = "all_sp_in_wb_wb_name",
      choices = c("None",paste0(lk_crd_tbl$wb_name)),
      selected = "None"
    )
  }
})

selected_wb_from_tbl = reactiveVal("None")
# If the user picks one of the waterbodies by name in the dropdown,
# update the lat and long for the 'all_sp_in_wb' search
observe({
  if(!input$search_type_input){
    if(!is.null(input$all_sp_in_wb_wb_name)){
      if(input$all_sp_in_wb_wb_name != "None"){

        if(input$all_sp_in_wb_wb_name != selected_wb_from_tbl()){
          # Check that the clicked coordinates field would change based on this wb selection.
          # If not, then don't make Shiny do this multiple times
          wb_for_coords = lk_crd_tbl[lk_crd_tbl$wb_name == input$all_sp_in_wb_wb_name,]
          selected_wb_from_tbl(wb_for_coords$wb_name)
          update_coords = F
          if(is.null(clicked_lng())) update_coords = TRUE
          if(!is.null(clicked_lng())){
            if(clicked_lng() != wb_for_coords$longitude){
              update_coords = T
            }
          }
          if(!is.null(clicked_lat())){
            if(clicked_lat() != wb_for_coords$latitude){
              update_coords = T
            }
          }
          if(update_coords){
            clicked_lng(wb_for_coords$longitude)
            updateTextInput(session = session,
                            inputId = 'all_sp_in_wb_lng',
                            value = clicked_lng())
            # print(paste0("all_sp_in_wb_lng was updated to be ",clicked_lng()))
            clicked_lat(wb_for_coords$latitude)
            updateTextInput(session = session,
                            inputId = 'all_sp_in_wb_lat',
                            value = clicked_lat())
          }
        }
      }
    }
  }
})

# Fork in pipe for which dataset to use - either species dataset or waterbody dataset
observe({
  # Make plot dat an empty dataframe if nothing's been done yet.
  if(is.null(occ_dat_sp()) & 'results' %in% names(all_sp_in_wbs())) plot_dat(data.frame())
  # We have data in occ_dat_sp reactive! If we have the filtered form, update
  # plot_dat with that.
  if(!is.null(occ_dat_sp())){
    if(!is.null(occ_dat_sp_f())){
      plot_dat(occ_dat_sp_f())
    }
  }
  # All AIS in a waterbody search has been performed! Use that to
  # update the plot_dat reactive.
  if(!'results' %in% names(all_sp_in_wbs())){
    plot_dat(all_sp_in_wbs())
  }
  nrow(plot_dat())
})

# If the user changes map type from species drop-down to waterbody,
# or vice-versa, clear out the plotting data.
observeEvent(input$search_type_input, {
  all_sp_in_wbs(data.frame(results = 'No search run yet.'))
})


# # Take not of which rows, if any, have been selected using the datatable.
# DT_selected_rows = reactive({
#   req(!is.null(input$records_as_table_rows_selected))
#   input$records_as_table_rows_selected
# })

# Call the code that generates the interactivity between the leaflet map and the datatable.
source("server_scripts/leaflet_map_and_data_table_interactions.R", local = TRUE)$value

# Button that generates buffer around points.
observeEvent(input$make_ais_buffer, {

  req(nrow(plot_dat()) > 0)

  species_to_buffer = unique(plot_dat()$Species)

  # Buffer x kilometers around the selected species.
  result = sf::st_buffer(plot_dat() |>
                           dplyr::filter(Species %in% species_to_buffer) |>
                           dplyr::group_by(Species) |>
                           dplyr::summarise(),
                         dist = 1000*input$ais_buffer_radius)

  # Pop into the reactiveVal.
  occ_dat_buffer(result)

})

# Button that generates raster heatmap.
observeEvent(input$make_ais_heatmap_raster, {

  req(nrow(plot_dat()) > 0)
  req(input$ais_raster_heatmap_res >= 0.001 & input$ais_raster_heatmap_res <= 0.1)

  # make blank raster of bc extent with user's chosen resolution
  blank_rast = terra::rast(terra::vect(bc), res = 1000000 * input$ais_raster_heatmap_res)
  blank_rast$val = 1
  blank_rast = terra::project(blank_rast, terra::crs('WGS84'))
  blank_rast = terra::mask(blank_rast, terra::vect(sf::st_transform(bc,4326)))

  filled_rast = terra::rasterize(terra::vect(plot_dat()),
                                 blank_rast,
                                 fun = length)

  # Pop into the reactiveVal.
  occ_dat_raster(filled_rast)
})

# Button that resets buffer and raster to be blank.
observeEvent(input$reset_buffer_raster, {
  occ_dat_buffer(NULL)
  occ_dat_raster(NULL)
})
