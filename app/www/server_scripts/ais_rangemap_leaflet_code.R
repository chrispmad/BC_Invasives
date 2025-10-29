# build-in color palette
my_colours = glasbey.colors(length(unique(occ_dat$Species)))

output$ais_rangemap_leaf = renderLeaflet({
  regs = sf::read_sf("nr_regions.gpkg")
  bc_bound = sf::read_sf("bc_bound.gpkg")
  
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::clearGroup(group = 'selected_species_circles') |>
    leaflet::removeControl('selected_species_legend') |>
    leaflet::addScaleBar('bottomright') |>
    leaflet::addMapPane(name = 'region_pane', zIndex = 300) |>
    leaflet::addMapPane(name = 'occ_pane', zIndex = 400) |>
    leaflet::addMapPane(name = 'erad_pane', zIndex = 475) |>
    leaflet::addMapPane(name = 'anec_pane', zIndex = 550) |>
    leaflet::addMapPane(name = 'native_pane', zIndex = 625) |>
    leaflet::addPolygons(
      data = regs,
      color = 'grey',
      weight = 1,
      fillColor = 'transparent',
      label = ~REGION_NAME,
      group = 'Regions',
      options = pathOptions(pane = "region_pane")
    ) |>
    leaflet::addLayersControl(overlayGroups = 'Regions') |>
    leaflet.extras::addResetMapButton()
})


occ_dat_pal = reactive({
  req(!is.null(occ_dat_sp()) || !any(names(all_sp_in_wbs()) == "results"))
  
  if (!is.null(occ_dat_sp()) || !any(names(all_sp_in_wbs()) == "results")) {
    if (!is.null(occ_dat_sp())) {
      if (input$ais_rangemap_reg == 'None') {
        # Drop-down species selector used.
        leaflet::colorFactor(palette = 'Spectral',
                             domain = c("SPI","Old AIS Layer","Incidental Observations","iNaturalist"))
      } else {
        # Drop-down region selector used.
        leaflet::colorFactor(palette = 'Spectral',
                             domain = unique(plot_dat()$Species))
      }
    } else {
      # waterbody search used.
      leaflet::colorFactor(palette = 'Spectral',
                           domain = unique(plot_dat()$Species))
    }
  }
})

# EXPERIMENTAL #
# Fly in to coordinates when coordinates have been selected #
observe({
  print(clicked_lng())
  if (!is.null(clicked_lng())) {
    leaflet::leafletProxy('ais_rangemap_leaf') |>
      leaflet::flyTo(lng = clicked_lng(),
                     lat = clicked_lat(),
                     zoom = 8)
  }
  if (!is.null(input$all_sp_in_wb_wb_name)) {
    if (input$all_sp_in_wb_wb_name != "None") {
      # Wait until we find the centroid coordinates...
      req(input$all_sp_in_wb_lat != "")
      
      wb_poly = get_waterbody_polygon(
        focus_wb_name = stringr::str_remove(input$all_sp_in_wb_wb_name,"_.*$"),
        focus_wb_coordinates = c(input$all_sp_in_wb_lng,input$all_sp_in_wb_lat)
      ) |>
        sf::st_transform(4326)
      
      leaflet::leafletProxy('ais_rangemap_leaf') |>
        leaflet::clearGroup("selected_wb") |>
        leaflet::addPolygons(
          data = wb_poly,
          label = ~WATERBODY_KEY_GROUP_CODE_50K,
          group = 'selected_wb'
        )
    }
  }
})

observe({
  # If we have just switched to waterbody search mode, wipe map.
  if (is.null(occ_dat_sp()) && any(names(all_sp_in_wbs()) == "results")) {
    l = leaflet::leafletProxy('ais_rangemap_leaf') |>
      leaflet::clearGroup(group = 'selected_species_circles') |>
      leaflet::clearGroup(group = 'selected_species_buffer') |>
      leaflet::removeControl('selected_species_legend')
    
    return(l)
  }
  req(!is.null(occ_dat_sp()) || !any(names(all_sp_in_wbs()) == "results"))
  
  # Once a species is selected, add to map
  l = leaflet::leafletProxy('ais_rangemap_leaf') |>
    leaflet::clearGroup(group = 'selected_species_circles') |>
    leaflet::clearGroup(group = 'selected_species_buffer') |>
    leaflet::clearGroup(group = 'native_range_markers') |>
    leaflet::clearGroup(group = 'eradication_markers') |>
    leaflet::clearGroup(group = 'anecdotal_markers') |>
    leaflet::removeControl('selected_species_legend') |>
    leaflet::removeControl('custom_legend')
  
  req(!is.null(plot_dat()))
  if (nrow(plot_dat()) > 0 || !any(names(all_sp_in_wbs()) == "results")) {
    
    if (any(names(all_sp_in_wbs()) == "results")) {
      req('geom' %in% names(plot_dat()) || 'geometry' %in% names(plot_dat()))
    }
    
    ## FIX: safely handle selected_species
    selected_species <- NULL
    
    # Only set selected_species if a species is actually selected
    if (!is.null(input$ais_rangemap_sp) && length(input$ais_rangemap_sp) > 0) {
      # Convert to TRUE/FALSE for isTRUE() logic
      has_selection <- length(input$ais_rangemap_sp) > 0
      if (isTRUE(has_selection)) {
        selected_species <- stringr::str_remove_all(input$ais_rangemap_sp, " \\(.*")
      }
    }
    
    l = leaflet::leafletProxy('ais_rangemap_leaf') |>
      leaflet::clearGroup(group = 'selected_species_circles') |>
      leaflet::clearGroup(group = 'selected_species_buffer') |>
      leaflet::clearGroup(group = 'native_range_markers') |>
      leaflet::clearGroup(group = 'eradication_markers') |>
      leaflet::clearGroup(group = 'anecdotal_markers') |>
      leaflet::removeControl('selected_species_legend') |>
      leaflet::removeControl('custom_legend')
    
    # if species search, filter; if waterbody search, keep all
    if (!is.null(selected_species) && length(selected_species) > 0) {
      dat <- plot_dat() |>
        dplyr::filter(Species %in% selected_species) |>
        dplyr::mutate(rows_to_keep = TRUE)
    } else {
      dat <- plot_dat() |> dplyr::mutate(rows_to_keep = TRUE) 
    }
    
    # Check to see if the user wants to filter datatable records by the map bounds.
    if (input$map_pane_filter_sel) {
      dat = dat |>
        dplyr::mutate(rows_to_keep = dat_in_pane()) |>
        dplyr::filter(rows_to_keep)
    }
    
    popup_content = leafpop::popupTable(
      if (sum(dat$rows_to_keep) == 0) {
        return(NULL)
      } else {
        dat |>
          dplyr::select(-rows_to_keep) |>
          sf::st_drop_geometry()
      }
    )
    
    # Color logic
    if (!is.null(occ_dat_sp()) && input$search_type_input) {
      if (input$ais_rangemap_reg == 'None') {
        dat_for_plot = dat |> dplyr::mutate(marker_colour = DataSource)
      } else {
        dat_for_plot = dat |> dplyr::mutate(marker_colour = Species)
      }
    }
    if (!is.null(input$all_sp_in_wb_wb_name)) {
      if (!any(names(all_sp_in_wbs()) == "results")) {
        dat_for_plot = dat |> dplyr::mutate(marker_colour = Species)
      }
    }
    
    # add circles
    l = l |>
      leaflet::addCircleMarkers(
        data = dat_for_plot,
        fillColor = ~occ_dat_pal()(marker_colour),
        fillOpacity = 0.65,
        color = 'black',
        weight = 1,
        label = ~paste0(Species,":",Date),
        popup = lapply(popup_content, htmltools::HTML),
        options = markerOptions(riseOnHover = T),
        group = 'selected_species_circles'
      ) |>
      leaflet::addLegend(
        pal = occ_dat_pal(),
        values = dat_for_plot$marker_colour,
        layerId = 'selected_species_legend',
        title = 'Record'
      )
    
    ## Other occurrence types
    eradicated_to_plot = eradicated_occs |>
      dplyr::mutate(Date = ifelse(!stringr::str_detect(Date,"-"),paste0(Date,"-01-01"),Date)) |>
      dplyr::mutate(Date = suppressWarnings(lubridate::ymd(Date, quiet = TRUE)))
    
    native_to_plot = native_range_occs |>
      dplyr::mutate(Date = ifelse(!stringr::str_detect(Date,"-"),paste0(Date,"-01-01"),Date)) |>
      dplyr::mutate(Date = suppressWarnings(lubridate::ymd(Date, quiet = TRUE)))
    
    anecdotal_to_plot = anecdotal_occs |>
      dplyr::mutate(Date = ifelse(!stringr::str_detect(Date,"-"),paste0(Date,"-01-01"),Date)) |>
      dplyr::mutate(Date = suppressWarnings(lubridate::ymd(Date, quiet = TRUE)))
    
    if (!is.null(selected_species) && length(selected_species) > 0) {
      eradicated_to_plot = eradicated_to_plot |> dplyr::filter(Species %in% selected_species)
      native_to_plot     = native_to_plot     |> dplyr::filter(Species %in% selected_species)
      anecdotal_to_plot  = anecdotal_to_plot  |> dplyr::filter(Species %in% selected_species)}
    
    if (!is.null(selected_dates())) {
      eradicated_to_plot = eradicated_to_plot |> 
        dplyr::filter(Date %within% lubridate::interval(start = lubridate::ymd(selected_dates()[1]),
                                                        end = lubridate::ymd(selected_dates()[2])) | is.na(Date))
      native_to_plot = native_to_plot |> 
        dplyr::filter(Date %within% lubridate::interval(start = lubridate::ymd(selected_dates()[1]),
                                                        end = lubridate::ymd(selected_dates()[2])) | is.na(Date))
      anecdotal_to_plot = anecdotal_to_plot |> 
        dplyr::filter(Date %within% lubridate::interval(start = lubridate::ymd(selected_dates()[1]),
                                                        end = lubridate::ymd(selected_dates()[2])) | is.na(Date))
    }
    
    if (nrow(eradicated_to_plot) > 0) {
      l <- l |>
        leaflet::addMarkers(
          data = eradicated_to_plot,
          icon = red_ex_icon,
          label = ~paste0(Species, ":", Date),
          popup = lapply(leafpop::popupTable(sf::st_drop_geometry(eradicated_to_plot)), shiny::HTML),
          group = 'eradication_markers',
          options = pathOptions(pane = "erad_pane")
        )
    }
    
    if (nrow(native_to_plot) > 0) {
      l <- l |>
        leaflet::addMarkers(
          data = native_to_plot,
          icon = native_range_square_icon,
          label = ~paste0(Species, ":", Date),
          popup = lapply(leafpop::popupTable(sf::st_drop_geometry(native_to_plot)), shiny::HTML),
          group = 'native_range_markers',
          options = pathOptions(pane = "native_pane")
        )
    }
    
    if (nrow(anecdotal_to_plot) > 0) {
      l <- l |>
        leaflet::addMarkers(
          data = anecdotal_to_plot,
          icon = anecdotal_question_icon,
          label = ~paste0(Species, ":", Date),
          popup = lapply(leafpop::popupTable(sf::st_drop_geometry(anecdotal_to_plot)), shiny::HTML),
          group = 'anecdotal_markers',
          options = pathOptions(pane = "anec_pane")
        )
    }
    
    if (nrow(eradicated_to_plot) > 0 || nrow(native_to_plot) > 0 || nrow(anecdotal_to_plot) > 0) {
      l = l |> addControl(position = "topright", html = legend_html, layerId = 'custom_legend')
    }
    
    # Drop any highlighted rows
    l = l |> clearGroup('highlighted_rows')
    
    rows <- tryCatch(ais_selected_rows(), error = function(e) NULL)
    if (is.numeric(rows) && length(rows) > 0 && all(!is.na(rows)) && all(rows <= nrow(dat))) {
      dat = plot_dat() |> dplyr::mutate(rows_to_keep = TRUE)
      if (input$map_pane_filter_sel) {
        dat = dat |> dplyr::mutate(rows_to_keep = dat_in_pane())
      }
      l = l |>
        leaflet::addCircleMarkers(
          data = dat[ais_selected_rows(),],
          color = 'gold',
          fillColor = 'gold',
          fillOpacity = 0.85,
          weight = 1.2,
          group = 'highlighted_rows',
          options = pathOptions(interactive = FALSE)
        )
    }
  }
  
  # Buffers
  if (!is.null(occ_dat_buffer())) {
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
  # Rasters
  if (!is.null(occ_dat_raster())) {
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
  if (is.null(occ_dat_buffer()) && is.null(occ_dat_raster())) {
    l = l |>
      clearGroup('selected_species_buffer') |>
      clearGroup('selected_species_raster') |>
      removeControl('selected_species_raster_legend')
  }
  l
})
