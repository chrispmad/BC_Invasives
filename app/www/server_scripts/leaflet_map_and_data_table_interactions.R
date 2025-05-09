
# Set up a reactive value to cache clicked points on the map.
# last_clicked_leaflet_marker = reactiveVal(NA)

# Click on data-table rows; highlights markers on map.
observeEvent(input$records_as_table_rows_selected, {
  ais_selected_rows(input$records_as_table_rows_selected)
  # last_clicked_resource("table")
  # print("last clicked: table")
})

# Click off all data-table rows; removes highlight from last marker on map.
observe({
  # if(is.null(input$records_as_table_rows_selected) & !is.na(last_clicked_resource())){
  if(is.null(input$records_as_table_rows_selected)){
    # last_clicked_resource('table')
    # if(last_clicked_resource() == 'table'){
      # print("NULL in datatable cleared ais_selected_rows")
      # last_clicked_leaflet_marker(NA)
      ais_selected_rows(NA)
    # }
    # last_clicked_resource(NA)
  }
})

# observeEvent(input$ais_rangemap_leaf_marker_click, {
#   req(!is.null(input$ais_rangemap_leaf_marker_click))
#   last_clicked_leaflet_marker(input$ais_rangemap_leaf_marker_click)
#   ais_selected_rows(last_clicked_leaflet_marker())
# })

# # Use the reactive cache for leaflet map points to update
# # the reactive for ais rows to plot plus which rows
# # to highlight in the datatable.
# observe({
#   # See which points are within a small radius, e.g. 20 meters, of the clicked spot.
#   req(!is.na(last_clicked_leaflet_marker()[1]) & !is.na(ais_selected_rows()))
#
#   # browser()
#   # Maybe don't update this if the last thing to be clicked was the data table?
#   # last_clicked_resource('map')
#
#   if(!is.na(last_clicked_resource())){
#     # if(last_clicked_resource() == 'table' & is.na(ais_selected_rows()[1])){
#     if(last_clicked_resource() == 'table'){
#       # browser()
#       clicked_spot_df = data.frame(lat = 0, lng = 0)
#     } else {
#       clicked_spot_df = data.frame(
#         lat = last_clicked_leaflet_marker()$lat,
#         lng = last_clicked_leaflet_marker()$lng
#       )
#     }
#   } else {
#     clicked_spot_df = data.frame(
#       lat = last_clicked_leaflet_marker()$lat,
#       lng = last_clicked_leaflet_marker()$lng
#     )
#   }
#
#   clicked_spot = sf::st_as_sf(
#     clicked_spot_df,
#     coords = c("lng","lat"),
#     crs = 4326) |>
#     sf::st_transform(3005) |>
#     sf::st_buffer(20) |>
#     sf::st_transform(4326)
#
# #   # If filtering by leaf pane, use the data in pane reactive.
# #   if(input$map_pane_filter_sel){
# #     dat = plot_dat()[dat_in_pane(),]
# #   } else {
# #     dat = plot_dat()
# #   }
#   dat = plot_dat()
#
#   overlapping_rows = as.numeric(sf::st_intersects(dat, clicked_spot))
#
#   ais_selected_rows(which(!is.na(overlapping_rows)))
#
#   last_clicked_resource('map')
#   print("last clicked: map")
#
#   # proxy <- DT::dataTableProxy("records_as_table")
#   # DT::selectRows(proxy, ais_selected_rows())
# })

# And if the user changes species entirely, reset
# the selected rows to NA!
observeEvent(input$ais_rangemap_sp, {
  ais_selected_rows(NA)
  last_clicked_resource(NA)
  proxy <- DT::dataTableProxy("records_as_table")
  DT::selectRows(proxy, ais_selected_rows())
})
