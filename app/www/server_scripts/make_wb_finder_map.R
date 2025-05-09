

# Make leaflet map for finding lag/long coords
output$wb_finder_map = renderLeaflet({
  leaflet() |>
    addTiles() |>
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
