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
