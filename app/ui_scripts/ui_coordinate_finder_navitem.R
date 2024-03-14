coordinate_finder_navitem = nav_item(
  fluidRow(
    column(width = 8,
           fluidRow(
             column(width = 6,
                    p('Coordinate Finder Map',
                      style = 'margin-right:-1rem;')
             ),
             column(width = 2,
                    shinyWidgets::switchInput(
                      'show_coord_map',
                      onLabel = 'Show',
                      offLabel = 'Hide'
                    )
             ),
             column(width = 4),
             style = 'padding:5px;'
           )
    ),
    style = 'justify-content:right;'
  ),
  # uiOutput('coord_finder_panel'),
  absolutePanel(
    id = 'coord_finder_panel_div',
    style = 'display:none;',
    top = '100px',
    right = '0px',
    width = 400,
    height = 400,
    card(
      p("(Click on the map to grab latitude / longitude)"),
      leafletOutput('wb_finder_map'),
      style = 'background-color: white;z-index:100;'
    )
  )
)
