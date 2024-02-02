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
    )
  ),
  uiOutput('coord_finder_panel')
)
