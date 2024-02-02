connected_wb_navpanel = bslib::nav_panel(
  title = 'Connected Waterbodies',
  card(
    layout_column_wrap(
      1/2,
      div(
        textInput('wb_for_downst','Waterbody Name', placeholder = '(optional)'),
        textInput('wb_for_downst_lat','Latitude of a point inside Waterbody', placeholder = '(e.g. 51.2)'),
        textInput('wb_for_downst_lng','Longitude of a point inside Waterbody', placeholder = '(e.g. -121.2)'),
        sliderInput('radius_wb_downst', 'Radius to search around Waterbody (km)',
                    value = 10, min = 0, max = 20),
        # checkboxInput('downstream_wb_only',
        #               'Downstream Waterbodies Only?'),
        actionButton('run_wb_connectivity_search',
                     'Run Search',
                     style = 'width:100%;')),
      tabsetPanel(
        # tabPanel("Coordinate Finder",
        #          tagList(
        #            p("(Click on the map to grab latitude / longitude)"),
        #            leafletOutput('wb_finder_map')
        #          )
        # ),
        tabPanel("Visual Results",
                 plotlyOutput('waterbody_graph_as_image')
        ),
        tabPanel("Tabular Results",
                 textOutput('waterbody_graph_named_lake_list'),
                 tableOutput('waterbody_graph_as_table'))
      )
    )
  )
)
