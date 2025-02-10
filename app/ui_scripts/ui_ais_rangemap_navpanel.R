button_bar = card(
  card_body(
    div(
    shinyWidgets::switchInput(
      inputId = 'search_type_input',
      label = 'Search Type',
      onLabel = 'Species',
      offLabel = 'Waterbody',
      value = T),
    style = "width:100%;"
    ),
    uiOutput('search_ui_output'),
    uiOutput('date_filter_of_inspection_records'),
    checkboxInput('map_pane_filter_sel',label="Filter Table with Map"),
    h5("Sources to Include", style = 'margin-bottom:-1rem;'),
    checkboxGroupInput('all_sp_in_wbs_sources',
                       label = '',
                       choices = c("FDIS","Old AIS Layer","Incidental Observations","iNaturalist"),
                       selected = c("FDIS","Old AIS Layer","Incidental Observations","iNaturalist"),
                       inline = T,
                       width = '100%'),
    h5("Filters", style = 'margin-top:-0.5rem;margin-bottom:0rem;'),
    checkboxInput('retain_unconfirmed_reports','Include Unconfirmed Reports?',
                  value = F),
    h5("Downloads", style = 'margin-top:-0.5rem;text-align:center;margin-bottom:0rem;'),
    p("Occurrence Data",style = 'margin-bottom:-0.5rem;'),
    layout_column_wrap(
      1/2,
      downloadButton('records_excel', 'Excel'),
      downloadButton('records_gpkg', 'Spatial')
    ),
    p("Range Polygon / Raster", style = 'margin-bottom:-0.5rem;margin-top:1.5rem;'),
    layout_column_wrap(
      1/2,
      downloadButton(outputId = 'rangemap_dl', 'Polygon'),
      downloadButton(outputId = 'heatmap_dl', 'Raster')
    ),
    div(
      capture::capture(
        selector = "body",
        filename = paste0("Rangemap_",Sys.Date(),"_screenshot.png"),
        icon("camera"), "Screen Capture",
        style = 'padding:10px;display:grid;height:8vh;margin-top:1.25rem;',
        class = "btn-info"
      ),
      style = 'display:grid;'
    ),
    style = 'overflow-x: hidden;'
  )
)

map_column = bslib::card(
  leafletOutput('ais_rangemap_leaf', height = '80vh'),
  full_screen = T
)

tbl_column =
  # bslib::card(
  # bslib::card_body(
  DT::DTOutput('records_as_table', height = '100%', width = '100%')#,
# style = "margin-top:1rem !important;"
# ),
# style = "top:-3rem; left:-1rem;"
# )

AIS_rangemap_navpanel = bslib::nav_panel(
  title = 'AIS Rangemaps',
  bslib::layout_columns(
    col_widths = c(3,9),
    button_bar,
    card(
      layout_sidebar(
        map_column,
        sidebar = sidebar(
          tbl_column,
          width = '50%',
          position = 'right'
        )
      )
    )
  )
)
