AIS_rangemap_navpanel = bslib::nav_panel(
  title = 'AIS Rangemaps',
  fluidRow(
    column(
      width = 2,
      h5("Select Species"),
      shinyWidgets::pickerInput('ais_rangemap_sp',
                                '',
                                choices = c('None'),
                                multiple = T),
      h5("Rangemap Buffer"),
      bslib::layout_column_wrap(
        1/2,
        actionButton('make_ais_buffer',
                     'Apply buffer!'),
        numericInput('ais_buffer_radius',
                     'Buffer radius (km)',
                     min = 0.01,
                     max = 500,
                     value = 10)
      ),
      h5("Raster Heatmap"),
      bslib::layout_column_wrap(
        1/2,
        actionButton('make_ais_heatmap_raster',
                     'Make heatmap!'),
        numericInput('ais_raster_heatmap_res',
                     'Pixel resolution (km^2)',
                     min = 0.001,
                     max = 0.1,
                     step = 0.005,
                     value = 0.01)
      ),
      h5("Downloads"),
      layout_column_wrap(
        1/3,
        capture::capture(
          selector = "body",
          filename = paste0("Rangemap_",Sys.Date(),"_screenshot.png"),
          icon("camera"), "Capture",
          style = 'padding:10px;display:grid;height:10vh;',
          class = "btn-info"
        ),
        downloadButton(outputId = 'rangemap_dl', 'Polygon'),
        downloadButton(outputId = 'heatmap_dl', 'Raster')
      )
    ),
    column(
      width = 10,
      leafletOutput('ais_rangemap_leaf', height = '88vh')
    )
  )
)
