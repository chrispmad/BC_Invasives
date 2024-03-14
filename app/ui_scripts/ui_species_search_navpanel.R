all_recs_of_sp_acc = accordion_panel(
  'All Records of Select Species in Province',
  fluidRow(
    column(
      width = 4,
      spec_search_UI('sp_search_bar'),
      textOutput('all_recs_of_sp_enum'),
      downloadButton('all_recs_of_sp_dl',
                     'Download Results',
                     style = 'width:100%;')
    ),
    column(width = 8,
           card(
             DT::DTOutput('all_rec_of_sp_results')
           )
    )
  )
)

all_sp_in_wbs = accordion_panel(
  'All Species in 1 Waterbody',
  fluidRow(
    column(
      width = 4,
      tagList(
        textInput('all_sp_in_wbs_name',"Name of Waterbody", placeholder='(optional)'),
        textInput('all_sp_in_wb_lat','Latitude of a point inside Waterbody',placeholder = '(e.g. 51.2)',value = NULL),
        textInput('all_sp_in_wb_lng','Longitude of a point inside Waterbody', placeholder = '(e.g. -121.2)', value = NULL),
        numericInput('waterbody_poly_buffer_size','Expand waterbody polygon by margin (m)', value = 50, min = 0, max = 1000, step = 25),
        checkboxGroupInput('all_sp_in_wbs_sources','Sources to search',
                           choices = c("SPI","Old Aquatic","Incident Reports","iNaturalist"),
                           selected = c("SPI","Old Aquatic","Incident Reports","iNaturalist"),
                           inline = T,
                           width = '100%'),
        actionButton('search_for_all_sp_in_wb', 'Search!', width = '100%'),
        # div(
        downloadButton('all_sp_in_wb_dl', 'Download (.xlsx)',style = 'margin-top:5px;width:100%;')
        # )
      )
    ),
    column(
      width = 8,
      card(
        DT::DTOutput('all_sp_in_wb_results')
      )
    )
  )
)


species_search_navpanel = bslib::nav_panel(
  title = 'Species Search',
  bslib::accordion(
    multiple = F,
    all_recs_of_sp_acc,
    all_sp_in_wbs
  )
)
