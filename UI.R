library(shiny)
library(tidyverse)
library(bslib)
library(devtools)
library(bcinvadeR)
library(leaflet)
library(shinyjs)

empty_table = tidyr::tibble(
  a = c(1:5),
  b = LETTERS[1:5]
  )

# devtools::install_github('chrispmad/bcinvadeR', upgrade = "always")

bcinv_theme = bs_theme(bootswatch = 'cerulean',
                       primary = "#168eb075",
                       secondary = "#48DAC6"
)

bcinv_theme = bs_add_rules(
  bcinv_theme,
  ".accordion-button:not(.collapsed){background: #168eb075;}"
  )
# bcinv_theme = bslib::bs_theme(
#   bootswatch = 'cerulean'
# )


all_recs_of_sp_acc = accordion_panel(
  'All Records of Select Species in Province',
  fluidRow(
    column(
      width = 4,
      card(
        textInput('all_rec_sp_name',"Species Common Name", placeholder = 'Species 1, Species 2, ...'),
        checkboxGroupInput('all_rec_sp_sources','Sources to search',
                           choices = c("SPI","Old Aquatic","Incident Reports","iNaturalist"),
                           selected = c("SPI","Old Aquatic","Incident Reports","iNaturalist"),
                           inline = T),
        shiny::actionButton('search_for_all_rec_sp', 'Search!')
      )
    ),
    column(width = 8,
           card(
             DT::DTOutput('all_rec_of_sp_results')
           )
    )
  )
)

all_sp_in_wbs = accordion_panel(
  'All Species in 1+ Waterbodies',
  layout_column_wrap(
    1/3,
    textInput('all_sp_in_wbs_name',"Name of Waterbody"),
    textInput('all_sp_in_wb_lat','Latitude of a point inside Waterbody',placeholder = '(e.g. 51.2)',value = NULL),
    textInput('all_sp_in_wb_lng','Longitude of a point inside Waterbody', placeholder = '(e.g. -121.2)', value = NULL)
  ),
  layout_column_wrap(
    1/2,
    actionButton('search_for_all_sp_in_wb', 'Search!'),
    downloadButton('all_rec_of_sp_res_dl', 'Download (.xlsx)')
  ),
  DT::DTOutput('all_sp_in_wb_results')
)


species_search_navpanel = bslib::nav_panel(
  title = 'Species Search',
  bslib::accordion(
    multiple = F,
    all_recs_of_sp_acc,
    all_sp_in_wbs
  )
)

downstream_wb_navpanel = bslib::nav_panel(
  title = 'Find Downstream Waterbodies',
  fluidRow(
    1/4,
    textInput('wb_for_downst','Waterbody Name', placeholder = '(optional)'),
    textInput('wb_for_downst_lat','Latitude of a point inside Waterbody', placeholder = '(e.g. 51.2)'),
    textInput('wb_for_downst_lng','Longitude of a point inside Waterbody', placeholder = '(e.g. -121.2)'),
    sliderInput('radius_wb_downst', 'Radius to search around Waterbody (km)',
                value = 10, min = 0, max = 20)
  )
)


prioritization_navpanel = bslib::nav_panel(
  title = 'Prioritization Model',
  layout_column_wrap(
    1/2,
    tagList(
      selectInput('p_model_layer_name', 'Layer', choices = c("Species Occurrence","Waterbody Connectivity","Proximity to Settlements")),
      selectInput('p_model_layer_role','Role',
                  choices = c('Species Occurrence Data' = 'occurrence',
                              'Geographic Units' = 'geog_units',
                              'ID Column in Geographic Units' = 'geog_id_col',
                              'Risk Factor' = 'risk'),
                  selected = 'occurrence')
    ),
    verbatimTextOutput('p_model_feedback')
  )
)

info_panel = bslib::nav_panel(
  title = 'Info',
  p("Here is a bunch of text. And some more text!")
)

bcinvadeR_update_button <- actionButton('update_bcinvadeR','Update {bcinvadeR}')

upload_incident_sheet <- shiny::fileInput('inc_report_xlsx',
                                          label = '',
                                          accept = '.xlsx',
                                          buttonLabel = 'Update Incident File')

ui = page_navbar(
  theme = bcinv_theme,
  title = 'BC Invasives Command Center',
  species_search_navpanel,
  downstream_wb_navpanel,
  prioritization_navpanel,
  info_panel,
  footer = div(
      div(bcinvadeR_update_button,style='margin-right:10px'),
      div(upload_incident_sheet, style = 'margin-top:-1.5rem'),
    style='display:inline-flex;margin-left:10px;'
  )
)
