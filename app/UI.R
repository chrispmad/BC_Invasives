library(shiny)
library(tidyverse)
library(bslib)
library(devtools)
library(bcinvadeR)
library(leaflet)
library(shinyjs)
library(plotly)

source('www/mods/species_search_module.R')

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
  '.accordion-button:not(.collapsed){
    background: #168eb075;
  }

  .unhooked-btn {
  background: grey;
  }

  .rainbow-btn-container {
    width: 100%;
    height: 93%;
    display: flex;
    justify-content: center;
    align-items: center;
  }

  .rainbow-btn {
    border-radius: 10px;
    cursor: pointer;
    z-index: 0;
    width: 200x;
    height: 35px;
    background: linear-gradient(45deg, #ff0000, #ff7300, #fffb00, #48ff00,
                                #00ffd5, #002bff, #7a00ff, #ff00c8, #ff0000);
    border-radius: 10px;
    background-size: 400%;
    animation: glowEffect 10s linear infinite;
  }

  @keyframes glowEffect {
    0%{
      background-position: 0 0;
    }
    50%{
      background-position: -200% 100%;
    }
    100%{
      background-position: 400% 0;
    }
  }

  '
  )

# bcinv_theme = bslib::bs_theme(
#   bootswatch = 'cerulean'
# )


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

connected_wb_navpanel = bslib::nav_panel(
  title = 'Find Connected Waterbodies',
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
      tabPanel("Coordinate Finder",
               tagList(
                 p("Click me to grab latitude / longitude!"),
                 leafletOutput('wb_finder_map')
               )
      ),
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


prioritization_navpanel = bslib::nav_panel(
  title = 'Prioritization Model',
  layout_column_wrap(
    1/2,
    accordion(
      id = 'p_model_accordion',
      accordion_panel(
        title = 'Focal Species',
        id = 'occ_dat_accordion',
        spec_search_UI('p_model_layer'),
        actionButton(
          'add_occ_data_to_p_model',
          label = div(
            class = 'rainbow-btn-container',
            div(
              id = 'rainbow-btn',
              class = 'unhooked-btn',
              textOutput('occ_data_results_text')
            )
          ),
          style = 'background:transparent;border:none;'
        )
      ),
      accordion_panel(
        title = 'Area(s) of Interest',
        id = 'area_of_int_accordion',
        selectInput('geog_units_presets',
                    'Preset Options',
                    choices = c('Natural Resource Regions' = 'nr_regs',
                                'Ecosections' = 'ecosecs')),
        actionButton(
          'add_geog_units_to_p_model',
          label = 'Add Geographic Units to model'
        )
      ),
      accordion_panel(
        title = 'Risk Layers',
        id = 'risk_accordion',
        layout_column_wrap(
          1/2,
          fileInput('risk_layers_input',
                    'Risk Layer(s)',
                    multiple = TRUE,
                    accept = c('.gpkg','.zip')),
          uiOutput('risk_layer_weights_ui')
        ),
        actionButton(
          'add_risk_layers_to_p_model',
          label = 'Add Risk Layers to model'
        )
      ),
      accordion_panel(
        title = 'Run Model',
        id = 'run_model_accordion',
        actionButton(
          'run_p_model',
          'Run Model!'
        )
      )
    ),
    tagList(
      # textOutput('p_model_feedback'),
      DT::DTOutput('p_model_output_DT')
    )
  )
)

info_panel = bslib::nav_panel(
  title = 'Info',
  h3("Metadata & Contact Information"),
  h5("Data Sources"),
  p("BC Data Catalogue"),
  HTML("<br><br><br>"),
  h5("Contact Information"),
  p("App Maintainer: Chris Madsen"),
  p("Email: chris.madsen@gov.bc.ca")
)

ui = page_navbar(
  shinyjs::useShinyjs(),
  theme = bcinv_theme,
  title = 'BC Invasives Command Center',
  species_search_navpanel,
  connected_wb_navpanel,
  prioritization_navpanel,
  info_panel,
  # footer = div(
  #     div(bcinvadeR_update_button,style='margin-right:10px'),
  #     # div(upload_incident_sheet, style = 'margin-top:-1.5rem'),
  #   style='display:inline-flex;margin-left:10px;'
  # )
)
