library(shiny)
library(tidyverse)
library(bslib)
library(devtools)
library(bcinvadeR)
library(ggiraph)
library(leaflet)
library(shinyjs)
library(plotly)
library(Polychrome)
library(terra)

# ais_lookup_tbl = vroom::vroom('www/priority_species_table.csv')

source('www/mods/species_search_module.R')
source('ui_scripts/ui_theming.R')
source('ui_scripts/ui_species_search_navpanel.R')
source('ui_scripts/ui_ais_rangemap_navpanel.R')
source('ui_scripts/ui_incident_dashboard_navpanel.R')
source('ui_scripts/ui_connected_wb_navpanel.R')
# source('ui_scripts/ui_prioritization_navpanel.R')
source('ui_scripts/ui_info_navpanel.R')
source('ui_scripts/ui_coordinate_finder_navitem.R')

ui = page_navbar(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      '
      .swimming-fish {
        animation: swim 3s linear infinite;
        left: 8rem;
        position: absolute;
      }

      .swimming-fish-bub {
        animation: bubble 3s linear infinite;
        position: absolute;
        visibility: hidden;
        left: 8.5rem;
      }

      .swimming-fish-bub:nth-child(2) {
        left: 8.0rem;
        animation-delay: -.25s;
      }

      @keyframes swim {
        0%{
          transform: rotate(-10deg) translateY(0px);
        }
        25%{
          transform: translateY(-1rem);
        }
        50%{
          transform: rotate(10deg) translateY(0px);
        }
        75%{
          transform: translateY(1rem);
        }
        100%{
          transform: rotate(-10deg) translateY(0px);
        }
      }

      @keyframes bubble {
        0%{
          visibility: hidden;
          transform: scale(0.01) translateY(0px);
        }
        25%{
          visibility: visible;
          transform: scale(0.01) translateY(-1rem);
        }
        50%{
          visibility: visible;
          transform: scale(0.2) translateY(0rem);
        }
        75%{
          visibility: visible;
          transform: scale(0.5) translateY(-0.75rem);
        }
        100%{
          visibility: hidden;
          transform: scale(1) translateY(-1.5rem);
        }
      }
      '
    )
  ),
  theme = bcinv_theme,
  title = tagList(
    'BC Invasives',
    shiny::icon('fish-fins',
                class = 'swimming-fish'),
    shiny::icon('o', class = 'swimming-fish-bub'),
    shiny::icon('o', class = 'swimming-fish-bub')
  ),
  species_search_navpanel,
  AIS_rangemap_navpanel,
  incident_dashboard_navpanel,
  connected_wb_navpanel,
  # prioritization_navpanel,
  info_panel,
  nav_spacer(),
  coordinate_finder_navitem
)
