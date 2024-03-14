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
        position: absolute;
      }

      .swimming-fish-bub {
        animation: bubble 3s linear infinite;
        position: absolute;
      }

      @keyframes swim {
        0%{
          transform: rotate(-10deg) translateY(0px);
        }
        25%{
          transform: translateY(-10px) translateX(6px);
        }
        50%{
          transform: rotate(10deg) translateY(0px) translateX(10px);
        }
        75%{
          transform: translateY(10px) translateX(6px);
        }
        100%{
          transform: rotate(-10deg) translateY(0px);
        }
      }

      @keyframes bubble {
        0%{
          visibility: hidden;
          transform: scale(0.1);
        }
        25%{
          visibility: visible;
          transform: scale(0.1) translateY(5px);
        }
        50%{
          visibility: visible;
          transform: scale(0.1) translateY(10px);
        }
        75%{
          visibility: visible;
          transform: scale(0.1) translateY(15px);
        }
        100%{
          visibility: scale(0.1) hidden;
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
    shiny::icon('o', class = 'swimming-fish-bub'),
    shiny::icon('o', class = 'swimming-fish-bub'),
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
