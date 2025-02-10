library(shiny)
library(tidyverse)
library(bslib)
library(devtools)
library(bcinvadeR)
library(leaflet)
library(shinyjs)
library(plotly)
library(Polychrome)
library(terra)
library(kableExtra)

source('www/mods/species_search_module.R')
source('ui_scripts/ui_theming.R')
# source('ui_scripts/ui_species_search_navpanel.R')
source('ui_scripts/ui_ais_rangemap_navpanel.R')
source('ui_scripts/ui_incident_dashboard_navpanel.R')
source('ui_scripts/ui_regional_contacts_navpanel.R')
# source('ui_scripts/ui_connected_wb_navpanel.R')
# source('ui_scripts/ui_prioritization_navpanel.R')
source('ui_scripts/ui_info_navpanel.R')
source('ui_scripts/ui_coordinate_finder_navitem.R')

ui = page_navbar(
  fillable = T,
  shinyjs::useShinyjs(),
  shiny::includeCSS('ui_scripts/styles/my_styles.css'),
  theme = bcinv_theme,
  title = tagList(
    'BC Invasives',
    shiny::icon('fish-fins',class = 'swimming-fish'),
    shiny::icon('o', class = 'swimming-fish-bub'),
    shiny::icon('o', class = 'swimming-fish-bub')
  ),
  # print("We've loaded in the header using a tagList"),
  # species_search_navpanel,
  AIS_rangemap_navpanel,
  incident_dashboard_navpanel,
  regional_contacts_navpanel,
  # connected_wb_navpanel,
  # prioritization_navpanel,
  info_panel,
  nav_spacer(),
  coordinate_finder_navitem
)
