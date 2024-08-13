function(input, output, session) {

  # Probably unnecessary once the app is published...
  if(!stringr::str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }

  source('server_scripts/load_data_files.R', local = T)$value

  source('server_scripts/establish_reactives.R', local = T)$value

  source('server_scripts/ais_rangemap_tab.R', local = T)$value

  source('server_scripts/make_wb_finder_map.R', local = T)$value

  source('server_scripts/incident_report_tab.R', local = T)$value

  source('server_scripts/regional_contacts_tab.R', local = T)$value
  # source('server_scripts/find_connected_waterbodies_tab.R', local = T)$value

  source('server_scripts/hide_coord_finder_map.R', local = T)$value

}
