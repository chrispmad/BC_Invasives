cat("This script updates the BC Invasives Dashboard (https://chrispmadsen.shinyapps.io/bc_invasives_dashboard/)\n")

invisible(library(tidyverse))

# Check to see if a previous attempt to automatically
# publish this app has failed; if so, don't attempt any of the stuff below,
# as that will very likely fail again.

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
proj_wd = getwd()
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"

setwd(here::here())

# source("../ais_prioritization_models/scripts/utils/gather_AIS_data.R")

print("Beginning republishing")

if(!file.exists(paste0('publishing_results/publishing_results_',Sys.Date(),'_errored.csv'))){

  publishing_results = data.frame(
    publish_succeeded = F,
    error_at = 'None',
    error = FALSE
  )

  if(file.exists('app/www/Master Incidence Report Records.xlsx')) {
    file.remove('app/www/Master Incidence Report Records.xlsx')
    print('Removed old version of incident report excel document.')
  }

  # if(file.exists('app/www/Master Incidence Report Records.xlsx')) {
  #   file.remove('app/www/Master Incidence Report Records Terrestrial.xlsx')
  #   print('Removed old version of terrestrial incident report excel document.')
  # }

  # Update the invasive tracker sheet
  tryCatch(
    file.copy(
      from = paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Master Incidence Report Records.xlsx"),
      to = 'app/www/Master Incidence Report Records.xlsx'
    ),
    error = function(e) {
      publishing_results$error_at = 'excel_copying_master_incident'
      publishing_results$error = TRUE
    }
  )

  # Put aside unconfirmed reports!
  unconf_occs = readxl::read_excel('app/www/Master Incidence Report Records.xlsx', sheet = 'Aquatic Reports') |>
    dplyr::filter(ID_Confirmation == 'Unconfirmed')

  print('Copied new version of master incident tracking sheet into app folder.')

  # Update the priority invasive species excel sheet and pull out species
  if(file.exists('app/www/AIS_priority_species.xlsx')) file.remove('app/www/AIS_priority_species.xlsx')

  tryCatch(
    file.copy(
      from = paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/AIS_priority_species.xlsx"),
      to = 'app/www/AIS_priority_species.xlsx'
    ),
    error = function(e) {
      publishing_results$error_at = 'excel_copying_priority_list'
      publishing_results$error = TRUE
    }
  )

  print('Copied new version of priority list into app folder.')

    # Did the file copying work?
  if(!file.exists('app/www/Master Incidence Report Records.xlsx') | !file.exists('app/www/AIS_priority_species.xlsx')){
    publishing_results$error_at = 'excel_copying'
    publishing_results$error = TRUE
  }

  if(file.exists('app/www/WLRS_contacts.xlsx')) file.remove('app/www/WLRS_contacts.xlsx')

  # Copy over the excel file listing WLRS regional contacts
  tryCatch(
    file.copy(
      from = paste0(lan_folder,"2 SCIENCE - Invasives/GENERAL/Communications/WLRS_Regions_Contacts_for_AIS.xlsx"),
      to = 'app/www/WLRS_contacts.xlsx'
    ),
    error = function(e) {
      publishing_results$error_at = 'excel_copying_contact_list'
      publishing_results$error = TRUE
    }
  )

  # Snag pre-collated AIS occurrence records!
  occ_dat_res_b = sf::read_sf(paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/AIS_occurrences_all_sources.gpkg")) |>
    dplyr::mutate(ID_Confirmation = 'Confirmed')

  # Read in priority list of AIS species (excel file)
  pr_sp = readxl::read_excel('app/www/AIS_priority_species.xlsx',
                             skip = 20)

  names(pr_sp) <- c("group","status","name","genus","species")
  
  # is iNat doing something else?
  bullhead_entry<-c("Fish", "Management", "Bullhead sp", "Ameiurus", "sp")
  
  pr_sp<-rbind(pr_sp, bullhead_entry)
  
  # Just for our species of interest.
  pr_sp = pr_sp |>
    dplyr::filter(group == 'Fish' | name %in% c("Whirling disease") | stringr::str_detect(name, '(mussel|crayfish|mystery snail|mudsnail|clam|jellyfish|shrimp|waterflea)'))

  # Split out grouped species names into separate rows.
  pr_sp = pr_sp |>
    dplyr::mutate(name = stringr::str_squish(name)) |>
    dplyr::arrange(name)

  # Replace some species names.
  pr_sp = pr_sp |>
    dplyr::mutate(name = ifelse(name == "Pumpkinseed sunfish", "Pumpkinseed", name))

  # Ensure species' common names are Sentence case.
  pr_sp$name = stringr::str_to_sentence(pr_sp$name)

  names(pr_sp) = stringr::str_to_title(names(pr_sp))

  # Filter unconfirmed incidence reports for these priority species
  unconf_occs_pr = unconf_occs |>
    dplyr::filter(str_to_sentence(Submitted_Common_Name) %in% pr_sp$Name) |>
    dplyr::mutate(DataSource = "Incidental Observation") |>
    dplyr::mutate(Date = dplyr::case_when(
      stringr::str_detect(Date,"[0-9]{5}") ~ openxlsx::convertToDate(Date),
      T ~ lubridate::ymd(Date)
    )) |>
    dplyr::mutate(Date = as.character(Date)) |>
    dplyr::mutate(ID_Confirmation = "Unconfirmed") |>
    dplyr::mutate(lat = as.numeric(Latitude), lng = as.numeric(Longitude)) |>
    dplyr::filter(!is.na(lat), !is.na(lng)) |>
    sf::st_as_sf(coords = c('lng','lat'), crs = 4326) |>
    dplyr::select(DataSource, Date, Species = Submitted_Common_Name, Location, ID_Confirmation, geom = geometry)

  # Bind the unconfirmed reports at the bottom of the occ_dat_res_b
  occ_dat_res_b = occ_dat_res_b |>
    dplyr::bind_rows(unconf_occs_pr)

  write.csv(pr_sp, 'app/www/priority_species_table.csv', row.names = F)

  # Ensure we have the right capitalization scheme for common name.
  occ_dat_res_b = occ_dat_res_b |>
    dplyr::mutate(Species = stringr::str_to_sentence(Species))

  # Remove records from occ_dat object where species are actually native to that
  # part of the province ("Native Range") or have been eradicated ("Eradicated")
  # or resampled and proven to be
  # no longer found there ("Anecdotal")

  # 1) Native Range
  ## Ecological Drainage Units
  np_native_range = bcdata::bcdc_query_geodata("eaubc-ecological-drainage-units") |>
    bcdata::filter(ECO_DRAINAGE_UNIT %in% c("Alsek","North Coastal","Lewes","Nakina",
                                    "Teslin","Upper Stikine","Upper Liard","Taku",
                                    "Lower Liard","Lower Peace","Hay")) |>
    bcdata::collect() |>
    sf::st_transform(4326)

  walleye_native_range = bcdata::bcdc_query_geodata("eaubc-ecological-drainage-units") |>
    bcdata::filter(ECO_DRAINAGE_UNIT %in% c("Upper Liard","Lower Liard","Hay","Lower Peace")) |>
    bcdata::collect() |>
    sf::st_transform(4326)

  native_range_occs = dplyr::bind_rows(
    occ_dat_res_b |>
      dplyr::filter(Species == 'Northern pike') |>
      sf::st_filter(np_native_range),
    occ_dat_res_b |>
      dplyr::filter(Species == 'Walleye') |>
      sf::st_filter(walleye_native_range)
  ) |>
    dplyr::mutate(range_label = "native")

  print(paste0("Looking for native ranges of Northern Pike and Walleye, we found ",nrow(native_range_occs)," rows that describe native occurences."))
  print(paste0("Before removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))

  # Drop records that are now in native range
  occ_dat_res_b = occ_dat_res_b |>
    dplyr::anti_join(native_range_occs |>
                       sf::st_drop_geometry())
  print(paste0("After removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))

  # 2) Eradicated
  eradicated_occs = occ_dat_res_b |>
    # Only one eradicated record currently, for Northern Pike!
    dplyr::filter((Date == '2009' & Location == 'Haha Lake') |
                    (Date == '2006' & Location == 'Haha Lake'))

  print(paste0("Looking for records that have been flagged as eradicated, we found ",nrow(eradicated_occs)," rows."))
  print(paste0("Before removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))
  # Drop records that are now in native range
  occ_dat_res_b = occ_dat_res_b |>
    dplyr::anti_join(eradicated_occs |>
                       sf::st_drop_geometry())
  print(paste0("After removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))

  # 3) Anecdotal
  anecdotal_occs = occ_dat_res_b |>
    dplyr::filter((Location == 'SUMMIT LAKE') |
                    (Date == '2001-01-01' & Location == "AID LAKE"))

  print(paste0("Looking for records that have been flagged as eradicated, we found ",nrow(eradicated_occs)," rows."))
  print(paste0("Before removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))
  # Drop records that are now in native range
  occ_dat_res_b = occ_dat_res_b |>
    dplyr::anti_join(anecdotal_occs |>
                       sf::st_drop_geometry())
  print(paste0("After removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))

  # for Bullheads, we are going to merge them all - so that Yellow, Brown, Black, And Bullead are shown.
  bullhead_rows <- occ_dat_res_b |> 
    filter(str_detect(Species, regex("Bullhead", ignore_case = TRUE)))
  
  bullhead_group <- bullhead_rows |> 
    mutate(`Confirmed common name` = Species) |> 
    mutate(Species = "Bullhead sp")            
  
  
  
  
  occ_dat_res_b <- occ_dat_res_b |> 
    mutate(`Confirmed common name` = Species)
  
  occ_dat_res_b <- bind_rows(occ_dat_res_b, bullhead_group) |> 
    arrange(Species)
  
  
  
  ### Must be finished
  # bass_rows<-occ_dat_res_b |> 
  #   filter(str_detect(Species, regex("Bass", ignore_case = TRUE))) |> 
  #   filter(str_detect(Species, regex("Bass (small or large-mouth)", ignore_case = TRUE))) |> 
  #   filter(str_detect(Species, regex("Smallmouth bass", ignore_case = TRUE))) |> 
  #   filter(str_detect(Species, regex("Largemouth bass", ignore_case = TRUE)))
  # 
  # 
  # bullhead_group <- bullhead_rows |> 
  #   mutate(Species = "Bullhead sp")     
  # 
  
  
  ######
  
  file.remove("app/www/native_range_occs.gpkg")
  file.remove("app/www/eradicated_occs.gpkg")
  file.remove("app/www/anecdotal_occs.gpkg")
  file.remove("app/www/occ_dat.gpkg")

  sf::write_sf(native_range_occs,"app/www/native_range_occs.gpkg")
  sf::write_sf(eradicated_occs,"app/www/eradicated_occs.gpkg")
  sf::write_sf(anecdotal_occs,"app/www/anecdotal_occs.gpkg")
  sf::write_sf(occ_dat_res_b, 'app/www/occ_dat.gpkg')

  print("Written SF object of occurrence data records to app's www folder")

  if(!publishing_results$error){
    # Update bcinvadeR R package
    if('bcinvadeR' %in% devtools::loaded_packages()$package){
      devtools::unload('bcinvadeR')
    }
    devtools::install_github('chrispmad/bcinvadeR',
                             upgrade = 'never')
  }

  if(!publishing_results$error){
    # Publish app to Shinyapps.io
    tryCatch(
      rsconnect::deployApp(
        appDir = 'app/',
        appTitle = 'BC_Invasives',
        account = 'jpphelan',
        forceUpdate = T
      ),
      error = function(e) {
        publishing_results$error_at = 'publishing'
        publishing_results$error = TRUE
      }
    )
  }

  if(publishing_results$error){
    write.csv(publishing_results,
              paste0('publishing_results/publishing_results_',Sys.Date(),'_errored.csv'))
  } else {
    write.csv(publishing_results,
              paste0('publishing_results/publishing_results_',Sys.Date(),'.csv'))
  }
}

print(paste0('Republishing of app complete at ', Sys.time()))

