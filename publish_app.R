# Check to see if a previous attempt to automatically
# publish this app has failed; if so, don't attempt any of the stuff below,
# as that will very likely fail again.

setwd(here::here())

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

  if(file.exists('app/www/Master Incidence Report Records.xlsx')) {
    file.remove('app/www/Master Incidence Report Records Terrestrial.xlsx')
    print('Removed old version of terrestrial incident report excel document.')
  }

  # Update the invasive tracker sheet
  tryCatch(
    file.copy(
      from = "J:/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Master Incidence Report Records.xlsx",
      to = 'app/www/Master Incidence Report Records.xlsx'
    ),
    error = function(e) {
      publishing_results$error_at = 'excel_copying_master_incident'
      publishing_results$error = TRUE
    }
  )

  print('Copied new version of master incident tracking sheet into app folder.')

  # Same but for terrestrial tracker sheet
  terr_excel_files = list.files(path = "V:/Ecosystems/Conservation Science/Invasive Species/SPECIES/5_Incidental Observations/",
             pattern = "Master.*xlsx",
             full.names = T)
  # Which is the biggest / most recent of these files.
  terr_excel_info = terr_excel_files |>
    lapply(\(x) as.data.frame(file.info(x))) |>
    dplyr::bind_rows()

  biggest_terr_excel = which(terr_excel_info$size == max(terr_excel_info$size))
  most_recent_terr_excel = which(terr_excel_info$mtime == max(terr_excel_info$mtime))

  if(biggest_terr_excel == most_recent_terr_excel){
    terr_to_grab = biggest_terr_excel
  } else {
    terr_to_grab = NULL
  }

  terr_file_name = stringr::str_extract(terr_excel_files[terr_to_grab], "(?<=\\/)[^\\/]+.xlsx")

  if(!is.null(terr_to_grab)){
    tryCatch(
      file.copy(
        from = paste0("V:/Ecosystems/Conservation Science/Invasive Species/SPECIES/5_Incidental Observations/",terr_file_name),
        to = 'app/www/Master Incidence Report Records Terrestrial.xlsx'
      ),
      error = function(e) {
        publishing_results$error_at = 'excel_copying_master_incident_terrestrial'
        publishing_results$error = TRUE
      }
    )
  }

  print('Copied new version of TERRESTRIAL master incident tracking sheet into app folder.')

  # Update the priority invasive species excel sheet and pull out species
  if(file.exists('app/www/AIS_priority_species.xlsx')) file.remove('app/www/AIS_priority_species.xlsx')

  tryCatch(
    file.copy(
      from = "J:/2 SCIENCE - Invasives/SPECIES/AIS_priority_species.xlsx",
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

  file.remove('app/www/WLRS_contacts.xlsx')
  # Copy over the excel file listing WLRS regional contacts
  tryCatch(
    file.copy(
      from = "J:/2 SCIENCE - Invasives/GENERAL/Communications/WLRS_Regions_Contacts_for_AIS.xlsx",
      to = 'app/www/WLRS_contacts.xlsx'
    ),
    error = function(e) {
      publishing_results$error_at = 'excel_copying_contact_list'
      publishing_results$error = TRUE
    }
  )

  # Read in priority list of AIS species (excel file)
  pr_sp = readxl::read_excel('app/www/AIS_priority_species.xlsx',
                             skip = 20)

  names(pr_sp) <- c("group","status","name","genus","species")

  # Just for our species of interest.
  pr_sp = pr_sp |>
    dplyr::filter(group == 'Fish' | name %in% c("Whirling disease") | stringr::str_detect(name, '(mussel|crayfish|mystery snail|mudsnail|clam|jellyfish|shrimp|waterflea)'))

  # Split out grouped species names into separate rows.
  pr_sp = pr_sp |>
    dplyr::mutate(name = stringr::str_squish(name)) |>
    dplyr::filter(name != 'Bullhead') |>
    dplyr::arrange(name)

  # Add a couple alternate ways of spelling species common names.
  pr_sp = pr_sp |>
    dplyr::bind_rows(
      tidyr::tibble(
        group = c('Fish','Fish','Fish','Fish','Other invertebrates
'),
        status = c('Provincial EDRR','Provincial EDRR','Management','Management','Management'),
        name = c('Oriental weatherfish','Fathead minnow','Pumpkinseed','Carp','Common Freshwater Jellyfish'),
        genus = c('Misgurnus','Pimephales','Lepomis','Cyprinus','Craspedacusta'),
        species = c('anguillicaudatus','promelas','gibbosus','carpio','sowerbyi')
      )
    )

  # Ensure species' common names are Sentence case.
  pr_sp$name = stringr::str_to_sentence(pr_sp$name)

  # Do record search for all species of interest! This takes a minute.
  occ_dat_search_results = pr_sp$name |>
    lapply(\(x) tryCatch(bcinvadeR::grab_aq_occ_data(x),error=function(e)return(NULL)))

  occ_dat_res_b = dplyr::bind_rows(occ_dat_search_results)

  occ_dat_res_b = dplyr::mutate(occ_dat_res_b, Species = stringr::str_to_sentence(Species))

  # Just include records that had coordinates within BC's bounding box.
  occ_dat_res_b = occ_dat_res_b |>
    sf::st_transform(3005) |>
    sf::st_filter(sf::st_as_sfc(sf::st_bbox(dplyr::summarise(bcmaps::bc_bound())))) |>
    sf::st_transform(4326)

  # For species with multiple common names, homogenize the names to fit whatever
  # is present in 'priority_species_table.xlsx' file.
  occ_dat_res_b = occ_dat_res_b |>
    dplyr::mutate(Species = dplyr::case_when(
      Species == 'Oriental weatherfish' ~ 'Oriental weather loach',
      Species == 'Fathead minnow' ~ 'Rosy red fathead minnow',
      Species == 'Pumpkinseed' ~ 'Pumpkinseed sunfish',
      Species == 'Common freshwater jellyfish' ~ 'Freshwater jellyfish',
      Species %in% c("Carp","European Carp","Common Carp") ~ "Common carp",
      T ~ Species
    ))

  # In case we've picked up some Asian Carp or other species that
  # we might not actually want because they're not (yet?) in BC, drop those.
  occ_dat_res_b = occ_dat_res_b |>
    dplyr::filter(!Species %in% c("Asian Carp","Grass Carp","Silver Carp","Black Carp",
                                  "Bighead Carp"))

  # Drop those temporary additions that we used to find more records online etc.
  pr_sp = pr_sp |>
    dplyr::filter(!name %in% c('Oriental weatherfish','Fathead minnow','Pumpkinseed','Common freshwater jellyfish'))

  write.csv(pr_sp, 'app/www/priority_species_table.csv', row.names = F)

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
        appTitle = 'BC_Invasives_Dashboard',
        account = 'chrispmadsen'
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
