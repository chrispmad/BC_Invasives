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

  file.remove('app/www/Master Incidence Report Records.xlsx')
  print('Removed old version of incident report excel document.')

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

  # Update the priority invasive species excel sheet and pull out species
  tryCatch(
    file.copy(
      from = "J:/2 SCIENCE - Invasives/GENERAL/Inter-governmental-relations/IMISWG/Priority species list/Provincial Priority IS List_2023-11-15.xlsx",
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

  # Read in priority list of AIS species (excel file), use it to update selectInput
  pr_sp = readxl::read_excel('app/www/AIS_priority_species.xlsx')

  # Format this terrible excel sheet
  data_start = min(which(pr_sp[,1] == 'Disease'))

  pr_sp = pr_sp[data_start:nrow(pr_sp),]

  names(pr_sp) <- c("group","status","name","genus","species")

  # Just for our species of interest.
  pr_sp = pr_sp |>
    dplyr::filter(group == 'Fish' | name %in% c("Whirling disease") | stringr::str_detect(name, '(mussel|crayfish)'))

  # Split out grouped species names into separate rows.
  pr_sp = pr_sp |>
    dplyr::mutate(name = dplyr::case_when(
      stringr::str_detect(name, 'Snakehead') ~ 'Northern Snakehead',
      name == 'Bullhead' ~ 'Yellow Bullhead',
      name == 'Red bellied piranha*' ~ 'Red bellied piranha',
      stringr::str_detect(name, '\\(') ~ stringr::str_remove_all(name,'\\(.*'),
      T ~ name
    )) |>
    dplyr::mutate(name = stringr::str_squish(name)) |>
    dplyr::bind_rows(
      tidyr::tibble(
        group = rep("Fish", 4),
        status = c("prevent","prevent",rep("management",2)),
        name = c("Blotched Snakehead","Rainbow Snakehead",'Black Bullhead','Brown Bullhead'),
        genus = c("Channa",'Channa','Ameiurus','Ameiurus'),
        species = c("maculata","bleheri",'melas','nebulosus')
      )
    ) |>
    dplyr::filter(name != 'Bullhead') |>
    dplyr::arrange(name)

  write.csv(pr_sp, 'app/www/priority_species_table2.csv', row.names = F)

  # Do record search for all species of interest! This takes a minute.
  occ_dat_search_results = pr_sp$name |>
    lapply(\(x) tryCatch(bcinvadeR::grab_aq_occ_data(x),error=function(e)return(NULL)))

  occ_dat_res_b = dplyr::bind_rows(occ_dat_search_results)

  sf::write_sf(occ_dat_res_b, 'app/www/occ_dat.gpkg')

  print("Written SF object of occurrence data records to app's www folder")

  if(!publishing_results$error){
    # Update bcinvadeR R package
    if('bcinvadeR' %in% devtools::loaded_packages()$package){
      unload('bcinvadeR')
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
