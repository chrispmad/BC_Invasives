cat("This script updates the BC Invasives Dashboard (https://chrispmadsen.shinyapps.io/bc_invasives_dashboard/)\n")

invisible(library(tidyverse))

# Check to see if a previous attempt to automatically
# publish this app has failed; if so, don't attempt any of the stuff below,
# as that will very likely fail again.

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

setwd(here::here())

source("../ais_prioritization_models/scripts/utils/gather_AIS_data.R")

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
  occ_dat_res_b = sf::read_sf(paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/AIS_occurrences_all_sources.gpkg"))

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
        group = c('Fish','Fish','Fish','Fish','Other invertebrates','Fish',
                  'Other invertebrates','Other invertebrates','Other invertebrates',
                  'Fish','Fish'),
        status = c('Provincial EDRR','Provincial EDRR','Management','Management','Management','Management',
                   'Provincial Containment','Provincial Containment','Provincial Containment','Management',
                   'Prevent'),
        name = c('Oriental weatherfish','Fathead minnow','Pumpkinseed','Carp','Common Freshwater Jellyfish','Bluegill',
                 'Asiatic clam','Golden clam','Good luck clam','Yellow pickerel',
                 'Mosquitofish'),
        genus = c('Misgurnus','Pimephales','Lepomis','Cyprinus','Craspedacusta','Lepomis',
                  'Corbicula','Corbicula','Corbicula','Sander','Gambusia'),
        species = c('anguillicaudatus','promelas','gibbosus','carpio','sowerbyi','macrochirus',
                    'fluminea','fluminea','fluminea','vitreus','affinis')
      )
    )
  #
  # Ensure species' common names are Sentence case.
  pr_sp$name = stringr::str_to_sentence(pr_sp$name)

  # Drop those temporary additions that we used to find more records online etc.
  pr_sp = pr_sp |>
    dplyr::filter(!name %in% c('Oriental weatherfish',
                               'Fathead minnow',
                               'Pumpkinseed',
                               'Common freshwater jellyfish',
                               'Bluegill',
                               'Mosquitofish',
                               'Asiatic clam',
                               'Golden clam',
                               'Good luck clam',
                               'Yellow pickerel'))

  names(pr_sp) = stringr::str_to_title(names(pr_sp))

  write.csv(pr_sp, 'app/www/priority_species_table.csv', row.names = F)

  sf::write_sf(occ_dat_res_b, 'app/www/occ_dat.gpkg')

  print("Written SF object of occurrence data records to app's www folder")

  # if(!publishing_results$error){
  #   # Update bcinvadeR R package
  #   if('bcinvadeR' %in% devtools::loaded_packages()$package){
  #     devtools::unload('bcinvadeR')
  #   }
  #   devtools::install_github('chrispmad/bcinvadeR',
  #                            upgrade = 'never')
  # }

  if(!publishing_results$error){
    # Publish app to Shinyapps.io
    tryCatch(
      rsconnect::deployApp(
        appDir = 'app/',
        appTitle = 'BC_Invasives_Dashboard',
        account = 'chrispmadsen',
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

