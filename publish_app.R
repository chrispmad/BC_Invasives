publishing_results = data.frame(
  publish_succeeded = F,
  error_at = 'None',
  error = FALSE
)

file.remove('app/www/Master Incidence Report Records.xlsx')

# Update the invasive tracker sheet
tryCatch(
  file.copy(
  from = "J:/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Master Incidence Report Records.xlsx",
  to = 'app/www/Master Incidence Report Records.xlsx'
  ),
  error = function(e) {
    publishing_results$error_at = 'excel_copying'
    publishing_results$error = TRUE
  }
)

# Did the file copy work?
if(!file.exists('app/www/Master Incidence Report Records.xlsx')){
  publishing_results$error_at = 'excel_copying'
  publishing_results$error = TRUE
}

if(!publishing_results$error){
  # Update bcinvadeR R package
  devtools::install_github('chrispmad/bcinvadeR',
                           upgrade = 'none')
}

if(!publishing_results$error){
# Publish app to Shinyapps.io
rsconnect::deployApp(
  appDir = 'app/',
  appTitle = 'BC_Invasives_Dashboard',
  account = 'chrispmadsen',
  on.failure = function(){write.csv}
)
}
