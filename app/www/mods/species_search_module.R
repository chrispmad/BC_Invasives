spec_search_UI <- function(id, species_common_names) {
  ns <- NS(id)
  tagList(
    card(
      shinyWidgets::pickerInput(
        ns('all_rec_sp_name'),
        "Species Common Name",
        choices = NA,
        multiple = F,
        options = list(container = "body")),
      checkboxGroupInput(ns('all_rec_sp_sources'),'Sources to search',
                         choices = c("SPI","Old Aquatic","Incident Reports","iNaturalist"),
                         selected = c("SPI","Old Aquatic","Incident Reports","iNaturalist"),
                         inline = T,
                         width = '100%'),
      shiny::actionButton(ns('search_for_all_rec_sp'), 'Search!')
    )
  )
}

spec_search_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ais_lookup_tbl = vroom::vroom('priority_species_table.csv')

      shinyWidgets::updatePickerInput(session = session,
                        inputId = "all_rec_sp_name",
                        choices = unique(ais_lookup_tbl$name))
      # Get All Records of Species
      all_rec_of_sp = reactiveVal(data.frame(results = 'No search run yet.'))

      names_of_sp_to_search_for = reactive({
        stringr::str_split(input$all_rec_sp_name, pattern = ', ') |>
          unlist()
      })

      observeEvent(input$search_for_all_rec_sp, {
        req(!is.null(input$all_rec_sp_name))

        withProgress(
          message = "Starting search...",
          detail = 'Testing BC Data Catalogue connection',
          value = 1/3, expr = {

            test_search = tryCatch(
              expr = bcdata::bcdc_query_geodata('aca81811-4b08-4382-9af7-204e0b9d2448') |>
                bcdata::filter(SPECIES_NAME == 'goldfish') |>
                bcdata::collect(),
              error = function(e) NULL)

            if(!is.null(nrow(test_search))){
              incProgress(amount = 1/3, message = 'Connection to BC Data Catalogue made')
              cat("BC Data Catalogue connection successful.")
            }

            occ_dat = tryCatch(
              expr = bcinvadeR::grab_aq_occ_data(
                common_names = names_of_sp_to_search_for(),
                sources = input$all_rec_sp_sources,
                excel_path = 'Master Incidence Report Records.xlsx',
                quiet = F),
              error = function(e) cat("Unfortunately, there was an error."))
            incProgress(amount = 1/3, message = 'Species search complete')
          })

        all_rec_of_sp(occ_dat)
      })

      all_rec_of_sp
    }
  )
}
