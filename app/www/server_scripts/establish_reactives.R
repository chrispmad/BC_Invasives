# Make reactive form of data, responding to the species selection in the
# AIS rangemap tab.
occ_dat_sp = reactive({
  # If we are in species search mode...
  if(input$search_type_input){
    # Make sure we have EITHER a species name OR a region name to search with.
    # Make sure we have a species name to search with.
    req(!is.null(input$ais_rangemap_sp))

    # Make sure we have EITHER a species name OR a region name to search with.
    req(!'None' %in% input$ais_rangemap_sp | input$ais_rangemap_reg != 'None')

    # We're looking for AIS by species name!
    if(!'None' %in% input$ais_rangemap_sp){

      species_for_search = input$ais_rangemap_sp

      carp_row_to_expand = str_detect(input$ais_rangemap_sp, '^Asian [c,C]arp')
      if(sum(carp_row_to_expand) > 0){
        # Drop input that was for Asian Carp.
        species_for_search = species_for_search[-carp_row_to_expand]
        # Add split-out options for Carp
        species_for_search = c(species_for_search, c("Bighead Carp","Black Carp","Grass Carp","Silver Carp","Asian Carp"))
      }
      species_for_search = stringr::str_to_sentence(species_for_search)

      dat = occ_dat |>
        dplyr::filter(Species %in% stringr::str_remove(species_for_search," \\(.*"))
    }
    if(input$ais_rangemap_reg != 'None'){
      # We're looking for AIS by NR region!
      selected_nr_reg = nr_regs |>
        dplyr::filter(reg_name == input$ais_rangemap_reg)
      dat = occ_dat |>
        sf::st_filter(selected_nr_reg)
    }
    # If user clicks on or off different data sources, apply those here.
    dat = dat |>
      dplyr::mutate(DataSource = dplyr::case_when(
        DataSource %in% c('BCG fish layer') ~ 'FDIS',
        DataSource %in% c('Old BCG AIS layer') ~ 'Old AIS Layer',
        DataSource == 'Incidental Observation' ~ 'Incidental Observations',
        T ~ DataSource
      )) |>
      dplyr::filter(DataSource %in% input$all_sp_in_wbs_sources)

    # Also, include or exclude unconfirmed reports depending on that input.
    if(!input$retain_unconfirmed_reports){
      dat = dat |>
        dplyr::filter(ID_Confirmation == 'Confirmed')
    }
    # Also, make sure the date column is in one single format.
    dat = dat |>
      dplyr::mutate(Date = dplyr::case_when(
        is.na(Date) ~ "No Date Info",
        stringr::str_detect(Date, '[0-9]{5}') ~ as.character(openxlsx::convertToDate(Date)),
        T ~ Date
      ))
  } else {
    # Otherwise we are in waterbody search mode
    dat = NULL
  }
  dat
})

selected_dates = reactive({
  input$date_filter_for_occ_dat
})

occ_dat_sp_f = reactive({
  req(!is.null(occ_dat_sp()))
  if(is.null(input$date_filter_for_occ_dat)) return(occ_dat_sp())
  req(!is.null(input$date_filter_for_occ_dat))

  occ_dat_sp() |>
    # Ensure the Date column has month and day; if not, add January 1st as default.
    dplyr::mutate(Date = ifelse(!stringr::str_detect(Date,"-"),paste0(Date,"-01-01"),Date)) |>
    dplyr::mutate(Date = lubridate::ymd(Date)) |>
    # dplyr::mutate(min_date = min(Date,na.rm=T),
    #               max_date = max(Date,na.rm=T)) |>
    dplyr::filter(Date %within% lubridate::interval(start = lubridate::ymd(selected_dates()[1]),
                                                    end = lubridate::ymd(selected_dates()[2])) | is.na(Date))
})

