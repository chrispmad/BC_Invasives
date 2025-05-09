wlrs_c = readxl::read_excel("WLRS_contacts.xlsx")

# Correct naming convention of regions.
wlrs_c = wlrs_c |>
  dplyr::mutate(Region = dplyr::case_when(
    Region == "North East Region" ~ "Northeast",
    Region == "Kootenay" ~ "Kootenay-Boundary",
    Region == "Thompson Okanagan" ~ "Thompson-Okanagan",
    T ~ Region
  ))

# Summarise by region
wlrs_s = wlrs_c |>
  dplyr::group_by(Region) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), \(x) paste0(x, collapse = "; ")))

# Get regions
regs = regs = sf::read_sf("nr_regions.gpkg") |>
  sf::st_transform(4326) |>
  dplyr::mutate(Region = stringr::str_extract(ORG_UNIT_NAME,".*(?= Natural)")) |>
  dplyr::select(Region)

regs = regs |>
  dplyr::left_join(wlrs_s)

tbls_for_leafpop = regs |>
  sf::st_drop_geometry() |>
  dplyr::group_by(Region) |>
  dplyr::group_split() |>
  purrr::map(\(x) {
    tbl_for_leafpop = x |>
      tidyr::separate_longer_delim(cols = dplyr::everything(), delim = ';') |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) stringr::str_squish(x))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) stringr::str_replace_all(x," ","<br>")))

    tbl_for_leafpop
  })

# to_html_table<-function(dataframe){
#   tags$table(
#     tags$thead(tags$tr(lapply(colnames(dataframe), function(x) tags$th(x)))),
#     tags$tbody(
#       apply(dataframe,1, function(x) { tags$tr(lapply(x, function(y) tags$td(y)))})
#     ))
# }
#
# test = tbls_for_leafpop |>
#   lapply(to_html_table)

test_kbl = tbls_for_leafpop |>
  lapply(\(x) {
    kable(x, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(2, width = "200px")
  })

test_kbl <- gsub(
  "<table",
  "<table style='background-color: rgba(255, 255, 255, 0.3); border: 1px solid rgba(0, 0, 0, 0.2);'",
  test_kbl
)

test_kbl
# Make leaflet map
output$reg_contacts_leaf = renderLeaflet({
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addPolygons(
      data = regs |> arrange(Region),
      label = lapply(test_kbl, shiny::HTML)#,
    ) |>
    leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(zoom = 8))
})
