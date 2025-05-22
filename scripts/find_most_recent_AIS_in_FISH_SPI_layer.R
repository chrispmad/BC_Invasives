library(bcinvadeR)

source("../ais_prioritization_models/scripts/utils/gather_AIS_data.R")

my_query = bcdata:::CQL("ORDER BY OBSERVATION_DATE")

# spi_fish = bcdc_query_geodata("aca81811-4b08-4382-9af7-204e0b9d2448") |>
#   collect()

lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
pr_sp = gather_ais_data(data = 'species list', lan_root = lan_root, onedrive_wd = onedrive_wd)

spi_fish |>
  dplyr::filter(!str_detect(OBSERVATION_DATE,"^9")) |>
  dplyr::filter(OBSERVATION_DATE <= lubridate::ymd("2025-01-16")) |>
  dplyr::mutate(species_name = str_to_lower(SPECIES_NAME)) |>
  dplyr::filter(species_name %in% all_of(str_to_lower(pr_sp$name))) |>
  sf::st_drop_geometry() |>
  dplyr::arrange(dplyr::desc(OBSERVATION_DATE))|>
  dplyr::slice(1) |>
  View()
