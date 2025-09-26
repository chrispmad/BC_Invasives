# Read in provincial shapefile.
bc = bcmaps::bc_bound()

# Read in priority list of AIS species (excel file), use it to update selectInput
pr_sp = vroom::vroom('priority_species_table.csv') |>
  purrr::set_names(snakecase::to_snake_case) |>
  dplyr::mutate(name = ifelse(name == 'Oriental weather loach',
                              'Oriental weatherfish',
                              name))

# Read in occurrence data for species (this is updated each time the 'publish_app.R'
# script is run)
occ_dat = sf::read_sf('occ_dat.gpkg')

native_range_occs = sf::read_sf("native_range_occs.gpkg")
eradicated_occs = sf::read_sf("eradicated_occs.gpkg")
anecdotal_occs = sf::read_sf("anecdotal_occs.gpkg")

nr_regs = sf::read_sf('nr_regions.gpkg') |>
  dplyr::mutate(reg_name = stringr::str_remove(REGION_NAME, " Natural.*")) |>
  dplyr::select(reg_name)

# Lake name and region coordinate lookup table.
lk_crd_tbl = readRDS("lake_name_coordinate_lookup_tbl.rds") |>
  # Add row numbers. This helps us when there are multiple rows with the
  # same lake name in the same region (a rare case!)
  dplyr::mutate(row_id = dplyr::row_number()) |>
  dplyr::mutate(codename = paste0(wb_name,"_",row_id))
