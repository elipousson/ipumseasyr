ts_tables <- list_nhgis_ts_tables()

nhgis_ts_tables <- ts_tables[["name"]]

nhgis_ts_tables <- rlang::set_names(nhgis_ts_tables, ts_tables[["description"]])

usethis::use_data(nhgis_ts_tables, overwrite = TRUE)
