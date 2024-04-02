extract <- get_nhgis_ts_data(
  table = nhgis_ts_tables[[1]],
  year = c(1980, 1990, 2000)
)

ts_minmax_years <- ts_tables |>
  unnest(years, names_sep = "_") |>
  mutate(
    years_int = readr::parse_integer(
      stringr::str_extract(years_description, "[:digit:]+$")
    )
  ) |>
  group_by(
    name,
    geographic_integration,
    sequence
  ) |>
  summarise(
    min_year = min(years_int),
    max_year = max(years_int)
  )

ts_denominators <- ts_tables |>
  unnest(time_series, names_sep = "_") |>
  select(!c(years, geog_levels)) |>
  mutate(
    sequence_start = as.integer(sequence),
    variable = paste0(name, time_series_name),
    denominator = stringr::str_extract(
      time_series_description,
      "([:alnum:]|[:blank:])+(?=:)"),
    .before = everything()
  ) |>
  filter(
    !stringr::str_detect(time_series_description, "--"),
    !stringr::str_detect(time_series_description, "~"),
    sequence_start != 103
    # time_series_sequence < 3,
    # !stringr::str_detect(time_series_description, "years"),
    # !stringr::str_detect(time_series_description, "Under"),
    # !stringr::str_detect(time_series_description, "less than")
  ) |>
  left_join(ts_minmax_years)
  # filter(
  #   stringr::str_detect(time_series_description, "Total"),
  #   !stringr::str_detect(time_series_description, "Total races tallied")
  # ) |>
  # View()
  distinct(
    denominator_level1,
    denominator_level2,
    .keep_all = TRUE
  )# |>
  # View()

  # # Nominal
  # "Persons" ~ "A00AA",
  # "Persons: Male" ~ "A08AA",
  # "Total Families" ~ "A68AA",
  # # Standardized
  # "Persons" ~ "CL8AA",
  #
  # "Total Families" ~ "CM5AA"

# ts_tables |>
#   unnest(time_series, names_sep = "_") |>
#   View()
