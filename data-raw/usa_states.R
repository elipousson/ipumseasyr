## code to prepare `divisions` dataset goes here
usa_states <- data.frame(
  "STATE" = state.name,
  "STUSPS" = state.abb,
  "division" = state.division,
  "region" = state.region
)

usethis::use_data(usa_states, overwrite = TRUE)
