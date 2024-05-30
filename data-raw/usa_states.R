## code to prepare `divisions` dataset goes here
usa_states <- data.frame(
  "STATE" = c(state.name, "District of Columbia")
  "STUSPS" = c(state.abb, "DC"),
  "division" = factor(c(state.division, "South Atlantic"), levels = levels(state.division))
  "region" = factor(c(state.region, "South"), levels = levels(state.region))
)

usethis::use_data(usa_states, overwrite = TRUE)
