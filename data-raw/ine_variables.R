## code to prepare `ine_variables` dataset
##
## Registry of INE indicators exposed by inedemogR. `id_table` is the real
## INE "idTable" identifier (verified live via ineapir::get_data_table()),
## `geo_var` is the name of the geographic dimension column that ineapir
## returns for that table when called with metanames = TRUE, and `geo_level`
## is the geographic granularity that table actually provides. Indicators
## with `id_table = NA` are registered but not yet wired to a live INE table;
## get_ine_demog() refuses to fetch them rather than guess an endpoint.
##
## Verified against the live INE API on 2026-07-25:
##   - 29005: "Cifras oficiales del padrón por municipio" (operation DPOP)
##   - 6506:  "Por lugar de residencia de la madre y sexo. Total nacional y
##             provincias" (operation MNPN)
##   - 6545:  "Por lugar de residencia y sexo. Total nacional y provincias"
##            (operation MNPD)

ine_variables <- tibble::tibble(
  indicator = c(
    "population_total", "births_total", "deaths_total"
  ),
  name = c(
    "Total Population", "Total Births", "Total Deaths"
  ),
  description = c(
    "Resident population by municipality and sex (Padron continuo).",
    "Live births by province of mother's residence and sex.",
    "Deaths by province of residence and sex."
  ),
  id_table = c(29005L, 6506L, 6545L),
  geo_var = c("Municipios", "Provincias", "Provincias"),
  geo_level = c("municipality", "province", "province")
)

usethis::use_data(ine_variables, overwrite = TRUE)
