# Offline tests of the INE parsers on fixtures that copy the shape of real
# Tempus3 responses (series names from table 6547, retrieved 2026-10-05).
# INE returns cells with no deaths as series with an empty `Data` array.

ine_series <- function(name, values) {
  list(
    Nombre = name,
    Data = lapply(names(values), function(y) list(Anyo = as.integer(y), Valor = values[[y]]))
  )
}
death_name <- function(prov, sex, age) {
  sprintf("%s. Defunción. %s. %s. Lugar de residencia. Dato base. ", prov, sex, age)
}

test_that("deaths-by-age parser reads ages, the open interval and infants", {
  raw <- list(
    ine_series(death_name("Ceuta", "Total", "Menores de un año"), list(`2024` = 6)),
    ine_series(death_name("Ceuta", "Total", "1 año"), list(`2024` = 1)),
    ine_series(death_name("Ceuta", "Total", "45 años"), list(`2024` = 2)),
    ine_series(death_name("Ceuta", "Total", "100 y más años"), list(`2024` = 3)),
    ine_series(death_name("Ceuta", "Total", "2 años"), list()) # zero deaths
  )
  tidy <- parse_deaths_by_age(raw)
  expect_equal(sort(tidy$age), c(0, 1, 45, 100))
  expect_equal(tidy$deaths[tidy$age == 100], 3)
  expect_true(all(tidy$ine_code == "51"))
  expect_false(2 %in% tidy$age) # empty series yield no row: a structural zero
})

test_that("a suppressed sex-specific count is recovered as Total minus the other sex", {
  raw <- list(
    ine_series(death_name("Ceuta", "Total", "80 años"), list(`2024` = 5)),
    ine_series(death_name("Ceuta", "Hombres", "80 años"), list(`2024` = 3)),
    ine_series(death_name("Ceuta", "Mujeres", "80 años"), list()),
    ine_series(death_name("Ceuta", "Total", "81 años"), list(`2024` = 4)),
    ine_series(death_name("Ceuta", "Hombres", "81 años"), list(`2024` = 2)),
    ine_series(death_name("Ceuta", "Mujeres", "81 años"), list(`2024` = 2))
  )
  clean <- suppressMessages(clean_deaths_provinces_age(parse_deaths_by_age(raw)))
  expect_equal(clean$female[clean$age == 80], 2)
  expect_equal(clean$male[clean$age == 80], 3)
  expect_equal(clean$female + clean$male, clean$total)
  expect_equal(unique(clean$nuts3_code), "ES630")
})

test_that("an unrecoverable count stays missing and fails validation", {
  raw <- list(
    ine_series(death_name("Ceuta", "Hombres", "80 años"), list(`2024` = 3)),
    ine_series(death_name("Ceuta", "Mujeres", "80 años"), list())
  )
  clean <- suppressWarnings(suppressMessages(clean_deaths_provinces_age(parse_deaths_by_age(raw))))
  expect_true(is.na(clean$female))
  expect_false(suppressWarnings(validate_deaths_age(clean))$passed)
})

test_that("population CSV parser keeps 100+ and drops overlapping coarse open groups", {
  csv <- tibble::tibble(
    Provincias = "51 Ceuta",
    Sexo = c("Mujeres", "Mujeres", "Mujeres", "Mujeres", "Hombres", "Total"),
    `Edad simple` = c("0 años", "99 años", "100 y más años",
                      "85 y más años", "0 años", "Todas las edades"),
    Periodo = "1 de enero de 2024",
    Total = c("1.234", "10", "4", "900", "1.300", "84.000")
  )
  tidy <- parse_population_csv(csv)
  expect_equal(sort(tidy$age[tidy$sex == "Female"]), c(0, 99, 100))
  expect_equal(tidy$population[tidy$sex == "Female" & tidy$age == 0], 1234)
  expect_equal(unique(tidy$year), 2024)
  expect_false(any(tidy$population == 84000)) # grand total excluded
})

test_that("clean_population warns when INE totals disagree with female + male", {
  tidy <- tibble::tibble(
    ine_code = "51", age = 0, year = 2024,
    sex = c("Female", "Male", "Total"), population = c(100, 110, 250)
  )
  expect_warning(out <- clean_population(tidy), "INE total != Female\\+Male")
  expect_equal(out$total, 250)
  expect_equal(out$province_name, "Ceuta")
})
