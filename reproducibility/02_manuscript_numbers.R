# Every number reported in the SoftwareX manuscript, computed from the frozen
# inputs, and (optionally) the same numbers under version 0.1.0 so the effect
# of the corrections can be reported.
#
#   Rscript reproducibility/02_manuscript_numbers.R
#
# To include the 0.1.0 comparison, first install 0.1.0 into a separate
# library, e.g. from the CRAN archive:
#   install.packages("https://cran.r-project.org/src/contrib/Archive/inedemogR/inedemogR_0.1.0.tar.gz",
#                    lib = "~/inedemogR-0.1.0", repos = NULL)
# and set INEDEMOGR_OLD_LIB=~/inedemogR-0.1.0 before running this script.

numbers <- function() {
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  d <- "reproducibility/data"
  pop <- readRDS(file.path(d, "population_56945_100.rds"))$data
  deaths <- readRDS(file.path(d, "deaths_6545_6547_100.rds"))$data_provinces
  births_age <- readRDS(file.path(d, "births_by_age_6508_100.rds"))$data

  ex <- quiet(compute_exposure(pop, deaths))$data
  rates <- quiet(compute_death_rates(deaths, ex))
  lt <- quiet(build_life_tables(rates$mx_1x1))
  alt <- quiet(build_abridged_life_tables(rates$mx_1x1, rates$mx_5x1))

  get <- function(tab, prov, yr, age = 0, col = "ex") {
    tab[[col]][tab$province_name == prov & tab$year == yr & tab$age == age]
  }
  provs <- c("Madrid", "Barcelona", "Sevilla", "A Coruna")
  out <- list()
  add <- function(item, value) out[[length(out) + 1]] <<- tibble::tibble(item = item, value = value)

  for (p in provs) {
    add(paste("female e0 2024,", p), get(lt$fltper, p, 2024))
    add(paste("female e65 2024,", p), get(lt$fltper, p, 2024, 65))
  }
  add("male e0 2024, A Coruna", get(lt$mltper, "A Coruna", 2024))
  a <- alt$fltper
  add("abridged female e0 2024, A Coruna", a$ex[a$province_name == "A Coruna" & a$year == 2024 & a$age_start == 0])

  f <- lt$fltper
  coruna <- f[f$province_name == "A Coruna" & f$year == 2024, ]
  madrid <- f[f$province_name == "Madrid" & f$year == 2024, ]
  arr <- decompose_life_expectancy(coruna, madrid, "arriaga")
  pol <- decompose_life_expectancy(coruna, madrid, "pollard")
  gap <- madrid$ex[1] - coruna$ex[1]
  add("female e0 gap Madrid - A Coruna 2024", gap)
  add("Arriaga sum - gap", sum(arr$contribution) - gap)
  add("Pollard sum - gap", sum(pol$contribution) - gap)
  add("Arriaga-Pollard correlation", stats::cor(arr$contribution, pol$contribution))
  add("A Coruna female e0 gain 2006-2024", get(f, "A Coruna", 2024) - get(f, "A Coruna", 2006))

  asfr <- quiet(age_specific_fertility_rate(births_age, pop))
  tfr <- total_fertility_rate(asfr)
  mac <- mean_age_at_childbearing(asfr)
  for (p in provs) {
    add(paste("TFR 2024,", p), tfr$tfr[tfr$province_name == p & tfr$year == 2024])
    add(paste("MAC 2024,", p), mac$mac[mac$province_name == p & mac$year == 2024])
  }
  asdr <- rates$asdr
  for (p in provs) add(paste("female ASDR 2024 (per 100,000),", p),
                       asdr$asdr_female[asdr$province_name == p & asdr$year == 2024])
  dplyr::bind_rows(out)
}

if (identical(Sys.getenv("INEDEMOGR_RUN_OLD"), "1")) {
  library(inedemogR, lib.loc = Sys.getenv("INEDEMOGR_OLD_LIB"))
  stopifnot(packageVersion("inedemogR", lib.loc = Sys.getenv("INEDEMOGR_OLD_LIB")) == "0.1.0")
  saveRDS(numbers(), Sys.getenv("INEDEMOGR_OLD_OUT"))
  quit(save = "no")
}

devtools::load_all(quiet = TRUE)
new <- numbers()
names(new)[2] <- "v0.2.0"
old_lib <- Sys.getenv("INEDEMOGR_OLD_LIB")
if (nzchar(old_lib)) {
  tmp <- tempfile(fileext = ".rds")
  status <- system2("Rscript", "reproducibility/02_manuscript_numbers.R",
                    env = c("INEDEMOGR_RUN_OLD=1", paste0("INEDEMOGR_OLD_LIB=", old_lib),
                            paste0("INEDEMOGR_OLD_OUT=", tmp)))
  stopifnot(status == 0)
  old <- readRDS(tmp)
  names(old)[2] <- "v0.1.0"
  new <- dplyr::left_join(old, new, by = "item") |>
    dplyr::mutate(change = .data$v0.2.0 - .data$v0.1.0)
}
dir.create("reproducibility/output", showWarnings = FALSE)
readr::write_csv(new, "reproducibility/output/manuscript_numbers.csv")
print(as.data.frame(new), digits = 6)
