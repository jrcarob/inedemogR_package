# Reproduction bundle for the SoftwareX article (inedemogR 0.2.0)

Everything here runs offline from the frozen inputs in `data/`. Run the
scripts from the package root (the folder containing `DESCRIPTION`); they
load the package source with `devtools::load_all()`.

| Script | Produces |
|---|---|
| `00_freeze_inputs.R` | `data/`: INE inputs and INE's provincial life tables, each with its retrieval time. Only downloads files that are absent. |
| `01_validation.R` | `output/validation_summary.md` and CSVs: benchmark of e0/e65 against INE (2002-2024, all provinces, both sexes and total), age decomposition of the differences, sensitivity to the Lexis-triangle split and the old-age model, Poisson bootstrap for 2024 and for Figure 3. About 30 minutes. |
| `02_manuscript_numbers.R` | `output/manuscript_numbers.csv`: every number quoted in the article. With `INEDEMOGR_OLD_LIB` pointing to a library holding 0.1.0, also the 0.1.0 values and the change. |
| `03_figures.R` | `figs/`: the article's figures, at 600 dpi, with explicit years. |
| `04_rate_benchmark.R` | `output/rate_benchmark_*.csv`: grouped death rates against INE's. |
| `05_fertility_benchmark.R` | `output/fertility_benchmark_*.csv`: TFR and mean age at childbearing against INE's provincial series, 1975-2024, tolerance 0.01. |

## Frozen inputs (`data/`)

| File | INE source | Retrieved (UTC) |
|---|---|---|
| `population_56945_100.rds` | Table 56945, population on 1 January by province, sex and single age | 2026-10-05 08:35 |
| `deaths_6545_6547_100.rds` | Tables 6545 and 6547, deaths by province, sex and age | 2026-10-05 08:41 |
| `births_by_age_6508_100.rds` | Table 6508, births by province and mother's age | 2026-10-05 08:44 |
| `births_6506_100.rds` | Table 6506, births by province | 2026-10-05 08:55 |
| `demog_totals_2023.rds` | Birth and death totals via `get_ine_demog()`, 2023 | 2026-10-05 08:56 |
| `ine_life_tables_27155.rds` | Table 27155, INE provincial life tables (methodology: <https://www.ine.es/metodologia/t20/t2020319a.pdf>) | 2026-10-05 08:34 |
| `ine_idb_1478.rds`, `ine_idb_1581.rds` | Tables 1478 (TFR) and 1581 (mean age at childbearing), Indicadores Demograficos Basicos (methodology: <https://ine.es/metodologia/t20/metodologia_idb.pdf>) | 2026-10-05 09:45 |

The exact request and time are stored in each object's `provenance`
attribute: `attr(readRDS(file), "provenance")`.

## Environment

Produced with R 4.5.2 on macOS. Package versions of the dependencies are
those current on CRAN on 5 October 2026.
