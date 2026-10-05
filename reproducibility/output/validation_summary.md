# Validation summary (inedemogR 0.2.0)

Generated 2026-10-05 09:17:47 UTC from frozen inputs in reproducibility/data. Tolerance fixed in advance: 0.25 years.

## Coverage
| tables_built | tables_flagged | province_years_withheld | withheld_years | built_years |
| --- | --- | --- | --- | --- |
| 3588 | 0 | 1404 | 1975-2001 | 2002-2024 |

## Benchmark against INE provincial life tables (2002-2024)
| measure | sex | n | mean_signed | mean_abs | p95_abs | max_abs | within_tol |
| --- | --- | --- | --- | --- | --- | --- | --- |
| e0 | female | 1196 | 0.028 | 0.048 | 0.129 | 0.643 | 0.986 |
| e0 | male | 1196 | 0.002 | 0.031 | 0.077 | 0.674 | 0.992 |
| e0 | total | 1196 | 0.015 | 0.033 | 0.085 | 0.552 | 0.987 |
| e65 | female | 1196 | 0.031 | 0.051 | 0.138 | 0.686 | 0.985 |
| e65 | male | 1196 | 0.004 | 0.036 | 0.089 | 0.758 | 0.990 |
| e65 | total | 1196 | 0.018 | 0.037 | 0.093 | 0.604 | 0.987 |

| measure | size_class | n | mean_signed | mean_abs | p95_abs | max_abs | within_tol |
| --- | --- | --- | --- | --- | --- | --- | --- |
| e0 | <200k |  621 | 0.053 | 0.072 | 0.291 | 0.674 | 0.932 |
| e0 | 200k-1M | 2001 | 0.007 | 0.030 | 0.077 | 0.202 | 1.000 |
| e0 | >1M |  966 | 0.008 | 0.030 | 0.077 | 0.160 | 1.000 |
| e65 | <200k |  621 | 0.060 | 0.081 | 0.324 | 0.758 | 0.926 |
| e65 | 200k-1M | 2001 | 0.008 | 0.033 | 0.084 | 0.211 | 1.000 |
| e65 | >1M |  966 | 0.010 | 0.033 | 0.087 | 0.188 | 1.000 |

By INE's closing age group:
| measure | ine_open_age | n | mean_signed | mean_abs | p95_abs | max_abs | within_tol |
| --- | --- | --- | --- | --- | --- | --- | --- |
| e0 | 90+ (Ceuta, Melilla) |  138 | 0.148 | 0.182 | 0.508 | 0.674 | 0.696 |
| e0 | 95+ (50 provinces) | 3450 | 0.010 | 0.031 | 0.085 | 0.202 | 1.000 |
| e65 | 90+ (Ceuta, Melilla) |  138 | 0.169 | 0.206 | 0.561 | 0.758 | 0.667 |
| e65 | 95+ (50 provinces) | 3450 | 0.012 | 0.035 | 0.094 | 0.211 | 1.000 |

Remaining life expectancy at 90, Ceuta and Melilla:
| province_name | n | mean_inedemogR_e90 | mean_ine_e90 | mean_diff |
| --- | --- | --- | --- | --- |
| Ceuta | 69 | 4.318 | 3.542 | 0.776 |
| Melilla | 69 | 4.427 | 3.558 | 0.870 |

Largest differences:
| province_name | year | sex | measure | inedemogR | ine | diff |
| --- | --- | --- | --- | --- | --- | --- |
| Melilla | 2013 | male | e65 | 17.264 | 16.507 | 0.758 |
| Melilla | 2009 | female | e65 | 21.146 | 20.460 | 0.686 |
| Melilla | 2013 | male | e0 | 78.913 | 78.239 | 0.674 |
| Melilla | 2024 | female | e65 | 22.711 | 22.054 | 0.657 |
| Melilla | 2009 | female | e0 | 83.540 | 82.898 | 0.643 |
| Melilla | 2008 | female | e65 | 20.687 | 20.055 | 0.632 |
| Melilla | 2024 | female | e0 | 85.821 | 85.203 | 0.619 |
| Melilla | 2017 | male | e65 | 17.765 | 17.157 | 0.608 |
| Melilla | 2024 | total | e65 | 21.192 | 20.588 | 0.604 |
| Melilla | 2024 | male | e65 | 19.437 | 18.855 | 0.582 |

## Age decomposition of the inedemogR - INE difference in e0 (Arriaga)
| band | mean_signed | mean_abs | max_abs |
| --- | --- | --- | --- |
| age 0 |  0.000 | 0.000 | 0.002 |
| ages 1-79 | -0.008 | 0.008 | 0.056 |
| ages 80+ |  0.023 | 0.038 | 0.712 |

## Attribution: same comparison with unsmoothed tables (same province-sex-years)
| tables | measure | size_class | n | mean_signed | mean_abs | p95_abs | max_abs | within_tol |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Kannisto (package default) | e0 | <200k |  368 | 0.040 | 0.064 | 0.211 | 0.643 | 0.959 |
| Kannisto (package default) | e0 | 200k-1M | 1332 | 0.007 | 0.032 | 0.082 | 0.202 | 1.000 |
| Kannisto (package default) | e0 | >1M |  644 | 0.008 | 0.031 | 0.082 | 0.160 | 1.000 |
| Kannisto (package default) | e65 | <200k |  368 | 0.044 | 0.071 | 0.226 | 0.686 | 0.957 |
| Kannisto (package default) | e65 | 200k-1M | 1332 | 0.008 | 0.035 | 0.090 | 0.211 | 1.000 |
| Kannisto (package default) | e65 | >1M |  644 | 0.009 | 0.034 | 0.091 | 0.188 | 1.000 |
| observed rates to 100+ | e0 | <200k |  368 | 0.051 | 0.079 | 0.241 | 1.090 | 0.951 |
| observed rates to 100+ | e0 | 200k-1M | 1332 | 0.015 | 0.038 | 0.109 | 0.289 | 0.999 |
| observed rates to 100+ | e0 | >1M |  644 | 0.024 | 0.041 | 0.123 | 0.369 | 0.992 |
| observed rates to 100+ | e65 | <200k |  368 | 0.056 | 0.086 | 0.265 | 1.164 | 0.943 |
| observed rates to 100+ | e65 | 200k-1M | 1332 | 0.017 | 0.042 | 0.118 | 0.307 | 0.999 |
| observed rates to 100+ | e65 | >1M |  644 | 0.028 | 0.046 | 0.137 | 0.431 | 0.989 |

## Sensitivity: Lexis-triangle split
| scenario | measure | size_class | n | mean_signed | mean_abs | max_abs |
| --- | --- | --- | --- | --- | --- | --- |
| 45% of deaths in lower triangle | e0 | <200k |  615 | -0.008 | 0.008 | 0.010 |
| 45% of deaths in lower triangle | e0 | 200k-1M | 2001 | -0.008 | 0.008 | 0.009 |
| 45% of deaths in lower triangle | e0 | >1M |  966 | -0.008 | 0.008 | 0.009 |
| 45% of deaths in lower triangle | e65 | <200k |  615 | -0.009 | 0.009 | 0.011 |
| 45% of deaths in lower triangle | e65 | 200k-1M | 2001 | -0.009 | 0.009 | 0.010 |
| 45% of deaths in lower triangle | e65 | >1M |  966 | -0.009 | 0.009 | 0.010 |
| 55% of deaths in lower triangle | e0 | <200k |  621 |  0.008 | 0.008 | 0.032 |
| 55% of deaths in lower triangle | e0 | 200k-1M | 2001 |  0.008 | 0.008 | 0.010 |
| 55% of deaths in lower triangle | e0 | >1M |  966 |  0.008 | 0.008 | 0.009 |
| 55% of deaths in lower triangle | e65 | <200k |  621 |  0.009 | 0.009 | 0.035 |
| 55% of deaths in lower triangle | e65 | 200k-1M | 2001 |  0.009 | 0.009 | 0.011 |
| 55% of deaths in lower triangle | e65 | >1M |  966 |  0.009 | 0.009 | 0.010 |
| 85% of infant deaths in lower triangle | e0 | <200k |  621 |  0.000 | 0.000 | 0.002 |
| 85% of infant deaths in lower triangle | e0 | 200k-1M | 2001 |  0.000 | 0.000 | 0.001 |
| 85% of infant deaths in lower triangle | e0 | >1M |  966 |  0.000 | 0.000 | 0.001 |
| 85% of infant deaths in lower triangle | e65 | <200k |  621 |  0.000 | 0.000 | 0.000 |
| 85% of infant deaths in lower triangle | e65 | 200k-1M | 2001 |  0.000 | 0.000 | 0.000 |
| 85% of infant deaths in lower triangle | e65 | >1M |  966 |  0.000 | 0.000 | 0.000 |

## Sensitivity: old-age model
| scenario | measure | size_class | n | mean_signed | mean_abs | max_abs |
| --- | --- | --- | --- | --- | --- | --- |
| Y fixed at 80 | e0 | <200k |  621 |  0.000 | 0.000 | 0.000 |
| Y fixed at 80 | e0 | 200k-1M | 2001 |  0.000 | 0.002 | 0.061 |
| Y fixed at 80 | e0 | >1M |  966 |  0.005 | 0.009 | 0.067 |
| Y fixed at 80 | e65 | <200k |  621 |  0.000 | 0.000 | 0.000 |
| Y fixed at 80 | e65 | 200k-1M | 2001 |  0.001 | 0.002 | 0.068 |
| Y fixed at 80 | e65 | >1M |  966 |  0.006 | 0.011 | 0.074 |
| Y fixed at 85 | e0 | <200k |  621 |  0.017 | 0.047 | 0.319 |
| Y fixed at 85 | e0 | 200k-1M | 2001 |  0.010 | 0.026 | 0.150 |
| Y fixed at 85 | e0 | >1M |  966 |  0.018 | 0.025 | 0.137 |
| Y fixed at 85 | e65 | <200k |  621 |  0.018 | 0.052 | 0.346 |
| Y fixed at 85 | e65 | 200k-1M | 2001 |  0.011 | 0.029 | 0.159 |
| Y fixed at 85 | e65 | >1M |  966 |  0.020 | 0.028 | 0.153 |
| Y fixed at 90 | e0 | <200k |  621 |  0.005 | 0.031 | 0.225 |
| Y fixed at 90 | e0 | 200k-1M | 2001 |  0.005 | 0.017 | 0.102 |
| Y fixed at 90 | e0 | >1M |  966 |  0.007 | 0.014 | 0.081 |
| Y fixed at 90 | e65 | <200k |  621 |  0.005 | 0.035 | 0.266 |
| Y fixed at 90 | e65 | 200k-1M | 2001 |  0.006 | 0.018 | 0.118 |
| Y fixed at 90 | e65 | >1M |  966 |  0.008 | 0.016 | 0.088 |
| Y fixed at 95 | e0 | <200k |  621 | -0.012 | 0.025 | 0.219 |
| Y fixed at 95 | e0 | 200k-1M | 2001 | -0.005 | 0.013 | 0.118 |
| Y fixed at 95 | e0 | >1M |  966 | -0.003 | 0.010 | 0.113 |
| Y fixed at 95 | e65 | <200k |  621 | -0.013 | 0.028 | 0.253 |
| Y fixed at 95 | e65 | 200k-1M | 2001 | -0.005 | 0.014 | 0.126 |
| Y fixed at 95 | e65 | >1M |  966 | -0.004 | 0.011 | 0.122 |
| fit window 85-99 | e0 | <200k |  621 | -0.066 | 0.190 | 1.161 |
| fit window 85-99 | e0 | 200k-1M | 2001 | -0.027 | 0.085 | 0.448 |
| fit window 85-99 | e0 | >1M |  966 | -0.006 | 0.024 | 0.397 |
| fit window 85-99 | e65 | <200k |  621 | -0.073 | 0.213 | 1.310 |
| fit window 85-99 | e65 | 200k-1M | 2001 | -0.029 | 0.095 | 0.523 |
| fit window 85-99 | e65 | >1M |  966 | -0.007 | 0.026 | 0.428 |
| no smoothing (observed rates to 100+) | e0 | <200k |  368 |  0.011 | 0.029 | 0.448 |
| no smoothing (observed rates to 100+) | e0 | 200k-1M | 1332 |  0.008 | 0.016 | 0.133 |
| no smoothing (observed rates to 100+) | e0 | >1M |  644 |  0.017 | 0.021 | 0.316 |
| no smoothing (observed rates to 100+) | e65 | <200k |  368 |  0.012 | 0.032 | 0.478 |
| no smoothing (observed rates to 100+) | e65 | 200k-1M | 1332 |  0.009 | 0.018 | 0.162 |
| no smoothing (observed rates to 100+) | e65 | >1M |  644 |  0.019 | 0.023 | 0.366 |

## Stability: Poisson bootstrap of 2024 deaths (B = 200 )
| sex | size_class | median_sd_e0 | max_sd_e0 | median_sd_e65 | max_sd_e65 |
| --- | --- | --- | --- | --- | --- |
| female | <200k | 0.426 | 0.704 | 0.278 | 0.619 |
| female | 200k-1M | 0.233 | 0.401 | 0.152 | 0.263 |
| female | >1M | 0.142 | 0.187 | 0.104 | 0.126 |
| male | <200k | 0.456 | 0.773 | 0.297 | 0.644 |
| male | 200k-1M | 0.267 | 0.355 | 0.164 | 0.266 |
| male | >1M | 0.158 | 0.200 | 0.110 | 0.134 |

## Figure 3 decomposition (A Coruna vs Madrid, females, 2024)
| method | gap | residual | ages_0_64 | ages_65_79 | ages_80_plus |
| --- | --- | --- | --- | --- | --- |
| arriaga | 0.9122 |  0e+00 | 0.0392 | 0.3075 | 0.5655 |
| pollard | 0.9122 | -4e-04 | 0.0383 | 0.3026 | 0.5717 |

| method | gap_lo | gap_hi | residual_lo | residual_hi | ages_0_64_lo | ages_0_64_hi | ages_65_79_lo | ages_65_79_hi | ages_80_plus_lo | ages_80_plus_hi |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| arriaga | 0.6394 | 1.2198 |  0e+00 | 0e+00 | -0.1835 | 0.2811 | 0.1399 | 0.4683 | 0.4381 | 0.7086 |
| pollard | 0.6394 | 1.2198 | -7e-04 | 2e-04 | -0.1813 | 0.2774 | 0.1380 | 0.4603 | 0.4426 | 0.7151 |

