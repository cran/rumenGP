
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rumenGP

<!-- badges: start -->

<!-- badges: end -->

**rumenGP** is an R package for importing, processing, visualizing,
fitting, comparing, and interpreting *in vitro* rumen gas production
data.

The package supports:

- ANKOM RF datasets
- Manually collected gas-volume datasets
- Pressure-based datasets (PSI or kPa)
- Twelve built-in kinetic models
- User-defined kinetic models
- Model comparison and ranking
- Treatment-level model evaluation
- Diagnostic and visualization tools

------------------------------------------------------------------------

## Installation

Development version:

``` r
# install.packages("devtools")

devtools::install_github(
  "araujorodrig-lab/rumenGP"
)
```

Or install from a local source:

``` r
devtools::install_local(
  "path/to/rumenGP"
)
```

------------------------------------------------------------------------

## Package Workflow

``` text
ANKOM RF
        ↓
process_ankom()
        ↓
     rumen_gp

Manual Gas Volume
        ↓
as_rumen_gp()
        ↓
     rumen_gp

Pressure Data
        ↓
as_rumen_gp()
        ↓
pressure_to_volume()
        ↓
     rumen_gp

             ↓

 Built-in Models
          +
  Custom Models

             ↓

 Comparison
 Ranking
 Diagnostics
 Visualization
```

------------------------------------------------------------------------

## Example Workflow: ANKOM Data

``` r
library(rumenGP)
```

### Load and Import Example Data

The package includes a small example dataset that can be used to learn
the workflow and test package functionality.

``` r
files <- example_data()

raw_data <- read_ankom(
  files$ankom
)

metadata <- read_metadata(
  files$metadata
)
```

### Inspect ANKOM Data

The ANKOM RF file contains pressure measurements collected during the
incubation period.

``` r
head(raw_data)
#> # A tibble: 6 × 52
#>   time_raw   `0`   `1`     `2`    `3`    `4`   `5`   `6`    `7`   `8`    `9`
#>   <chr>    <dbl> <dbl>   <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 10:29:34  14.2 0      0      0      0      0     0      0     0     0     
#> 2 10:39:34  14.2 0.873 -0.0728 0.0364 0.0364 0.910 0.218 -0.146 0.873 0.0364
#> 3 10:49:34  14.2 1.42  -0.109  0      0.0364 1.53  0.218  0.764 1.53  0.0364
#> 4 10:59:34  14.2 1.82  -0.109  0      0.0364 2.04  0.218  0.764 2.07  0     
#> 5 11:09:34  14.2 2.11  -0.146  0      0.0364 2.47  0.218  0.764 2.58  0     
#> 6 11:19:34  14.2 2.40  -0.146  0      0.0364 2.84  0.218  0.764 2.98  0     
#> # ℹ 41 more variables: `10` <dbl>, `11` <dbl>, `12` <dbl>, `13` <dbl>,
#> #   `14` <dbl>, `15` <dbl>, `16` <dbl>, `17` <dbl>, `18` <dbl>, `19` <dbl>,
#> #   `20` <lgl>, `21` <dbl>, `22` <dbl>, `23` <dbl>, `24` <dbl>, `25` <lgl>,
#> #   `26` <dbl>, `27` <lgl>, `28` <lgl>, `29` <lgl>, `30` <lgl>, `31` <lgl>,
#> #   `32` <lgl>, `33` <lgl>, `34` <lgl>, `35` <lgl>, `36` <lgl>, `37` <lgl>,
#> #   `38` <lgl>, `39` <lgl>, `40` <lgl>, `41` <lgl>, `42` <lgl>, `43` <lgl>,
#> #   `44` <lgl>, `45` <lgl>, `46` <lgl>, `47` <lgl>, `48` <lgl>, `49` <lgl>, …
```

### Inspect Metadata

The metadata table contains bottle identifiers, treatments, and
replicate information.

``` r
head(metadata)
#> # A tibble: 6 × 5
#>   Bottle  Head   Rep Treatment    pH
#>    <dbl> <dbl> <dbl> <chr>     <dbl>
#> 1      1     1     1 Plant_A    6.24
#> 2      2     2     2 Plant_A    6.22
#> 3      3     3     3 Plant_A    6.24
#> 4      4     4     4 Plant_A    6.3 
#> 5      5     5     5 Plant_A    6.15
#> 6      6     6     6 Plant_A    6.16
```

### Validate metadata

``` r
metadata <- validate_metadata(
  metadata
)
#> Metadata validation passed.
#> Heads: 24
#> Treatment: 5
```

### Process ANKOM data

``` r
gp <- process_ankom(
  raw_data,
  metadata,
  headspace_ml = 210,
  temperature_c = 39,
  zero_negative_pressure = TRUE
)
```

### Validate processed data

``` r
gp <- validate_ankom(
  gp
)
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
```

------------------------------------------------------------------------

## Example Workflow: Manual Gas Volume Data

``` r
manual_volume <- data.frame(

  Bottle = c(
    1,1,1,
    2,2,2
  ),

  Treatment = c(
    "Control","Control","Control",
    "Corn","Corn","Corn"
  ),

  Time = c(
    0,4,8,
    0,4,8
  ),

  Gas = c(
    0,20,40,
    0,35,60
  )

)

gp_manual <- as_rumen_gp(
  data = manual_volume,
  head_col = "Bottle",
  treatment_col = "Treatment",
  time_col = "Time",
  gas_col = "Gas"
)

head(gp_manual)
#>   Head Bottle Rep Treatment Time_h Gas_mL
#> 1    1      1   1   Control      0      0
#> 2    1      1   1   Control      4     20
#> 3    1      1   1   Control      8     40
#> 4    2      2   1      Corn      0      0
#> 5    2      2   1      Corn      4     35
#> 6    2      2   1      Corn      8     60
```

------------------------------------------------------------------------

## Example Workflow: Pressure Data

``` r
manual_pressure <- data.frame(

  Bottle = rep(
    1,
    10
  ),

  Time = c(
    0,2,4,6,8,
    12,16,24,36,48
  ),

  PSI = c(
    0,
    0.2,
    0.5,
    0.8,
    1.2,
    1.8,
    2.5,
    3.2,
    4.0,
    4.5
  )

)

gp_pressure <- as_rumen_gp(
  data = manual_pressure,
  head_col = "Bottle",
  time_col = "Time",
  pressure_col = "PSI",
  pressure_unit = "psi",
  headspace_volume = 60
)

head(gp_pressure)
#>   Head Bottle Rep Treatment Time_h    Gas_mL
#> 1    1      1   1   Unknown      0 0.0000000
#> 2    1      1   1   Unknown      2 0.7140855
#> 3    1      1   1   Unknown      4 1.7852138
#> 4    1      1   1   Unknown      6 2.8563421
#> 5    1      1   1   Unknown      8 4.2845132
#> 6    1      1   1   Unknown     12 6.4267698
```

Pressure measurements are automatically converted to gas volume using
the same conversion framework used by `process_ankom()`.

------------------------------------------------------------------------

## Model Fitting

``` r
groot_fit <- fit_groot(gp)
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5

brody_fit <- fit_brody(gp)
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5

gompertz_fit <- fit_gompertz(gp)
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
```

### Model summaries

``` r
summary(groot_fit)
#> 
#> Groot model summary
#> -------------------
#> Total bottles: 24
#> Successful fits: 24
#> Failed fits: 0
#> Low R-squared (< 0.90): 3
```

------------------------------------------------------------------------

## Compare Models

``` r
comparison <- compare_models(

  Groot = fit_groot(gp),

  Brody = fit_brody(gp),

  Gompertz = fit_gompertz(gp)

)
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5

comparison
#>      Model Bottles Successful_Fits Failed_Fits   Mean_R2 Mean_RMSE  Mean_RSS
#> 1    Groot      24              24           0 0.9134994  2.154301  491.4637
#> 2    Brody      24              23           1 0.9453304  3.604892 1076.8697
#> 3 Gompertz      24              23           1 0.9545965  4.216094 1837.5449
#>   Mean_AIC Mean_BIC Lambda_Boundary
#> 1 297.0687 306.1754               0
#> 2 393.6300 402.7918               0
#> 3 392.2245 401.3864               8
```

### Rank Models

``` r
rank_models(
  comparison
)
#>      Model Bottles Successful_Fits Failed_Fits   Mean_R2 Mean_RMSE  Mean_RSS
#> 1    Groot      24              24           0 0.9134994  2.154301  491.4637
#> 2    Brody      24              23           1 0.9453304  3.604892 1076.8697
#> 3 Gompertz      24              23           1 0.9545965  4.216094 1837.5449
#>   Mean_AIC Mean_BIC Lambda_Boundary Rank_R2 Rank_RMSE Rank_AIC Rank_BIC
#> 1 297.0687 306.1754               0       3         1        1        1
#> 2 393.6300 402.7918               0       2         2        3        3
#> 3 392.2245 401.3864               8       1         3        2        2
```

------------------------------------------------------------------------

## Treatment-Level Comparison

``` r
treatment_comparison <-
  compare_models_by_treatment(

    Groot = fit_groot(gp),

    Brody = fit_brody(gp),

    Gompertz = fit_gompertz(gp)

  )
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5

ranked_treatments <-
  rank_models_by_treatment(
    treatment_comparison
  )

best_model_by_treatment(
  ranked_treatments
)
#> # A tibble: 5 × 12
#>   Treatment Model Mean_R2 Mean_RMSE Mean_AIC Mean_BIC Rank_R2 Rank_RMSE Rank_AIC
#>   <chr>     <chr>   <dbl>     <dbl>    <dbl>    <dbl>   <int>     <int>    <int>
#> 1 BLANK     Groot   0.714      1.75     257.     266.       3         1        1
#> 2 Plant_A   Groot   0.968      2.33     281.     290.       1         1        1
#> 3 Plant_B   Groot   0.854      1.73     281.     290.       3         1        1
#> 4 Plant_C   Groot   0.982      2.11     309.     319.       1         1        1
#> 5 TMR       Groot   0.988      3.14     377.     386.       1         1        1
#> # ℹ 3 more variables: Rank_BIC <int>, Total_Rank <int>, Overall_Rank <int>
```

------------------------------------------------------------------------

## User-Defined Models

Researchers can fit their own kinetic equations using `fit_custom()`.

``` r
custom_fit <- fit_custom(

  data = gp,

  formula =
    Gas_mL ~
      A *
      (
        Time_h /
        (
          Time_h + K
        )
      ),

  start = list(
    A = 150,
    K = 10
  ),

  lower = c(
    A = 0,
    K = 0
  ),

  model_name = "Hyperbolic"

)
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5

summary(custom_fit)
#> 
#> Custom model summary
#> --------------------
#> Model name: Hyperbolic
#> 
#> Formula:
#> Gas_mL ~ A * (Time_h/(Time_h + K))
#> 
#> Total bottles: 24
#> Successful fits: 23
#> Failed fits: 1
#> Mean R-squared: 0.8965
#> Mean RMSE: 3.5146
#> Mean AIC: 384.8351
#> Mean BIC: 391.7065
```

Compare custom and built-in models:

``` r
compare_models(

  Groot = fit_groot(gp),

  Brody = fit_brody(gp),

  Hyperbolic = custom_fit

)
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
#> rumenGP data validation passed.
#> Observations: 1752
#> Heads: 24
#> Treatments: 5
#>        Model Bottles Successful_Fits Failed_Fits   Mean_R2 Mean_RMSE  Mean_RSS
#> 1      Groot      24              24           0 0.9134994  2.154301  491.4637
#> 2      Brody      24              23           1 0.9453304  3.604892 1076.8697
#> 3 Hyperbolic      24              23           1 0.8964862  3.514586 1053.1526
#>   Mean_AIC Mean_BIC Lambda_Boundary
#> 1 297.0687 306.1754               0
#> 2 393.6300 402.7918               0
#> 3 384.8351 391.7065               0
```

------------------------------------------------------------------------

## Visualization

### Raw gas production

``` r
plot_gp(
  gp,
  head = "1"
)
```

### Plot model fit

``` r
plot_fit(
  groot_fit,
  head = "1"
)
```

### Plot residuals

``` r
plot_residuals(
  groot_fit,
  head = "1"
)
```

### Treatment means

``` r
plot_treatment_mean(
  Groot = fit_groot(gp),
  Brody = fit_brody(gp),
  treatment = "Control"
)
```

------------------------------------------------------------------------

## Quality Control Workflow

``` text
Import data
    ↓
Validate metadata
    ↓
Create rumen_gp object
    ↓
Fit models
    ↓
Flag problematic bottles
    ↓
Inspect residuals
    ↓
Document exclusions
    ↓
Refit models
    ↓
Compare models
```

Example:

``` r
gp_clean <- exclude_heads(
  gp,
  heads = c("10"),
  reason = "Sensor malfunction"
)
```

------------------------------------------------------------------------

## Implemented Models

### Built-in Models

- Brody
- Dual Logistic
- EXP0
- EXPL
- Gompertz
- Groot
- LE0
- LEL
- Logistic
- Mitscherlich
- Michaelis-Menten
- Orskov and McDonald

### Model Notes

The Groot and generalized Michaelis-Menten models are mathematically
equivalent.

Parameter correspondence:

- VF = A
- b = K
- k = c

------------------------------------------------------------------------

## Major Features

### Data Import

- `process_ankom()`
- `as_rumen_gp()`

### Modeling

- 12 built-in models
- `fit_custom()`

### Diagnostics

- `summary()`
- `flag_model()`
- `exclude_heads()`

### Visualization

- `plot_gp()`
- `plot_fit()`
- `plot_residuals()`
- `plot_treatment_mean()`

### Model Comparison

- `compare_models()`
- `rank_models()`
- `compare_models_by_treatment()`
- `rank_models_by_treatment()`
- `best_model_by_treatment()`
- `model_win_frequency()`

------------------------------------------------------------------------

### Completed

- ✅ ANKOM RF workflow
- ✅ Generic data import
- ✅ Pressure-based workflows
- ✅ Twelve kinetic models
- ✅ Custom model framework
- ✅ Model comparison framework
- ✅ Treatment-level model ranking
- ✅ Diagnostic workflows
- ✅ Visualization tools
- ✅ Five package vignettes
- ✅ pkgdown website
- ✅ Clean R CMD check

### Development Status

rumenGP is currently maintained and developed as an open-source
framework for rumen gas production analysis. Future releases may include
additional modeling, reporting, and statistical workflows.

------------------------------------------------------------------------

## Citation

If you use **rumenGP** in research, please cite:

``` text
Rodrigues, A. A. and Mantovani, H. C.

rumenGP:
An R package for rumen gas production
kinetic modeling, model comparison,
and visualization.
```

(Citation information will be updated as the package develops.)
