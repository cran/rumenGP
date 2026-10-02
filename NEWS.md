
# rumenGP 0.1.0

## Initial release

rumenGP provides tools for importing, processing,
visualizing, fitting, comparing, and interpreting
in vitro rumen gas production data.

### Data Import and Processing

* Added support for importing ANKOM RF data.
* Added metadata import and validation workflows.
* Added timestamp parsing utilities.
* Added pressure-to-volume conversion using the ideal gas law.
* Added standardized `rumen_gp` objects.
* Added quality-control and data-validation functions.
* Added tools for excluding problematic bottles while
  preserving exclusion records.

### Kinetic Models

Implemented the following gas production models:

* Brody
* Dual Logistic
* EXP0
* EXPL
* Gompertz
* Groot
* LE0
* LEL
* Logistic
* Mitscherlich
* Michaelis-Menten
* Orskov and McDonald

### Custom Models

* Added `fit_custom()` for user-defined nonlinear models.
* Supports custom formulas.
* Supports custom starting values.
* Supports parameter bounds.
* Integrates with plotting and model-comparison tools.

### Model Comparison

* Added model performance comparison tools.
* Added model ranking functions.
* Added treatment-level model comparison.
* Added treatment-level model ranking.
* Added best-model selection by treatment.
* Added model win frequency summaries.

### Visualization

* Added raw gas production plots.
* Added fitted curve visualization.
* Added treatment mean visualizations.
* Added residual diagnostics.
* Added model comparison visualizations.
* Added model ranking visualizations.
* Added dual-pool decomposition plots.

### Documentation

* Added extensive function documentation.
* Added parameter interpretation guidance.
* Added executable examples throughout the package.
* Added five package vignettes:

  * Getting Started
  * Importing Data
  * Custom Models
  * Model Equations
  * Interpreting Models

### Website

* Added a pkgdown website.
* Added searchable reference documentation.
* Added rendered examples and figures.
* Added article and vignette integration.

### Quality Assurance

* Achieved clean `R CMD check`:

  * 0 errors
  * 0 warnings
  * 0 notes
