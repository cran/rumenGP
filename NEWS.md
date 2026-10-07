
# rumenGP 0.2.0

## New Models

Added two new nonlinear gas production models:

* Burr XII
* Inverse Paralogistic

### Burr XII

Equation:

V(t) = VF [1 - (1 + (rt)^a)^(-p)]

Features:

* Flexible four-parameter sigmoidal model
* Accommodates diverse fermentation profiles
* Supports asymmetry and variable inflection behavior
* Useful for comparative model-selection studies

### Inverse Paralogistic

Equation:

V(t) = VF [1 + (rt)^(-a)]^(-a)

Features:

* Flexible three-parameter sigmoidal model
* Biologically interpretable asymptote
* Strong performance across diverse fermentation profiles

---

## Documentation Improvements

### Model Reference Vignette

Updated the "Model Equations Reference" vignette:

* Added Burr XII documentation
* Added Inverse Paralogistic documentation
* Added model-selection guidance
* Expanded model-equivalence discussion

### Model Interpretation Vignette

Updated the "Interpreting Gas Production Models" vignette:

* Added interpretation of Burr XII parameters
* Added interpretation of Inverse Paralogistic parameters
* Expanded discussion of shape parameters
* Added model-equivalence interpretation
* Added practical model-selection guidance

### Getting Started Vignette

Updated the "Getting Started with rumenGP" vignette:

* Added Burr XII examples
* Added Inverse Paralogistic examples
* Expanded model-comparison workflow
* Added model-selection recommendations

### Importing Non-ANKOM Data Vignette

Updated the "Importing Non-ANKOM Data" vignette:

* Added examples using newly implemented models
* Expanded model-comparison examples
* Added model-equivalence notes

---

## Model Equivalence Documentation

Documented the mathematical equivalence of:

* Groot
* Generalized Michaelis-Menten
* Log-logistic

Parameter correspondence:

* VF = A = VF
* b = K = 1/r
* k = c = a

Clarified that the Log-logistic formulation is
already represented through the existing Groot
and generalized Michaelis-Menten parameterizations.

---

## README Improvements

Updated package documentation:

* Added Burr XII
* Added Inverse Paralogistic
* Documented model equivalence
* Expanded model-comparison examples
* Added recommended model-selection workflow
* Updated built-in model count

---

## Package Content

Current built-in model library:

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
* Generalized Michaelis-Menten
* Ørskov and McDonald
* Burr XII
* Inverse Paralogistic

Total:

* 14 built-in kinetic models

---

## Quality Assurance

* Updated package documentation throughout
* Updated package website content
* Updated vignettes
* Updated examples
* Maintained clean package checks

R CMD check:

* 0 errors
* 0 warnings
* 0 notes
