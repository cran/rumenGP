
# rumenGP Development Roadmap

## Vision

Develop a scientifically rigorous and user-friendly R package for analyzing in vitro rumen gas production data.

---

# Completed

## Data Import

- [x] read_ankom()
- [x] read_metadata()

## Processing

- [x] parse_ankom_time()
- [x] process_ankom()

## Validation

- [x] validate_metadata()
- [x] validate_ankom()

## Gompertz Workflow

- [x] fit_gompertz()
- [x] summary.gompertz_fit()
- [x] flag_model()
- [x] plot_gp()
- [x] plot_gompertz_fit()
- [x] plot_residuals()

## Logistic Workflow

- [x] fit_logistic()
- [x] summary.logistic_fit()

## Mitscherlich Workflow

- [x] fit_mitscherlich()
- [x] summary.mitscherlich_fit()

## Model Comparison

- [x] compare_models()
- [x] rank_models()

## Visualization

- [x] plot_all_fits()
- [x] plot_treatment_mean()
- [x] plot_model_comparison_treatment()

## Quality Control

- [x] exclude_heads()

## Documentation

- [x] Example dataset
- [x] README workflow

---

# Current Results

Current ranking for example dataset:

1. Mitscherlich
2. Gompertz
3. Logistic

---

# Next Development Steps

## Priority

- [ ] fit_france()
- [ ] summary.france_fit()

## Model Comparison

- [ ] Add France model to compare_models()
- [ ] Add France model to rank_models()

## Visualization

- [ ] plot_model_comparison_all()
- [ ] treatment-level publication figure

## Quality Control

- [ ] Generic residual diagnostics dashboard

---

# Future Models

- [ ] Michaelis-Menten
- [ ] Logistic-Exponential
- [ ] Exponential
- [ ] Modified Gompertz

---

# Long-Term Goals

- [ ] Automatic model selection
- [ ] Biological plausibility screening
- [ ] Mixed-effects workflow
- [ ] Package website
- [ ] Unit tests
- [ ] CRAN submission
