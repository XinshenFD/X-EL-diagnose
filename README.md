# Sanitized Code Package

This repository contains a cleaned and anonymized R code package for submission.

## Included Modules (Methods-Only)
- `dependencies.R`: package loading.
- `preprocess_raw_cohorts.R`: raw cohort loading and initial normalization.
- `clean_features_and_impute.R`: feature cleaning and numeric preprocessing.
- `merge_and_standardize_training_cohorts.R`: cohort merge and schema harmonization.
- `data_preparation_for_modeling.R`: final modeling table assembly.
- `build_train.R`: model training (logistic, KNN, RF, SVM, XGBoost, tree).
- `build_validation.R`: internal validation and metrics.
- `extra_validation.R`: external validation.
- `ablation_no_lt_models.R`: ablation model set without LT.
- `ablation_no_acd_models.R`: ablation model set without ACD.
- `ablation_no_acd_lt_models.R`: ablation model set without LT and ACD.
- `evaluation_internal_full_plots.R`: ROC/PR/calibration/DCA/CIC for full model.
- `evaluation_internal_no_lt_plots.R`: plotting for no-LT ablation.
- `evaluation_internal_no_acd_plots.R`: plotting for no-ACD ablation.
- `evaluation_internal_no_acd_lt_plots.R`: plotting for no-ACD-LT ablation.
- `evaluation_external_ablation_plots.R`: external plotting and model comparison.
- `subgroup_auc_analysis_external.R`: subgroup AUC analysis (external cohort).
- `feature_selection_lasso_full.R`: variable selection (full setting).
- `feature_selection_lasso_ablation.R`: variable selection (ablation settings).
- `youden_thresholds.R`: Youden index threshold selection.
- `xgboost_explanation_rules.R`: patient-level XGBoost explanation logic.
- `shap_waterfall_plot.R`: SHAP waterfall plotting.
- `export_model_artifacts.R`: model artifact export for deployment.
- `app.R`: Shiny inference interface.

## Data
- `data/sample1_anonymized.xlsx`
- `data/sample1_anonymized.csv`

## Sanitization Notes
- Non-English comments and direct identifiers were removed/replaced.
- Unrelated scripts (doctor comparison, ad hoc tables, exploratory extras) were removed.
- File names were standardized to descriptive English labels.
