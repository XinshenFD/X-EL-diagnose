# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

summary(model_no_ACD_LT_xgb)
model_no_ACD_LT_xgb$finalModel$feature_names

data_predictors_af_logistic_no_ACD_LT_train
class(data_predictors_af_logistic_no_ACD_LT_train)

prediction_threshold <- 0.3657517
model_features <- model_no_ACD_LT_xgb$finalModel$feature_names

get_patient_explanation_E <- function(model, patient_data_df, patient_id, top_n = 2, threshold = 0.3657517) {

  model_features <- model$feature_names

  missing_features <- setdiff(model_features, names(patient_data_df))
  if (length(missing_features) > 0) stop(paste("Missing features:", paste(missing_features, collapse = ", ")))

  patient_matrix <- as.matrix(patient_data_df[, model_features, drop = FALSE])

  raw_score <- predict(model, patient_matrix, type = "prob")

  prob <- 1 - raw_score

  diagnosis_label <- ifelse(prob > threshold, "Yes", "No")

  shap_result <- shap.values(xgb_model = model, X_train = patient_matrix)
  shap_scores <- shap_result$shap_score

  contributions <- data.frame(
    Feature = colnames(patient_matrix),
    SHAP_Value = as.numeric(shap_scores[1, ]),
    Feature_Value = as.numeric(patient_matrix[1, ])
  )

  if (diagnosis_label == "No") {
    return(list(
      Patient_ID = patient_id,
      Prediction_Prob = round(prob, 4),
      Diagnosis = diagnosis_label,
      Top_Risk_Features = character(0),
      Clinical_Reason = "Model assesses as low risk for ectopia lentis.",
      Full_Data = data.frame()
    ))
  }

  risk_contributors <- contributions %>%
    filter(SHAP_Value < 0) %>%
    arrange(desc(abs(SHAP_Value)))

  if (nrow(risk_contributors) == 0) {
    return(list(
      Patient_ID = patient_id,
      Prediction_Prob = round(prob, 4),
      Diagnosis = diagnosis_label,
      Top_Risk_Features = character(0),
      Clinical_Reason = "Model predicts risk, but it is driven by baseline values; no single significant feature identified.",
      Full_Data = data.frame()
    ))
  }

  top_features <- risk_contributors$Feature[1:min(top_n, nrow(risk_contributors))]

  other_feat_name <- setdiff(top_features, "LogMAR")[1]
  other_feat_val <- if(!is.na(other_feat_name)) round(patient_data_df[, other_feat_name], 2) else NA
  logmar_val <- round(patient_data_df[, "LogMAR"], 2)

  reason_text <- case_when(

    all(c("AL", "pre_S") %in% top_features) ~
      paste0("Indicates a mismatch between Axial Length (AL=", round(patient_data_df[,"AL"], 2),
             ") and Refraction (S=", round(patient_data_df[,"pre_S"], 2),
             "), suggesting a risk of lens abnormality."),

    all(c("AL", "LogMAR") %in% top_features) ~
      paste0("Mismatch between Axial Length (AL=", round(patient_data_df[,"AL"], 2),
             ") and Visual Acuity (LogMAR=", logmar_val,
             "); visual impairment is disproportionate to axial length alone."),

    all(c("AL", "pre_C") %in% top_features) ~
      paste0("Significant Astigmatism (C=", round(patient_data_df[,"pre_C"], 2),
             ") is the dominant risk indicator, strongly suggesting lens tilt or subluxation."),

    (all(c("AL", "K1") %in% top_features) || all(c("AL", "K2") %in% top_features)) ~
      paste0("Abnormal ratio/mismatch between Axial Length (AL=", round(patient_data_df[,"AL"], 2),
             ") and Corneal Curvature (K)."),

    all(c("pre_S", "LogMAR") %in% top_features) ~
      paste0("Refraction (S=", round(patient_data_df[,"pre_S"], 2),
             ") and Visual Acuity (LogMAR=", logmar_val,
             ") are the primary risk indicators; visual impairment exceeds expectations."),

    "LogMAR" %in% top_features & !is.na(other_feat_name) ~
      paste0(other_feat_name, " (=", other_feat_val,
             ") and patient LogMAR (=", logmar_val,
             "), indicating high risk of Ectopia Lentis."),

    "AL" %in% top_features[1] ~
      paste0("Axial Length (AL=", round(patient_data_df[,"AL"], 2), ") is the primary risk indicator."),

    any(c("K1", "K2") %in% top_features[1]) ~
      paste0("Abnormal Corneal Curvature is the primary source of risk."),

    "pre_C" %in% top_features[1] ~
      paste0("High Astigmatism (C=", round(patient_data_df[,"pre_C"], 2), ") suggests possible lens tilt."),

    "pre_S" %in% top_features[1] ~
      paste0("Abnormal Spherical Equivalent/Refraction is the primary source of risk."),

    "LogMAR" %in% top_features[1] ~
      paste0("Significant decrease in visual acuity (LogMAR=", logmar_val, ") is the primary source of risk."),

    TRUE ~ paste("Primary risk derived from:", paste(top_features, collapse = " and "))
  )

  feature_map <- c(
    "pre_S"  = "Sphere",
    "pre_C"  = "Cylinder",
    "LogMAR" = "Best Corrected Visual Acuity",
    "AL"     = "Axial Length"
  )

  top_features_display <- sapply(top_features, function(x) {
    if (x %in% names(feature_map)) feature_map[[x]] else x
  })

  top_features_display <- unname(top_features_display)

  return(list(
    Patient_ID = patient_id,
    Prediction_Prob = round(prob, 4),
    Diagnosis = diagnosis_label,
    Top_Risk_Features = top_features_display,
    Clinical_Reason = reason_text,
    Full_Data = risk_contributors
  ))
}

batch_explain_patients_E <- function(model, data_df, id_col = NULL, top_n = 2, threshold = 0.3657517) {

  model_features <- model$feature_names

  missing <- setdiff(model_features, names(data_df))
  if (length(missing) > 0) stop(paste("REDACTED", paste(missing, collapse=", ")))

  if (!is.null(id_col) && id_col %in% names(data_df)) {
    p_ids <- data_df[[id_col]]
  } else {
    p_ids <- 1:nrow(data_df)
  }

  results_list <- purrr::map2(
    .x = split(data_df, seq_len(nrow(data_df))),
    .y = p_ids,
    .f = function(df_row, pid) {
      get_patient_explanation_E(model, df_row, pid, top_n, threshold)
    }
  )

  for (i in seq_along(results_list)) {
    if (length(results_list[[i]]$Top_Risk_Features) == 0) {
      results_list[[i]]$Top_Risk_Features_Str <- ""
    } else {
      results_list[[i]]$Top_Risk_Features_Str <- paste(results_list[[i]]$Top_Risk_Features, collapse = ", ")
    }
    results_list[[i]]$Top_Risk_Features <- NULL
    results_list[[i]]$Full_Data <- NULL
  }

  return(dplyr::bind_rows(results_list))
}

colnames(sample1)

result_test<- batch_explain_patients_E(model = model_no_ACD_LT_xgb$finalModel,
                                     data_df = sample1)
write.xlsx(result_test,
           "result_test_E.xlsx")

result_test<- batch_explain_patients_E(model = model_no_ACD_LT_xgb$finalModel,
                                       data_df = sample2)
write.xlsx(result_test,
           "result_test_E-2.xlsx")
