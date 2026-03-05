# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

summary(model_no_ACD_LT_xgb)
model_no_ACD_LT_xgb$finalModel

data_predictors_af_logistic_no_ACD_LT_train
class(data_predictors_af_logistic_no_ACD_LT_train)

prediction_threshold <- 0.3657517
model_features <- model_no_ACD_LT_xgb$finalModel$feature_names

get_patient_explanation <- function(model, patient_data_df, patient_id, top_n = 2, threshold = 0.3657517) {

  model_features <- model$feature_names

  missing_features <- setdiff(model_features, names(patient_data_df))
  if (length(missing_features) > 0) stop(paste("REDACTED", paste(missing_features, collapse = ", ")))

  patient_matrix <- as.matrix(patient_data_df[, model_features, drop = FALSE])

  raw_score <- predict(model, patient_matrix,type="prob")

  prob <- 1-raw_score

  diagnosis_label <- ifelse((1-raw_score) > threshold, "Yes", "No")

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
      Clinical_Reason = "REDACTED",
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
      Clinical_Reason = "REDACTED",
      Full_Data = data.frame()
    ))
  }

  top_features <- risk_contributors$Feature[1:min(top_n, nrow(risk_contributors))]

  reason_text <- case_when(

    all(c("AL", "pre_S") %in% top_features) ~
      paste0("REDACTED", round(patient_data_df[,"AL"], 2), "REDACTED", round(patient_data_df[,"pre_S"], 2), "REDACTED"),

    (all(c("AL", "K1") %in% top_features) || all(c("AL", "K2") %in% top_features)) ~
      paste0("REDACTED", round(patient_data_df[,"AL"], 2), "REDACTED"),

    all(c("pre_S", "LogMAR") %in% top_features) ~
      paste0("REDACTED", round(patient_data_df[,"pre_S"], 2), "REDACTED", round(patient_data_df[,"LogMAR"], 2), "REDACTED"),

    "AL" %in% top_features[1] ~ paste0("REDACTED", round(patient_data_df[,"AL"], 2), "REDACTED"),
    any(c("K1", "K2") %in% top_features[1]) ~ paste0("REDACTED"),
    "pre_C" %in% top_features[1] ~ paste0("REDACTED", round(patient_data_df[,"pre_C"], 2), "REDACTED"),
    "pre_S" %in% top_features[1] ~ paste0("REDACTED"),
    "LogMAR" %in% top_features[1] ~ paste0("REDACTED"),

    TRUE ~ paste("REDACTED", paste(top_features, collapse = "REDACTED"))
  )

  return(list(
    Patient_ID = patient_id,
    Prediction_Prob = round(prob, 4),
    Diagnosis = diagnosis_label,
    Top_Risk_Features = top_features,
    Clinical_Reason = reason_text,
    Full_Data = risk_contributors
  ))
}

batch_explain_patients <- function(model, data_df, id_col = NULL, top_n = 2, threshold = 0.3657517) {

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
      get_patient_explanation(model, df_row, pid, top_n, threshold)
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

result_test<- batch_explain_patients(model = model_no_ACD_LT_xgb$finalModel,
                       data_df = sample1)
write.xlsx(result_test,
           "result_test.xlsx")
