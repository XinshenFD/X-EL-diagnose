# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

library(SHAPforxgboost)

model_no_ACD_LT_xgb

summary(model_no_ACD_LT_xgb)
model_no_ACD_LT_xgb$finalModel

data_predictors_af_logistic_no_ACD_LT_train
class(data_predictors_af_logistic_no_ACD_LT_train)

get_patient_explanation <- function(model, data_matrix, patient_index, top_n = 2) {

  patient_data <- data_matrix[patient_index, , drop = FALSE]

  shap_result <- shap.values(xgb_model = model, X_train = patient_data)
  shap_scores <- shap_result$shap_score

  contributions <- data.frame(
    Feature = names(shap_scores),
    SHAP_Value = as.numeric(shap_scores[1,]),
    Feature_Value = as.numeric(patient_data[1,])
  )

  positive_contributors <- contributions %>%
    filter(SHAP_Value > 0) %>%
    arrange(desc(SHAP_Value))

  if (nrow(positive_contributors) == 0) {
    return(list(Diagnosis = "No", Reason = "REDACTED"))
  }

  top_features <- positive_contributors$Feature[1:min(top_n, nrow(positive_contributors))]

  reason_text <- case_when(

    all(c("AL", "K1") %in% top_features) | all(c("AL", "K2") %in% top_features) ~
      "REDACTED",

    all(c("pre_S", "LogMAR") %in% top_features) ~
      "REDACTED",

    "AL" %in% top_features[1] ~
      paste0("REDACTED", round(patient_data[,"AL"], 2), "REDACTED"),

    any(c("K1", "K2") %in% top_features[1]) ~
      "REDACTED",

    "pre_C" %in% top_features[1] ~
      "REDACTED",

    TRUE ~ paste("REDACTED", paste(top_features, collapse = "REDACTED"))
  )

  raw_score <- predict(model, patient_data, outputmargin = TRUE)
  prob <- 1 / (1 + exp(-raw_score))

  return(list(
    Patient_ID = patient_index,
    Prediction_Prob = round(prob, 4),
    Top_Risk_Features = top_features,
    Clinical_Reason = reason_text,
    Full_Data = positive_contributors
  ))
}

xgb_model <- model_no_ACD_LT_xgb$finalModel

input_matrix <- as.matrix(data_predictors_af_logistic_no_ACD_LT_train[2,])

result <- get_patient_explanation(
  model = xgb_model,
  data_matrix = input_matrix,
  patient_index = 1
)

if (result$Prediction_Prob > 0.5) {
  cat("========================================\n")
  cat("REDACTED")
  cat(sprintf("REDACTED", result$Prediction_Prob * 100))
  cat("REDACTED", paste(result$Top_Risk_Features, collapse=", "), "\n")
  cat("REDACTED", result$Clinical_Reason, "\n")
  cat("========================================\n")
} else {
  cat("REDACTED")
}
