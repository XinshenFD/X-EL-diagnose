# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

getwd()

sample1_result <- read.xlsx("doctor_VS_AI/result_1/sample1_result.xlsx")
colnames(sample1_result )
table(sample1_result$EL_severity)
sample1_result$Real_label <- ifelse(sample1_result$EL_severity == "0",
                                    "No",
                                    "Yes")

sample1_result$Real_label
sample1_result$CXY
colnames(sample1_result)

calc_metrics <- function(predicted, actual, positive_label = "Yes") {

  tbl <- table(Predicted = predicted, Actual = actual)

  all_levels <- unique(c(as.character(predicted), as.character(actual)))

  if(!positive_label %in% all_levels) {
    return(c(Accuracy=NA, Sensitivity=NA, Specificity=NA))
  }

  TP <- sum(predicted == positive_label & actual == positive_label, na.rm = TRUE)
  TN <- sum(predicted != positive_label & actual != positive_label, na.rm = TRUE)
  FP <- sum(predicted == positive_label & actual != positive_label, na.rm = TRUE)
  FN <- sum(predicted != positive_label & actual == positive_label, na.rm = TRUE)

  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)

  return(c(Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity))
}

eval_cols <- c("AI", "CZX", "LLZ", "WJ", "XYB", "ZZR",
               "HQY", "JWN", "WYL", "ZYL", "CXY", "JYX", "CZ")

positive_class <- "Yes"

results_df <- data.frame(
  ID = character(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

for (col in eval_cols) {

  pred_vals <- sample1_result[[col]]
  real_vals <- sample1_result[["Real_label"]]

  metrics <- calc_metrics(pred_vals, real_vals, positive_label = positive_class)

  results_df <- rbind(results_df, data.frame(
    ID = col,
    Accuracy = round(metrics["Accuracy"], 4),
    Sensitivity = round(metrics["Sensitivity"], 4),
    Specificity = round(metrics["Specificity"], 4)
  ))
}

writexl::write_xlsx(results_df, "human_ai_result/result_df_1.xlsx")
