# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

getwd()
library(openxlsx)
sample2_result <- read.xlsx("doctor_VS_AI/result_2/sample2.xlsx")
colnames(sample2_result )
table(sample2_result$EL_severity)
sample2_result$Real_label <- ifelse(sample2_result$EL_severity == "0",
                                    "No",
                                    "Yes")

sample2_result$Real_label
sample2_result$CXY
colnames(sample2_result)

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
colnames(sample2_result)
sample2_result$AI <- sample2_result$predicted_label

results_df_2 <- data.frame(
  ID = character(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

for (col in eval_cols) {

  pred_vals <- sample2_result[[col]]
  real_vals <- sample2_result[["Real_label"]]

  metrics <- calc_metrics(pred_vals, real_vals, positive_label = positive_class)

  results_df_2 <- rbind(results_df_2, data.frame(
    ID = col,
    Accuracy = round(metrics["Accuracy"], 4),
    Sensitivity = round(metrics["Sensitivity"], 4),
    Specificity = round(metrics["Specificity"], 4)
  ))
}

writexl::write_xlsx(results_df_2, "human_ai_result/result_df_2.xlsx")
