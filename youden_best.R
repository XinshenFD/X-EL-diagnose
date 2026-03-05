# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

log_no_LT_best <- pROC::coords(log_no_LT_roc, "best", best.method = "youden")
knn_no_LT_best <- pROC::coords(knn_no_LT_roc, "best", best.method = "youden")
rf_no_LT_best <- pROC::coords(rf_no_LT_roc, "best", best.method = "youden")
svm_no_LT_best <- pROC::coords(svm_no_LT_roc, "best", best.method = "youden")
xgb_no_LT_best <- pROC::coords(xgb_no_LT_roc, "best", best.method = "youden")
tree_no_LT_best <- pROC::coords(tree_no_LT_roc, "best", best.method = "youden")

log_no_ACD_best <- pROC::coords(log_no_ACD_roc, "best", best.method = "youden")
knn_no_ACD_best <- pROC::coords(knn_no_ACD_roc, "best", best.method = "youden")
rf_no_ACD_best <- pROC::coords(rf_no_ACD_roc, "best", best.method = "youden")
svm_no_ACD_best <- pROC::coords(svm_no_ACD_roc, "best", best.method = "youden")
xgb_no_ACD_best <- pROC::coords(xgb_no_ACD_roc, "best", best.method = "youden")
tree_no_ACD_best <- pROC::coords(tree_no_ACD_roc, "best", best.method = "youden")

log_no_ACD_LT_best <- pROC::coords(log_no_ACD_LT_roc, "best", best.method = "youden")
knn_no_ACD_LT_best <- pROC::coords(knn_no_ACD_LT_roc, "best", best.method = "youden")
rf_no_ACD_LT_best <- pROC::coords(rf_no_ACD_LT_roc, "best", best.method = "youden")
svm_no_ACD_LT_best <- pROC::coords(svm_no_ACD_LT_roc, "best", best.method = "youden")
xgb_no_ACD_LT_best <- pROC::coords(xgb_no_ACD_LT_roc, "best", best.method = "youden")
tree_no_ACD_LT_best <- pROC::coords(tree_no_ACD_LT_roc, "best", best.method = "youden")

log_best <- pROC::coords(log_roc, "best", best.method = "youden")
knn_best <- pROC::coords(knn_roc, "best", best.method = "youden")
rf_best <- pROC::coords(rf_roc, "best", best.method = "youden")
svm_best <- pROC::coords(svm_roc, "best", best.method = "youden")
xgb_best <- pROC::coords(xgb_roc, "best", best.method = "youden")
tree_best <- pROC::coords(tree_roc, "best", best.method = "youden")

summary(xgb_probs_no_LT)
xgb_no_LT_best
xgb_no_LT_pred_best <- ifelse(xgb_probs_no_LT$Yes > xgb_no_LT_best$threshold, "Yes", "No")
xgb_no_ACD_LT_pred_best <- ifelse(xgb_probs_no_ACD_LT$Yes > xgb_no_ACD_LT_best$threshold, "Yes", "No")
xgb_no_ACD_pred_best <- ifelse(xgb_probs_no_ACD$Yes > xgb_no_ACD_best$threshold, "Yes", "No")
xgb_pred_best <- ifelse(xgb_probs$Yes > xgb_best$threshold, "Yes", "No")

xgb_no_LT_pred_best <- factor(xgb_no_LT_pred_best, levels = c("No", "Yes"))
xgb_no_ACD_LT_pred_best <- factor(xgb_no_ACD_LT_pred_best, levels = c("No", "Yes"))
xgb_no_ACD_pred_best <- factor(xgb_no_ACD_pred_best, levels = c("No", "Yes"))
xgb_pred_best <- factor(xgb_pred_best, levels = c("No", "Yes"))

identify_misclassified <- function(true_labels, predicted_labels, validation_data, model_name) {
  misclassified <- which(true_labels != predicted_labels)

  if(length(misclassified) > 0) {
    misclassified_cases <- validation_data[misclassified, ]
    misclassified_cases$True_Label <- true_labels[misclassified]
    misclassified_cases$Predicted_Label <- predicted_labels[misclassified]
    misclassified_cases$Model <- model_name
    misclassified_cases$Row_Index <- rownames(validation_data)[misclassified]

    return(misclassified_cases)
  } else {
    return(NULL)
  }
}

length(data_extras$Group)
length(xgb_no_LT_pred_best)
xgb_no_LT_pred_best_misclassified <- identify_misclassified(data_extras_new1202$Group, xgb_no_LT_pred_best ,
                                                            data_extras_new1202, "xgb_no_LT_pred_best")
xgb_no_ACD_pred_best_misclassified <- identify_misclassified(data_extras_new1202$Group, xgb_no_ACD_pred_best ,
                                                            data_extras_new1202, "xgb_no_ACD_pred_best")
xgb_no_ACD_LT_pred_best_misclassified
xgb_no_ACD_LT_pred_best_misclassified <- identify_misclassified(data_extras_new1202$Group, xgb_no_ACD_LT_pred_best ,
                                                             data_extras_new1202, "xgb_no_ACD_LT_pred_best")

xgb_pred_best_misclassified <- identify_misclassified(data_extras_new1202$Group, xgb_pred_best ,
                                                                data_extras_new1202, "xgb_pred_best")

summary(xgb_pred_best_misclassified)
summary(xgb_no_ACD_LT_pred_best_misclassified )
summary(xgb_no_ACD_pred_best_misclassified)
summary(xgb_no_LT_pred_best_misclassified )

xgb_pred_best <- factor(xgb_pred_best,
                        levels = c("No", "Yes"))
confusionMatrix(data_extras$Group, xgb_pred_best, positive ="Yes")

table(validation_set$Group)
table(data_extras$Group)
extra_data$peidui <- paste0(extra_data$AL, extra_data$K1, extra_data$KSE)

data_extras$peidui <- paste0(data_extras$AL, data_extras$K1, data_extras$KSE)

xgb_no_ACD_LT_pred_best_misclassified$peidui <- paste0(xgb_no_ACD_LT_pred_best_misclassified$AL,
                                             xgb_no_ACD_LT_pred_best_misclassified$K1,
                                             xgb_no_ACD_LT_pred_best_misclassified$KSE)
xgb_pred_best_misclassified
table(data_extras$peidui %in% extra_data$peidui)
table(xgb_no_ACD_LT_pred_best_misclassified$peidui %in% extra_data$peidui)

which(extra_data$peidui %in% xgb_no_ACD_LT_pred_best_misclassified$peidui == TRUE)

toczx <- extra_data[c(1, 20,  49,  52,
                      73,  91, 125, 146, 150, 171, 179, 456), ]

extra_contral_copy <- read.xlsx("or_data/extra_control.xlsx")

extra_contral_copy$peidui <- paste0(extra_contral_copy$AL, extra_contral_copy$K1, extra_contral_copy$KSE)
table(xgb_no_ACD_LT_pred_best_misclassified$peidui %in% extra_contral_copy$peidui)

which(extra_contral_copy$peidui %in% xgb_no_ACD_LT_pred_best_misclassified$peidui == TRUE)

toczx_2 <- extra_contral_copy[c(2   ,3 , 20  ,62  ,74 ,430), ]

write.xlsx(toczx, "or_data/result/toczx.xlsx")
write.xlsx(toczx_2, "or_data/result/toczx_2.xlsx")

xgb_no_ACD_LT_pred_best
data_extras$label_predict <- xgb_no_ACD_LT_pred_best
result_data_1020 <- left_join(data_extras, extra_data,
                         by = "peidui")

result_data_1020 <- subset(result_data_1020,
                           is.na(result_data_1020$name) == FALSE)

result_data_1020
write.xlsx(result_data_1020, "data/result_data_1020.xlsx", rownames = TRUE)

length(data_extras_new1202$Age)
