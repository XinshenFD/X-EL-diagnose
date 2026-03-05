# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

extra_data
summary(extra_data)

extra_data
summary(extra_data)
table(extra_data$surgery_follow)
extra_data$Group <- "Yes"
extra_validations <- dplyr::select(extra_data,
                            Age,
                            gender,
                            pre_S,
                            pre_C,
                            LogMAR,
                            AL,
                            K1,
                            K2,
                            ACD,
                            LT,
                            WTW,
                            Group)
summary(extra_validations)
extra_validations$gender <- ifelse(extra_validations$gender == "Male",
                                   1,
                                   0)
extra_validations$Group <- 1

extra_validations$WTW <- as.numeric(extra_validations$WTW)
extra_validations$LogMAR <- as.numeric(extra_validations$LogMAR)

table(is.na(predictors$LT))
table(is.na(predictors$ACD))

extra_validations$gender
summary(extra_validations)
extra_validations$K1
extra_validations <- subset(extra_validations,
       is.na(extra_validations$K1) == FALSE)
summary(extra_validations)

extra_contral <- read.xlsx("or_data/extra_control.xlsx")
extra_contral$Age <- (extra_contral$exam_date - extra_contral$birthday)/365.25
extra_contral$Group <- 0
extra_contral$LogMAR <- log10(1/extra_contral$V)
summary(extra_contral)
extra_contral$pre_C <- as.numeric(extra_contral$pre_C)
extra_contral <- dplyr::select(extra_contral,
                               Age,
                               gender,
                               pre_S,
                               pre_C,
                               LogMAR,
                               AL,
                               K1,
                               K2,
                               ACD,
                               LT,
                               WTW,
                               Group)

rownames(extra_validations)
rownames(extra_contral)
extra_vv <- rbind(extra_contral,extra_validations)
data_extras <- knnImputation(as.data.frame(extra_vv), k = 5)
rownames(data_extras)
data_extras$Group <- ifelse(data_extras$Group == 1,
                            "Yes",
                            "No")
data_extras$Group <- factor(data_extras$Group,
                            levels = c("No", "Yes"))
predict(model_log, data_extras)

log_probs <- predict(model_log, data_extras_new1202,type="prob")
knn_probs <- predict(model_knn, data_extras_new1202,type="prob")
rf_probs <- predict(model_rf, data_extras_new1202,type="prob")
SVM_probs <- predict(model_svm, data_extras_new1202,type="prob")
xgb_probs <- predict(model_xgb, data_extras_new1202,type="prob")
tree_probs <- predict(model_tree, data_extras_new1202,type="prob")

log_pred <- predict(model_log, data_extras_new1202)
knn_pred <- predict(model_knn, data_extras_new1202)
rf_pred <- predict(model_rf, data_extras_new1202)
SVM_pred <- predict(model_svm, data_extras_new1202)
xgb_pred <- predict(model_xgb, data_extras_new1202)
tree_pred <- predict(model_tree, data_extras_new1202)

data_extras_new1202$Group

log_cm <- confusionMatrix(log_pred, data_extras_new1202$Group, positive ="Yes")
knn_cm <- confusionMatrix(knn_pred, data_extras_new1202$Group, positive ="Yes")
rf_cm <- confusionMatrix(rf_pred, data_extras_new1202$Group, positive ="Yes")
svm_cm <- confusionMatrix(SVM_pred, data_extras_new1202$Group, positive ="Yes")
xgb_cm <- confusionMatrix(xgb_pred, data_extras_new1202$Group, positive ="Yes")
tree_cm <- confusionMatrix(tree_pred, data_extras_new1202$Group, positive ="Yes")

log_roc <- pROC::roc(data_extras_new1202$Group, log_probs$Yes)
knn_roc <- pROC::roc(data_extras_new1202$Group, knn_probs$Yes)
rf_roc <- pROC::roc(data_extras_new1202$Group, rf_probs$Yes)
svm_roc <- pROC::roc(data_extras_new1202$Group, SVM_probs$Yes)
xgb_roc <- pROC::roc(data_extras_new1202$Group, xgb_probs$Yes)
tree_roc <- pROC::roc(data_extras_new1202$Group, tree_probs$Yes)

plot(log_roc)
plot(knn_roc)
plot(rf_roc)
plot(svm_roc)
plot(xgb_roc)
plot(tree_roc)

roc_df <- bind_rows(
  data.frame(
    Model = "Logistic",
    TPR = log_roc$sensitivities,
    FPR = 1 - log_roc$specificities
  ),
  data.frame(
    Model = "KNN",
    TPR = knn_roc$sensitivities,
    FPR = 1 - knn_roc$specificities
  ),
  data.frame(
    Model = "Random Forest",
    TPR = rf_roc$sensitivities,
    FPR = 1 - rf_roc$specificities
  ),
  data.frame(
    Model = "SVM",
    TPR = svm_roc$sensitivities,
    FPR = 1 - svm_roc$specificities
  ),
  data.frame(
    Model = "XGBoost",
    TPR = xgb_roc$sensitivities,
    FPR = 1 - xgb_roc$specificities
  ),
  data.frame(
    Model = "Decision Tree",
    TPR = tree_roc$sensitivities,
    FPR = 1 - tree_roc$specificities
  )
)

auc_values <- c(
  Logistic = pROC::auc(log_roc),
  KNN = pROC::auc(knn_roc),
  RandomForest = pROC::auc(rf_roc),
  SVM = pROC::auc(svm_roc),
  XGBoost = pROC::auc(xgb_roc),
  Tree = pROC::auc(tree_roc)
)

names(auc_values) <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree")

roc_df$Model <- factor(roc_df$Model,
                       levels = names(auc_values),
                       labels = paste0(names(auc_values), " (AUC=", round(auc_values, 3), ")")
)

plotmodel_extra_plotmodel_extra_data <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.1, alpha=1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_minimal(base_size = 14) +
  labs(
    title = "ROC Curves for Models",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model"
  ) +
  scale_fill_jama()+
  scale_color_jama()+
  theme_classic(base_size = 20) +
  theme(axis.text=element_text(size=15, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=16),
        axis.text.x = element_text( )) +
  theme(

    legend.position = c(0.8, 0.25),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
plotmodel_extra_data
plotmodel_extra_plotmodel_extra_data
