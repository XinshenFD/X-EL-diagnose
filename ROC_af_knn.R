# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

models <- list(Logistic = model_log, KNN = model_knn, RF = model_rf,
               SVM = model_svm, XGB = model_xgb,
               Tree = model_tree)

results <- resamples(models)

log_probs <- predict(model_log, validation_set,type="prob")
knn_probs <- predict(model_knn, validation_set,type="prob")
rf_probs <- predict(model_rf, validation_set,type="prob")
SVM_probs <- predict(model_svm, validation_set,type="prob")
xgb_probs <- predict(model_xgb, validation_set,type="prob")
tree_probs <- predict(model_tree, validation_set,type="prob")

log_pred <- predict(model_log, validation_set)
knn_pred <- predict(model_knn, validation_set)
rf_pred <- predict(model_rf, validation_set)
SVM_pred <- predict(model_svm, validation_set)
xgb_pred <- predict(model_xgb, validation_set)
tree_pred <- predict(model_tree, validation_set)

log_cm <- confusionMatrix(log_pred, validation_set$Group, positive ="Yes")
knn_cm <- confusionMatrix(knn_pred, validation_set$Group, positive ="Yes")
rf_cm <- confusionMatrix(rf_pred, validation_set$Group, positive ="Yes")
svm_cm <- confusionMatrix(SVM_pred, validation_set$Group, positive ="Yes")
xgb_cm <- confusionMatrix(xgb_pred, validation_set$Group, positive ="Yes")
tree_cm <- confusionMatrix(tree_pred, validation_set$Group, positive ="Yes")

summary(validation_set$Group)
summary(log_probs$Yes)
log_roc <- pROC::roc(validation_set$Group, log_probs$Yes)
knn_roc <- pROC::roc(validation_set$Group, knn_probs$Yes)
rf_roc <- pROC::roc(validation_set$Group, rf_probs$Yes)
svm_roc <- pROC::roc(validation_set$Group, SVM_probs$Yes)
xgb_roc <- pROC::roc(validation_set$Group, xgb_probs$Yes)
tree_roc <- pROC::roc(validation_set$Group, tree_probs$Yes)

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

plotmodel_validation_set <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
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
