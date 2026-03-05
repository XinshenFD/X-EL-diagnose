# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

colnames(data_predictors)
x_lasso <- as.matrix(data_predictors[, c(1:10, 13)])
y_lasso <- as.matrix(data_predictors[, 14])
f_lasso <- glmnet(x_lasso, y_lasso, family="binomial", alpha=1)
plot(f_lasso,
     xvar = "lambda",
     label = TRUE)

cvfit=cv.glmnet(x_lasso, y_lasso,family="binomial", intercept=F, alpha=1)
plot(cvfit)

coef1 <- predict(f_lasso, s=cvfit$lambda.1se, type = "coefficients")
coef2 <- predict(f_lasso, s=cvfit$lambda.min, type = "coefficients")
coef1
coef2

model_logistic_no_ACD <- glm(Group ~ pre_S+pre_C+LogMAR+AL+K1+LT,
                                data = data_predictors,
                                family = binomial(link = "logit"))

summary(model_logistic_no_ACD)

data_predictors_af_logistic_no_ACD <- dplyr::select(data_predictors_af_logistic,
                                                       pre_S,
                                                       pre_C,LogMAR, AL, K1,Group, LT)
data_predictors_af_logistic_no_ACD <- cbind(data_predictors_af_logistic_no_ACD,
                                            data_predictors$K2)
colnames(data_predictors_af_logistic_no_ACD)[8] <- "K2"

data_predictors_af_logistic_no_ACD_train <- data_predictors_af_logistic_no_ACD[training, ]
colnames(data_predictors_af_logistic_no_ACD_train)
summary(yunnan_add_na)
yunnan_add_na_trainging <-dplyr::select(yunnan_add_na,
                                        pre_S,
                                        pre_C,
                                        LogMAR,
                                        AL,
                                        K1,

                                        Group,
                                        LT,
                                        K2)
data_predictors_af_logistic_no_ACD_train <- rbind(data_predictors_af_logistic_no_ACD_train,
                                                     yunnan_add_na_trainging)

data_predictors_af_logistic_no_ACD_train
length(data_predictors_af_logistic_no_ACD_train$pre_S)

model_no_ACD_log <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_train,
                             method = "glm", family = "binomial",
                             trControl = control, metric = "ROC")
model_no_ACD_log

model_no_ACD_knn <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_train,
                             method = "knn", trControl = control, metric = "ROC")
model_no_ACD_knn

model_no_ACD_rf <- train(Group ~ .,  data = data_predictors_af_logistic_no_ACD_train,
                            method = "rf", trControl = control, metric = "ROC")
model_no_ACD_rf

model_no_ACD_svm <- train(Group ~ ., data =  data_predictors_af_logistic_no_ACD_train,
                             method = "svmRadial", trControl = control, metric = "ROC")
model_no_ACD_svm

model_no_ACD_xgb <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_train,
                             method = "xgbTree", trControl = control, metric = "ROC")
model_no_ACD_xgb

model_no_ACD_tree <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_train,
                              method = "rpart", trControl = control, metric = "ROC")
model_no_ACD_tree

log_probs_no_ACD <- predict(model_no_ACD_log, validation_set,type="prob")
knn_probs_no_ACD <- predict(model_no_ACD_knn, validation_set,type="prob")
rf_probs_no_ACD <- predict(model_no_ACD_rf, validation_set,type="prob")
SVM_probs_no_ACD <- predict(model_no_ACD_svm, validation_set,type="prob")
xgb_probs_no_ACD <- predict(model_no_ACD_xgb, validation_set,type="prob")
tree_probs_no_ACD <- predict(model_no_ACD_tree, validation_set,type="prob")

log_no_ACD_pred <- predict(model_no_ACD_log, validation_set)
knn_no_ACD_pred <- predict(model_no_ACD_knn, validation_set)
rf_no_ACD_pred <- predict(model_no_ACD_rf, validation_set)
SVM_no_ACD_pred <- predict(model_no_ACD_svm, validation_set)
xgb_no_ACD_pred <- predict(model_no_ACD_xgb, validation_set)
tree_no_ACD_pred <- predict(model_no_ACD_tree, validation_set)

log_no_ACD_cm <- confusionMatrix(log_no_ACD_pred, validation_set$Group, positive ="Yes")
knn_no_ACD_cm <- confusionMatrix(knn_no_ACD_pred, validation_set$Group, positive ="Yes")
rf_no_ACD_cm <- confusionMatrix(rf_no_ACD_pred, validation_set$Group, positive ="Yes")
svm_no_ACD_cm <- confusionMatrix(SVM_no_ACD_pred, validation_set$Group, positive ="Yes")
xgb_no_ACD_cm <- confusionMatrix(xgb_no_ACD_pred, validation_set$Group, positive ="Yes")
tree_no_ACD_cm <- confusionMatrix(tree_no_ACD_pred, validation_set$Group, positive ="Yes")

log_no_ACD_roc <- pROC::roc(validation_set$Group, log_probs_no_ACD$Yes)
knn_no_ACD_roc <- pROC::roc(validation_set$Group, knn_probs_no_ACD$Yes)
rf_no_ACD_roc <- pROC::roc(validation_set$Group, rf_probs_no_ACD$Yes)
svm_no_ACD_roc <- pROC::roc(validation_set$Group, SVM_probs_no_ACD$Yes)
xgb_no_ACD_roc <- pROC::roc(validation_set$Group, xgb_probs_no_ACD$Yes)
tree_no_ACD_roc <- pROC::roc(validation_set$Group, tree_probs_no_ACD$Yes)

plot(log_no_ACD_roc)
plot(knn_no_ACD_roc)
plot(rf_no_ACD_roc)
plot(svm_no_ACD_roc)
plot(xgb_no_ACD_roc)
plot(tree_no_ACD_roc)

roc_no_ACD_df <- bind_rows(
  data.frame(
    Model = "Logistic",
    TPR = log_no_ACD_roc$sensitivities,
    FPR = 1 - log_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "KNN",
    TPR = knn_no_ACD_roc$sensitivities,
    FPR = 1 - knn_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "Random Forest",
    TPR = rf_no_ACD_roc$sensitivities,
    FPR = 1 - rf_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "SVM",
    TPR = svm_no_ACD_roc$sensitivities,
    FPR = 1 - svm_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "XGBoost",
    TPR = xgb_no_ACD_roc$sensitivities,
    FPR = 1 - xgb_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "Decision Tree",
    TPR = tree_no_ACD_roc$sensitivities,
    FPR = 1 - tree_no_ACD_roc$specificities
  )
)

auc_no_ACD_values <- c(
  Logistic = pROC::auc(log_no_ACD_roc),
  KNN = pROC::auc(knn_no_ACD_roc),
  RandomForest = pROC::auc(rf_no_ACD_roc),
  SVM = pROC::auc(svm_no_ACD_roc),
  XGBoost = pROC::auc(xgb_no_ACD_roc),
  Tree = pROC::auc(tree_no_ACD_roc)
)

names(auc_no_ACD_values) <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree")

roc_no_ACD_df$Model <- factor(roc_no_ACD_df$Model,
                                 levels = names(auc_no_ACD_values),
                                 labels = paste0(names(auc_no_ACD_values), " (AUC=", round(auc_no_ACD_values, 3), ")")
)

plotmodel_no_ACD_validation_set <- ggplot(roc_no_ACD_df, aes(x = FPR, y = TPR, color = Model)) +
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

log_probs_no_ACD <- predict(model_no_ACD_log, data_extras_new1202,type="prob")
knn_probs_no_ACD <- predict(model_no_ACD_knn, data_extras_new1202,type="prob")
rf_probs_no_ACD <- predict(model_no_ACD_rf, data_extras_new1202,type="prob")
SVM_probs_no_ACD <- predict(model_no_ACD_svm, data_extras_new1202,type="prob")
xgb_probs_no_ACD <- predict(model_no_ACD_xgb, data_extras_new1202,type="prob")
tree_probs_no_ACD <- predict(model_no_ACD_tree, data_extras_new1202,type="prob")

log_no_ACD_pred <- predict(model_no_ACD_log, data_extras_new1202)
knn_no_ACD_pred <- predict(model_no_ACD_knn, data_extras_new1202)
rf_no_ACD_pred <- predict(model_no_ACD_rf, data_extras_new1202)
SVM_no_ACD_pred <- predict(model_no_ACD_svm, data_extras_new1202)
xgb_no_ACD_pred <- predict(model_no_ACD_xgb, data_extras_new1202)
tree_no_ACD_pred <- predict(model_no_ACD_tree, data_extras_new1202)

log_no_ACD_cm <- confusionMatrix(log_no_ACD_pred, data_extras_new1202$Group, positive ="Yes")
knn_no_ACD_cm <- confusionMatrix(knn_no_ACD_pred, data_extras_new1202$Group, positive ="Yes")
rf_no_ACD_cm <- confusionMatrix(rf_no_ACD_pred, data_extras_new1202$Group, positive ="Yes")
svm_no_ACD_cm <- confusionMatrix(SVM_no_ACD_pred, data_extras_new1202$Group, positive ="Yes")
xgb_no_ACD_cm <- confusionMatrix(xgb_no_ACD_pred, data_extras_new1202$Group, positive ="Yes")
tree_no_ACD_cm <- confusionMatrix(tree_no_ACD_pred, data_extras_new1202$Group, positive ="Yes")

log_no_ACD_roc <- pROC::roc(data_extras_new1202$Group, log_probs_no_ACD$Yes)
knn_no_ACD_roc <- pROC::roc(data_extras_new1202$Group, knn_probs_no_ACD$Yes)
rf_no_ACD_roc <- pROC::roc(data_extras_new1202$Group, rf_probs_no_ACD$Yes)
svm_no_ACD_roc <- pROC::roc(data_extras_new1202$Group, SVM_probs_no_ACD$Yes)
xgb_no_ACD_roc <- pROC::roc(data_extras_new1202$Group, xgb_probs_no_ACD$Yes)
tree_no_ACD_roc <- pROC::roc(data_extras_new1202$Group, tree_probs_no_ACD$Yes)

plot(log_no_ACD_roc)
plot(knn_no_ACD_roc)
plot(rf_no_ACD_roc)
plot(svm_no_ACD_roc)
plot(xgb_no_ACD_roc)
plot(tree_no_ACD_roc)

roc_no_ACD_df <- bind_rows(
  data.frame(
    Model = "Logistic",
    TPR = log_no_ACD_roc$sensitivities,
    FPR = 1 - log_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "KNN",
    TPR = knn_no_ACD_roc$sensitivities,
    FPR = 1 - knn_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "Random Forest",
    TPR = rf_no_ACD_roc$sensitivities,
    FPR = 1 - rf_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "SVM",
    TPR = svm_no_ACD_roc$sensitivities,
    FPR = 1 - svm_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "XGBoost",
    TPR = xgb_no_ACD_roc$sensitivities,
    FPR = 1 - xgb_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "Decision Tree",
    TPR = tree_no_ACD_roc$sensitivities,
    FPR = 1 - tree_no_ACD_roc$specificities
  )
)

auc_no_ACD_values <- c(
  Logistic = pROC::auc(log_no_ACD_roc),
  KNN = pROC::auc(knn_no_ACD_roc),
  RandomForest = pROC::auc(rf_no_ACD_roc),
  SVM = pROC::auc(svm_no_ACD_roc),
  XGBoost = pROC::auc(xgb_no_ACD_roc),
  Tree = pROC::auc(tree_no_ACD_roc)
)

names(auc_no_ACD_values) <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree")

roc_no_ACD_df$Model <- factor(roc_no_ACD_df$Model,
                                 levels = names(auc_no_ACD_values),
                                 labels = paste0(names(auc_no_ACD_values), " (AUC=", round(auc_no_ACD_values, 3), ")")
)

plotmodel_no_ACD_data_extras_new1202 <- ggplot(roc_no_ACD_df, aes(x = FPR, y = TPR, color = Model)) +
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

plotmodel_no_ACD_data_extras
plotmodel_no_ACD_data_extras_new1202

predict(model_no_ACD_LT_xgb, data_xiaoce_used)
