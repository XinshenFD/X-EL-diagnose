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

model_logistic_no_LT_ACD <- glm(Group ~ pre_S+pre_C+LogMAR+AL+K1+K2,
                      data = data_predictors,
                      family = binomial(link = "logit"))

summary(model_logistic_no_LT_ACD)

table(rownames(data_predictors_af_logistic) == rownames(data_predictors))
data_predictors_af_logistic$Group
data_predictors_af_logistic_no_ACD_LT <- dplyr::select(data_predictors_af_logistic,
                                                       pre_S,
                                                       pre_C,LogMAR, AL, K1,Group)
data_predictors_af_logistic_no_ACD_LT <- cbind(data_predictors_af_logistic_no_ACD_LT,
                                               data_predictors$K2)
colnames(data_predictors_af_logistic_no_ACD_LT)[7] <- "K2"

data_predictors_af_logistic_no_ACD_LT_train <- data_predictors_af_logistic_no_ACD_LT[training, ]
colnames(data_predictors_af_logistic_no_ACD_LT)
summary(yunnan_add_na)
yunnan_add_na_trainging <-dplyr::select(yunnan_add_na,
                                        pre_S,
                                        pre_C,
                                        LogMAR,
                                        AL,
                                        K1,

                                        Group,
                                        K2)
data_predictors_af_logistic_no_ACD_LT_train <- rbind(data_predictors_af_logistic_no_ACD_LT_train,
                                                       yunnan_add_na_trainging)
tail(yunnan_add_na)
tail(data_predictors_af_logistic_no_ACD_LT_train)
length(data_predictors_af_logistic_no_ACD_LT_train$pre_S)

model_no_ACD_LT_log <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_LT_train,
                   method = "glm", family = "binomial",
                   trControl = control, metric = "ROC")
model_no_ACD_LT_log

model_no_ACD_LT_knn <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_LT_train,
                   method = "knn", trControl = control, metric = "ROC")
model_no_ACD_LT_knn

model_no_ACD_LT_rf <- train(Group ~ .,  data = data_predictors_af_logistic_no_ACD_LT_train,
                  method = "rf", trControl = control, metric = "ROC")
model_no_ACD_LT_rf

model_no_ACD_LT_svm <- train(Group ~ ., data =  data_predictors_af_logistic_no_ACD_LT_train,
                   method = "svmRadial", trControl = control, metric = "ROC")
model_no_ACD_LT_svm

model_no_ACD_LT_xgb <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_LT_train,
                   method = "xgbTree", trControl = control, metric = "ROC")
model_no_ACD_LT_xgb

model_no_ACD_LT_tree <- train(Group ~ ., data = data_predictors_af_logistic_no_ACD_LT_train,
                    method = "rpart", trControl = control, metric = "ROC")
model_no_ACD_LT_tree

log_probs_no_ACD_LT <- predict(model_no_ACD_LT_log, validation_set,type="prob")
knn_probs_no_ACD_LT <- predict(model_no_ACD_LT_knn, validation_set,type="prob")
rf_probs_no_ACD_LT <- predict(model_no_ACD_LT_rf, validation_set,type="prob")
SVM_probs_no_ACD_LT <- predict(model_no_ACD_LT_svm, validation_set,type="prob")
xgb_probs_no_ACD_LT <- predict(model_no_ACD_LT_xgb, validation_set,type="prob")
tree_probs_no_ACD_LT <- predict(model_no_ACD_LT_tree, validation_set,type="prob")

log_no_ACD_LT_pred <- predict(model_no_ACD_LT_log, validation_set)
knn_no_ACD_LT_pred <- predict(model_no_ACD_LT_knn, validation_set)
rf_no_ACD_LT_pred <- predict(model_no_ACD_LT_rf, validation_set)
SVM_no_ACD_LT_pred <- predict(model_no_ACD_LT_svm, validation_set)
xgb_no_ACD_LT_pred <- predict(model_no_ACD_LT_xgb, validation_set)
tree_no_ACD_LT_pred <- predict(model_no_ACD_LT_tree, validation_set)

log_no_ACD_LT_cm <- confusionMatrix(log_no_ACD_LT_pred, validation_set$Group, positive ="Yes")
knn_no_ACD_LT_cm <- confusionMatrix(knn_no_ACD_LT_pred, validation_set$Group, positive ="Yes")
rf_no_ACD_LT_cm <- confusionMatrix(rf_no_ACD_LT_pred, validation_set$Group, positive ="Yes")
svm_no_ACD_LT_cm <- confusionMatrix(SVM_no_ACD_LT_pred, validation_set$Group, positive ="Yes")
xgb_no_ACD_LT_cm <- confusionMatrix(xgb_no_ACD_LT_pred, validation_set$Group, positive ="Yes")
tree_no_ACD_LT_cm <- confusionMatrix(tree_no_ACD_LT_pred, validation_set$Group, positive ="Yes")

log_no_ACD_LT_roc <- pROC::roc(validation_set$Group, log_probs_no_ACD_LT$Yes)
knn_no_ACD_LT_roc <- pROC::roc(validation_set$Group, knn_probs_no_ACD_LT$Yes)
rf_no_ACD_LT_roc <- pROC::roc(validation_set$Group, rf_probs_no_ACD_LT$Yes)
svm_no_ACD_LT_roc <- pROC::roc(validation_set$Group, SVM_probs_no_ACD_LT$Yes)
xgb_no_ACD_LT_roc <- pROC::roc(validation_set$Group, xgb_probs_no_ACD_LT$Yes)
tree_no_ACD_LT_roc <- pROC::roc(validation_set$Group, tree_probs_no_ACD_LT$Yes)

plot(log_no_ACD_LT_roc)
plot(knn_no_ACD_LT_roc)
plot(rf_no_ACD_LT_roc)
plot(svm_no_ACD_LT_roc)
plot(xgb_no_ACD_LT_roc)
plot(tree_no_ACD_LT_roc)

roc_no_ACD_LT_df <- bind_rows(
  data.frame(
    Model = "Logistic",
    TPR = log_no_ACD_LT_roc$sensitivities,
    FPR = 1 - log_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "KNN",
    TPR = knn_no_ACD_LT_roc$sensitivities,
    FPR = 1 - knn_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "Random Forest",
    TPR = rf_no_ACD_LT_roc$sensitivities,
    FPR = 1 - rf_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "SVM",
    TPR = svm_no_ACD_LT_roc$sensitivities,
    FPR = 1 - svm_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "XGBoost",
    TPR = xgb_no_ACD_LT_roc$sensitivities,
    FPR = 1 - xgb_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "Decision Tree",
    TPR = tree_no_ACD_LT_roc$sensitivities,
    FPR = 1 - tree_no_ACD_LT_roc$specificities
  )
)

auc_no_ACD_LT_values <- c(
  Logistic = pROC::auc(log_no_ACD_LT_roc),
  KNN = pROC::auc(knn_no_ACD_LT_roc),
  RandomForest = pROC::auc(rf_no_ACD_LT_roc),
  SVM = pROC::auc(svm_no_ACD_LT_roc),
  XGBoost = pROC::auc(xgb_no_ACD_LT_roc),
  Tree = pROC::auc(tree_no_ACD_LT_roc)
)

names(auc_no_ACD_LT_values) <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree")

roc_no_ACD_LT_df$Model <- factor(roc_no_ACD_LT_df$Model,
                       levels = names(auc_no_ACD_LT_values),
                       labels = paste0(names(auc_no_ACD_LT_values), " (AUC=", round(auc_no_ACD_LT_values, 3), ")")
)

plotmodel_no_ACD_LT_validation_set <- ggplot(roc_no_ACD_LT_df, aes(x = FPR, y = TPR, color = Model)) +
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

colnames(data_predictors_af_logistic_no_ACD_LT_train)
validation_set_add <- dplyr::select(validation_set,
                                    pre_S,
                                    pre_C,
                                    LogMAR,
                                    AL,
                                    K1,
                                    Group,
                                    K2)

data_predictors_af_logistic_no_ACD_LT_all <- rbind(data_predictors_af_logistic_no_ACD_LT_train,
                                                   validation_set_add)

log_probs_no_ACD_LT <- predict(model_no_ACD_LT_log, data_extras_new1202,type="prob")
knn_probs_no_ACD_LT <- predict(model_no_ACD_LT_knn, data_extras_new1202,type="prob")
rf_probs_no_ACD_LT <- predict(model_no_ACD_LT_rf, data_extras_new1202,type="prob")
SVM_probs_no_ACD_LT <- predict(model_no_ACD_LT_svm, data_extras_new1202,type="prob")
xgb_probs_no_ACD_LT <- predict(model_no_ACD_LT_xgb, data_extras_new1202,type="prob")
tree_probs_no_ACD_LT <- predict(model_no_ACD_LT_tree, data_extras_new1202,type="prob")

log_no_ACD_LT_pred <- predict(model_no_ACD_LT_log, data_extras_new1202)
knn_no_ACD_LT_pred <- predict(model_no_ACD_LT_knn, data_extras_new1202)
rf_no_ACD_LT_pred <- predict(model_no_ACD_LT_rf, data_extras_new1202)
SVM_no_ACD_LT_pred <- predict(model_no_ACD_LT_svm, data_extras_new1202)
xgb_no_ACD_LT_pred <- predict(model_no_ACD_LT_xgb, data_extras_new1202)
tree_no_ACD_LT_pred <- predict(model_no_ACD_LT_tree, data_extras_new1202)

log_no_ACD_LT_cm <- confusionMatrix(log_no_ACD_LT_pred, data_extras_new1202$Group, positive ="Yes")
knn_no_ACD_LT_cm <- confusionMatrix(knn_no_ACD_LT_pred, data_extras_new1202$Group, positive ="Yes")
rf_no_ACD_LT_cm <- confusionMatrix(rf_no_ACD_LT_pred, data_extras_new1202$Group, positive ="Yes")
svm_no_ACD_LT_cm <- confusionMatrix(SVM_no_ACD_LT_pred, data_extras_new1202$Group, positive ="Yes")
xgb_no_ACD_LT_cm <- confusionMatrix(xgb_no_ACD_LT_pred, data_extras_new1202$Group, positive ="Yes")
tree_no_ACD_LT_cm <- confusionMatrix(tree_no_ACD_LT_pred, data_extras_new1202$Group, positive ="Yes")

log_no_ACD_LT_roc <- pROC::roc(data_extras_new1202$Group, log_probs_no_ACD_LT$Yes)
knn_no_ACD_LT_roc <- pROC::roc(data_extras_new1202$Group, knn_probs_no_ACD_LT$Yes)
rf_no_ACD_LT_roc <- pROC::roc(data_extras_new1202$Group, rf_probs_no_ACD_LT$Yes)
svm_no_ACD_LT_roc <- pROC::roc(data_extras_new1202$Group, SVM_probs_no_ACD_LT$Yes)
xgb_no_ACD_LT_roc <- pROC::roc(data_extras_new1202$Group, xgb_probs_no_ACD_LT$Yes)
tree_no_ACD_LT_roc <- pROC::roc(data_extras_new1202$Group, tree_probs_no_ACD_LT$Yes)

plot(log_no_ACD_LT_roc)
plot(knn_no_ACD_LT_roc)
plot(rf_no_ACD_LT_roc)
plot(svm_no_ACD_LT_roc)
plot(xgb_no_ACD_LT_roc)
plot(tree_no_ACD_LT_roc)

roc_no_ACD_LT_df <- bind_rows(
  data.frame(
    Model = "Logistic",
    TPR = log_no_ACD_LT_roc$sensitivities,
    FPR = 1 - log_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "KNN",
    TPR = knn_no_ACD_LT_roc$sensitivities,
    FPR = 1 - knn_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "Random Forest",
    TPR = rf_no_ACD_LT_roc$sensitivities,
    FPR = 1 - rf_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "SVM",
    TPR = svm_no_ACD_LT_roc$sensitivities,
    FPR = 1 - svm_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "XGBoost",
    TPR = xgb_no_ACD_LT_roc$sensitivities,
    FPR = 1 - xgb_no_ACD_LT_roc$specificities
  ),
  data.frame(
    Model = "Decision Tree",
    TPR = tree_no_ACD_LT_roc$sensitivities,
    FPR = 1 - tree_no_ACD_LT_roc$specificities
  )
)

auc_no_ACD_LT_values <- c(
  Logistic = pROC::auc(log_no_ACD_LT_roc),
  KNN = pROC::auc(knn_no_ACD_LT_roc),
  RandomForest = pROC::auc(rf_no_ACD_LT_roc),
  SVM = pROC::auc(svm_no_ACD_LT_roc),
  XGBoost = pROC::auc(xgb_no_ACD_LT_roc),
  Tree = pROC::auc(tree_no_ACD_LT_roc)
)

names(auc_no_ACD_LT_values) <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree")

roc_no_ACD_LT_df$Model <- factor(roc_no_ACD_LT_df$Model,
                                 levels = names(auc_no_ACD_LT_values),
                                 labels = paste0(names(auc_no_ACD_LT_values), " (AUC=", round(auc_no_ACD_LT_values, 3), ")")
)

plotmodel_no_ACD_LT_data_extras_new1202 <- ggplot(roc_no_ACD_LT_df, aes(x = FPR, y = TPR, color = Model)) +
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

plotmodel_no_ACD_LT_data_extras
plotmodel_no_ACD_LT_data_extras_new1202
data_xiaoce_used$KSE <- 42.21633
log10(1/0.6)
data_xiaoce_used[4, ] <- c(2.5, -2.5, 0.2218487, 22.0, 40.26, 3.56, 3.78, 40.69)
data_xiaoce_used$K2 <- 41.13
predict(model_no_ACD_LT_knn, data_xiaoce_used)
predict(model_no_ACD_LT_log, data_xiaoce_used)
predict(model_no_ACD_LT_rf, data_xiaoce_used)
predict(model_no_ACD_LT_svm, data_xiaoce_used)
predict(model_no_ACD_LT_tree, data_xiaoce_used)
predict(model_no_ACD_LT_xgb, data_xiaoce_used)
predict(model_no_ACD_LT_xgb, xgb_no_ACD_LT_pred_best_misclassified[32, ],type="prob")
