# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

models <- list(Logistic = model_log, KNN = model_knn, RF = model_rf,
               SVM = model_svm, XGB = model_xgb,
               Tree = model_tree)
validation_set
colnames(validation_set)
validation_set

data_predictors_af_logistic_1$Group
results <- lapply(models, function(mod){
  pred <- predict(mod, validation_set, type = "prob")[, "Yes"]
  roc_obj <- roc(response = validation_set$Group, predictor = pred)
  auc_val <- auc(roc_obj)
  list(pred = pred, roc = roc_obj, auc = auc_val)
})

colnames(data_predictors_af_logistic_1)
data_xiaoce <- validation_set[1, ]
log10(1/0.9)
data_xiaoce[, c(3, 4, 5, 6, 7, 11, 12)] <-c(0, -0.75,  0.04575749, 21.76, 41.62, 3.5, NA)
predict(model_tree, data_xiaoce, type = "prob")[, "Yes"]

colnames(data_predictors_af_logistic)
data_predictors_af_logistic_xiaoce <- data_predictors_af_logistic[, c(1:6, 8)]
library(pROC)
library(caret)
twoClassSummaryCustom <- function (data, lev = NULL, model = NULL) {
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The twoClassSummary() function isn't appropriate."))
  }
  caret:::requireNamespaceQuietStop("pROC")
  if (!all(levels(data[, "pred"]) == lev)) {
    stop("levels of observed and predicted data do not match")
  }
  rocObject <- try(pROC::roc(data$obs, data[, lev[1]], direction = ">",
                             quiet = TRUE), silent = TRUE)
  rocAUC <- if (inherits(rocObject, "try-error"))
    NA
  else rocObject$auc
  out <- c(rocAUC, sensitivity(data[, "pred"], data[, "obs"],
                               lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
  out2 <- postResample(data[, "pred"], data[, "obs"])
  out <- c(out, out2[1])
  names(out) <- c("AUC", "Sens", "Spec", "Accuracy")
  out
}
control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
predictors
colnames(predictors)
colnames(data_predictors_af_logistic_xiaoce)
predictors_xiaoce <- dplyr::select(predictors,
                                   pre_S,pre_C, LogMAR, AL, K1, ACD,LT,  Group)
data_predictors_af_logistic_xiaoce$Group
predictors_xiaoce$Group <- factor(predictors_xiaoce$Group,
                                  levels = c(0, 1))
predictors_xiaoce$Group <- as.numeric(predictors_xiaoce$Group)
predictors_xiaoce$Group <- ifelse(predictors_xiaoce$Group == 2,
                                  "Yes",
                                  "No")
predictors_xiaoce$Group <- factor(predictors_xiaoce$Group,
                                  levels = c("No", "Yes"))

model_xgb_xiaoce <- train(Group ~ ., data = predictors_xiaoce,
                   method = "xgbTree", trControl = control,na.action = na.pass, metric = "ROC")

model_xgb
summary(model_xgb_xiaoce)

names(model_xgb_xiaoce$trainingData)
names(data_xiaoce)
data_xiaoce <-  subset(data_xiaoce, select = -Group)

vars_used <- setdiff(names(model_xgb_xiaoce$trainingData), ".outcome")
data_xiaoce_used <- data_xiaoce[, vars_used, drop = FALSE]
data_xiaoce_used$LT <- NA
data_xiaoce_used[2, ] <- data_xiaoce_used[1,]
data_xiaoce_used[2, 7] <- 3.682713
data_xiaoce_used <- data_xiaoce_used[, 1:6]

predict(model_xgb_xiaoce, data_xiaoce_used, type = "prob")[, "Yes"]
predict(model_xgb_xiaoce$finalModel, newdata = as.matrix(data_xiaoce_used), type = "prob")
model_xgb_xiaoce$finalModel$params

str(model_xgb_xiaoce)
model_xgb_xiaoce$levels
colnames(validation_set)
validation_set_xiaoce <- dplyr::select(validation_set,
                                       pre_S,pre_C, LogMAR, AL, K1, ACD, LT,Group)
pred_prob <- predict(model_xgb_xiaoce$finalModel, newdata = as.matrix(validation_set_xiaoce),
                     type = "prob")
roc_obj <- pROC::roc(response = validation_set_xiaoce$Group, predictor = pred_prob)
mean(data_predictors_af_logistic$LT)

pROC::auc(roc_obj)

best_cutoff <- pROC::coords(roc_obj, "best", ret = "threshold", best.method = "youden")
best_cutoff
data_xiaoce
