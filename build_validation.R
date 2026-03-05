# Sanitized script for public submission.
# Internal validation and threshold selection.

models <- list(
  Logistic = model_log,
  KNN = model_knn,
  RF = model_rf,
  SVM = model_svm,
  XGB = model_xgb,
  Tree = model_tree
)

results <- lapply(models, function(mod) {
  pred <- predict(mod, validation_set, type = "prob")[, "Yes"]
  roc_obj <- pROC::roc(response = validation_set$Group, predictor = pred)
  auc_val <- pROC::auc(roc_obj)
  list(pred = pred, roc = roc_obj, auc = auc_val)
})

validation_summary <- data.frame(
  Model = names(results),
  AUC = round(sapply(results, function(x) as.numeric(x$auc)), 3)
)
validation_summary

xgb_roc_validation <- pROC::roc(
  response = validation_set$Group,
  predictor = results$XGB$pred
)

best_cutoff <- pROC::coords(
  xgb_roc_validation,
  "best",
  ret = "threshold",
  best.method = "youden"
)
best_cutoff
