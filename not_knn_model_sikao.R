# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

data_xiaoce_used
extra_validations_ <- dplyr::select(extra_vv,
                             pre_S,
                             pre_C,
                             LogMAR,
                             AL,
                             K1,
                             ACD,
                             LT)
preds_prob<- predict(model_xgb$finalModel, newdata =as.matrix(extra_validations_))
extra_validations_ <- dplyr::select(extra_vv,
                                    pre_S,
                                    pre_C,
                                    LogMAR,
                                    AL,
                                    K1,
                                    ACD,
                                    LT,Group)

extra_validations_$Group <- ifelse(extra_validations_$Group == 0,
                                   "No",
                                   "Yes")

y_true <- factor(extra_validations_$Group, levels = c("No", "Yes"))

roc_obj <- roc(response = y_true, predictor = preds_prob)
plot(roc_obj )

print(pROC::auc(roc_obj))
roc_obj_noknn <- pROC::auc(roc_obj)
best_coords <- pROC::coords(
  roc_obj,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

best_coords

predict(model_xgb$finalModel, newdata =as.matrix(extra_validations_))
predict(model_xgb$finalModel, newdata = as.matrix(data_xiaoce_used))
