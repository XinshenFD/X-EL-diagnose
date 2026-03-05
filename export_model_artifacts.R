# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

summary(model_no_ACD_LT_xgb)
model_no_ACD_LT_xgb$finalModel$feature_names

final_xgb_model <- model_no_ACD_LT_xgb$finalModel

saveRDS(model_no_ACD_LT_xgb, "model/my_xgb_model.rds")
library(xgboost)

xgb.save(final_xgb_model, "model/my_xgb_model.json")

xgb.save(final_xgb_model, "model/my_xgb_model.model")

xgb.save(model_no_ACD_LT_xgb$finalModel, "my_model_native.model")

xgb.save(model_no_ACD_LT_xgb$finalModel, "my_model_universal.json")
