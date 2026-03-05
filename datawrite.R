# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

train_data
extra_data
inner_data
data_predictors
data_predictors_af_lasso
data_extras
validation_set
data_predictors_af_logistic
training_set

openxlsx::write.xlsx(train_data,"data/train_data.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(training_set,"data/training_set.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(extra_data,"data/extra_data.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(inner_data,"data/inner_data.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(data_predictors,"data/data_predictors.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(data_predictors_af_lasso,"data/data_predictors_af_lasso.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(data_extras,"data/data_extras.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(validation_set,"data/validation_set.xlsx",  rowNames = TRUE)

openxlsx::write.xlsx(data_predictors_af_logistic,
                     "data/data_predictors_af_logistic.xlsx",  rowNames = TRUE)

table(data_extras$Group)
