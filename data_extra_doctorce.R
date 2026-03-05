# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

data_extras
set.seed(123)

df_shuffled <- data_extras[sample(nrow(data_extras)), ]

df_shuffled <- data_extras[sample(1:nrow(data_extras)), ]

write.xlsx(
  data_extras, "data/data_extras_yishengduizhao.xlsx", rowNames = T
)

write.xlsx(
  df_shuffled , "data/df_shuffled.xlsx", rowNames = T
)
