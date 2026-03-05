# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

stastic_result <- read.xlsx("data/result_data_1020.xlsx")
stastic_result$ELseverity
stastic_result$ELseverity <- as.numeric(stastic_result$ELseverity)

stastic_result$ELseverity
stastic_result$EL_class <- ifelse(stastic_result$ELseverity <= 90,
                                  "Mild",
                                  ifelse(
                                    stastic_result$ELseverity <= 180,
                                    "Moderate",
                                    "Severe"
                                  ))
stastic_result$EL_class
colnames(stastic_result)
n_cells <- stastic_result %>%
  group_by(EL_class, label_predict, Group) %>%
  dplyr::count() %>%
  spread(EL_class, n)

write.xlsx(n_cells,"data/n_cells.xlsx")
