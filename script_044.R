# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

data_extras_new1202
colnames(data_extras_new1202)
table(extra_data$Group)
200   +19   +86   +66
table(extra_data$EL_severity)
table(extra_contral$Group)
table(data_extras_new1202$Group)
table(extra_validations_newcc_part$Group)
length(add_EL$name)
table(add_EL$EL_severity)
1095  +199  +744  +431

add_EL$name_cate

table(extra_data$name_cate %in% add_EL$name_cate)
table(subset(train_data,
             train_data$Group == 1)$name_cate %in% add_EL$name_cate)

colnames(extra_data)
colnames(extra_contral)
naru <- colnames(extra_contral)

colnames(extra_validations_newcc_part)

212+380+315+74+256+621
1309+561

extra_data_table <- extra_data
extra_contral_table <- extra_contral
extra_validations_newcc_part_table <- extra_validations_newcc_part

extra_data_table$EL_severity
extra_contral_table$EL_severity <- NA
extra_contral_table$Group <- "RE"
extra_validations_newcc_part_table$EL_severity <- NA
naru <- c(naru, "EL_severity")
cc_new_add_1202$cate
cc_new_add_1202$KE <- 2/(1/cc_new_add_1202$K1 +1/cc_new_add_1202$K2)
table(cc_new_add_1202$cate)

table(extra_validations_newcc_part_table$Group)
extra_validations_newcc_part_table$AL == cc_new_add_1202$AL
extra_validations_newcc_part_table$Group <- cc_new_add_1202$cate
extra_validations_newcc_part_table$EL_severity <- NA
data_extra_validation_table <- rbind(extra_data_table[, naru],
                                     extra_contral_table[, naru],
                                     extra_validations_newcc_part_table[, naru])
data_extra_validation_table$EL_severity
table(data_extra_validation_table$EL_severity)
data_extra_validation_table$EL_severity <- ifelse(data_extra_validation_table$EL_severity == "REDACTED",
                                            "Spherophakia",
                                            ifelse(
                                              data_extra_validation_table$EL_severity == "REDACTED",
                                              "Moderate",
                                              ifelse(
                                                data_extra_validation_table$EL_severity == "REDACTED",
                                                "Mild",
                                                ifelse(
                                                  data_extra_validation_table$EL_severity == "REDACTED",
                                                  "Severe",
                                                  NA
                                                )
                                              )
                                            ))

table(data_extra_validation_table$Group)
data_extra_validation_table$Group <- ifelse(data_extra_validation_table$Group == "Yes",
                                            "EL",
                                            ifelse(data_extra_validation_table$Group == "No",
                                                   "RE",
                                                   data_extra_validation_table$Group))
table(data_extra_validation_table$Group)
table(data_extra_validation_table$EL_severity)

data_filled <- data_extra_validation_table

missing_indices <- which(data_filled$Group == "EL" & is.na(data_filled$EL_severity))

existing_counts <- table(data_filled$EL_severity[data_filled$Group == "EL"])

probs <- prop.table(existing_counts)

print("REDACTED")
print(probs)

filled_values <- sample(
  x = names(probs),
  size = length(missing_indices),
  replace = TRUE,
  prob = probs
)

data_filled$EL_severity[missing_indices] <- filled_values

print("REDACTED")
table(data_filled$EL_severity[data_filled$Group == "EL"])

sum(data_filled$Group == "EL" & is.na(data_filled$EL_severity))

table(data_filled$gender)
data_filled$gender <- ifelse(data_filled$gender == 0,
                             "Male",
                             ifelse(
                               data_filled$gender == 1,
                               "Female",
                               data_filled$gender
                             ))
colnames(data_filled)
table(data_filled$Group)
data_filled$Group <- ifelse(data_filled$Group == "CC",
                            "Cataract",
                            data_filled$Group)

data_filled_table1 <- data_filled %>%
  mutate(
    Group = factor(Group, levels = c("EL","Cataract", "RE")),
    EL_severity = factor(EL_severity, levels = c("Mild", "Moderate", "Severe", "Spherophakia"))
  )
colnames(data_filled_table1 )

vars_to_summary <- c("gender", "Age", "pre_S", "pre_C", "LogMAR",
                     "AL", "K1", "K2", "KSE", "K_c", "ACD",
                     "LT", "WTW")

data_filled$LogMAR <- as.numeric(data_filled$LogMAR)
data_filled$WTW <- as.numeric(data_filled$WTW)

table_control_extra <- data_filled %>%
  filter(Group %in% c("Cataract", "RE")) %>%
  dplyr::select(all_of(vars_to_summary), Group) %>%
  tbl_summary(
    by = Group,
    missing = "no",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)")
  )

table_el_extra <-data_filled %>%
  filter(Group == "EL") %>%
  dplyr::select(all_of(vars_to_summary), EL_severity) %>%
  tbl_summary(
    by = EL_severity,
    missing = "no",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_p()

final_table_extra <- tbl_merge(
  tbls = list( table_el_extra,table_control_extra),
  tab_spanner = c("**Ectopia Lentis (EL) Subgroups**","**Control Groups**")
)
final_gt_extra <- final_table_extra %>%
  as_gt() %>%

  tab_header(
    title = "Table 1. Baseline Characteristics by Group and Severity"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gt::gtsave(final_gt_extra, filename = "tables/Table1_extra_2.docx")
