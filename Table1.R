# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

train_data
colnames(train_data)
table(train_data$EL_severity)
table(train_data$EL)
data_predictors
colnames(data_extra_toinner_data)
colnames(CC_TC_train_data_select)
RE_train_data_select
inner_train_data

colnames(data_extra_toinner)
colnames(RE_train_data)
colnames(CC_TC_train_data)
colnames(inner_data)
extra_data$name.x
data_extra_toinner$name %in% extra_data$name.x

length(inner_train_data$name)
table(CC_TC_train_data_select$surgery_follow)

data_extra_toinner <- subset(extra_data,
                             extra_data$name %in% data_extra_toinner
)

table(data_extras_new1202$peidui %in% paste0(data_extra_toinner$AL, data_extra_toinner$K1, data_extra_toinner$KSE))

data_extras_new1202_qvdiaochongfu <- data_extras_new1202[!(data_extras_new1202$peidui %in% paste0(data_extra_toinner$AL, data_extra_toinner$K1, data_extra_toinner$KSE)), ]
length(data_extras_new1202_qvdiaochongfu$Age)
870+123
data_extra_toinner_data
CC_TC_train_data_select
RE_train_data_select
inner_train_data
colnames(data_extra_toinner_data)
table(data_extra_toinner_data$surgery_follow)
table(inner_train_data$surgery_follow)
table(CC_TC_train_data_select$surgery_follow)
colnames(CC_TC_train_data_select)
table(RE_train_data_select$surgery_follow)
colnames(RE_train_data_select)
colnames(inner_train_data)
inner_train_data
colnames(CC_TC_train_data)
colnames(train_data)

which(duplicated(train_data$name_cate))
which(train_data$name_cate == "REDACTED")
length(train_data$laterality)

train_data[c(1222, 1223, 1507, 1508, 1509,
             1511, 1512, 1513, 1764, 1765, 1766,
             1876),]
table(train_data$EL_severity)
train_data_for_table1 <- dplyr::select(train_data,
                                       laterality,
                                       gender,
                                       Age,
                                       pre_S,
                                       pre_C,
                                       CDVA,
                                       AL,
                                       K1,
                                       K2,
                                       KSE, K_c, ACD, LT, WTW, CCT, surgery_follow.x, LogMAR,
                                       EL_severity)
train_data_for_table1$Group <- ifelse(train_data_for_table1$surgery_follow.x %in% c("F1", "F2", "F3", "S"),
                                      "EL",
                                      ifelse(train_data_for_table1$surgery_follow.x %in% c("CC", "TC"),
                                             "Cataract",
                                             "RE"
                                      ))
table(train_data_for_table1$surgery_follow.x)
table(train_data_for_table1$EL_severity)
train_data_for_table1$EL_severity <- ifelse(train_data_for_table1$EL_severity == "REDACTED",
                                            "Spherophakia",
                                            ifelse(
                                              train_data_for_table1$EL_severity == "REDACTED",
                                              "Moderate",
                                              ifelse(
                                                train_data_for_table1$EL_severity == "REDACTED",
                                                "Mild",
                                                "Severe"
                                              )
                                            ))

table(train_data_for_table1$Group)
table(train_data_for_table1$EL_severity)
table(is.na(train_data_for_table1$EL_severity))
212+380+135+74
table(train_data_for_table1$surgery_follow.x)
summary(train_data_for_table1)

table(data_predictors_af_logistic_no_ACD_LT_train$Group)
table(validation_set$Group)
table(train_data_for_table1$surgery_follow.x)

library(gtsummary)
library(gt)
table(train_data_for_table1$EL_severity)

train_data_for_table1 <- train_data_for_table1 %>%
  mutate(
    Group = factor(Group, levels = c("EL","Cataract", "RE")),
    EL_severity = factor(EL_severity, levels = c("Mild", "Moderate", "Severe", "Spherophakia"))
  )
train_data_for_table1$gender <- ifelse(train_data_for_table1$gender == "0",
                                       "Female",
                                       "Male")

vars_to_summary <- c("laterality", "gender", "Age", "pre_S", "pre_C", "LogMAR",
                      "AL", "K1", "K2", "KSE", "K_c", "ACD",
                     "LT", "WTW", "CCT"  )

table_control <- train_data_for_table1 %>%
  filter(Group %in% c("Cataract", "RE")) %>%
  dplyr::select(all_of(vars_to_summary), Group) %>%
  tbl_summary(
    by = Group,
    missing = "no",
    statistic = list(
                     all_categorical() ~ "{n} ({p}%)")
  )

table_el <- train_data_for_table1 %>%
  filter(Group == "EL") %>%
  dplyr::select(all_of(vars_to_summary), EL_severity) %>%
  tbl_summary(
    by = EL_severity,
    missing = "no",
    statistic = list(
                     all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_p()

final_table <- tbl_merge(
  tbls = list( table_el,table_control),
  tab_spanner = c("**Ectopia Lentis (EL) Subgroups**","**Control Groups**")
)
library(gt)

final_gt <- final_table %>%
  as_gt() %>%

  tab_header(
    title = "Table 1. Baseline Characteristics by Group and Severity"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gt::gtsave(final_gt, filename = "tables/Table1_Baseline_2.docx")

final_gt

training

train_data_for_table1

table(train_data_for_table1$EL_severity)
kruskal.test(Age ~ Group, train_data_for_table1)
kruskal.test(AL ~ Group, train_data_for_table1)
