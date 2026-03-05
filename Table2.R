# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

212+380+135+74
256+621

train_copy

validation_copy
train_copy

colnames(validation_copy) == colnames(train_copy)

colnames(train_data)

table(validation_copy$name == train_data[-training,1])

table(train_copy$name == train_data[training,1])

validation_copy$Set <- "Validation set"
train_copy$Set <- "Train set"

copy_for_table2 <- rbind(validation_copy, train_copy)

train_data_for_table2 <- dplyr::select(copy_for_table2,
                                       laterality,
                                       gender,
                                       Age,
                                       pre_S,
                                       pre_C,
                                       CDVA,
                                       AL,
                                       K1,
                                       K2,
                                       KSE, K_c, ACD, LT, WTW, CCT, surgery_follow, LogMAR, Set)

train_data_for_table2$Group <- ifelse(train_data_for_table2$surgery_follow %in% c("F1", "F2", "F3", "S"),
                                      "EL",
                                      ifelse(train_data_for_table2$surgery_follow %in% c("CC", "TC"),
                                             "Cataract",
                                             "RE"
                                      ))

train_data_for_table2 <- train_data_for_table2 %>%
  mutate(
    Group = factor(Set, levels = c("Train set", "Validation set")),

  )
train_data_for_table2$gender <- ifelse(train_data_for_table2$gender == "0",
                                       "Female",
                                       "Male")

vars_to_summary <- c("laterality", "gender", "Age", "pre_S", "pre_C", "LogMAR",
                     "AL", "K1", "K2", "KSE", "K_c", "ACD",
                     "LT", "WTW", "CCT"  )

table_t_v_1 <- train_data_for_table2 %>%
  dplyr::select(all_of(vars_to_summary), Set) %>%
  tbl_summary(
    by = Set,
    missing = "no",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_p()

table_t_v_2 <- train_data_for_table2 %>%
  dplyr::select(all_of(vars_to_summary), Set) %>%
  tbl_summary(
    by = Set,
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_p()

table_t_v_2 <- table_t_v_2 %>%
  as_gt() %>%

  tab_header(
    title = "Table 2. Baseline Characteristics by Set"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

table_t_v_1 <- table_t_v_1 %>%
  as_gt() %>%

  tab_header(
    title = "Table 2. Baseline Characteristics by Set"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gt::gtsave(table_t_v_2, filename = "tables/Table2_t_v_2.docx")
gt::gtsave(table_t_v_1, filename = "tables/Table2_t_v.docx")

final_gt

training
