# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

model_no_ACD_LT_xgb

model_no_ACD_LT_xgb$ptype

data_extra_validation_table$LogMAR <- as.numeric(data_extra_validation_table$LogMAR)
table(data_extra_validation_table$LogMAR)
summary(data_extra_validation_table)
data_extra_validation_table2 <- data_filled
data_extra_validation_table2 <- subset(data_extra_validation_table2,
                                       is.na(data_extra_validation_table2$K1) == F)
summary(data_extra_validation_table2)
data_extra_validation_table2
data_extra_validation_table2$LogMAR <- as.numeric(data_extra_validation_table2$LogMAR)
data_extra_validation_table2$pre_C <- ifelse(is.na(data_extra_validation_table2$pre_C) == T,
                                             0,
                                             data_extra_validation_table2$pre_C)

data_extra_validation_table2$predicted_label <- predict(model_no_ACD_LT_xgb, data_extra_validation_table2,type="prob")

data_extra_validation_table2$predicted_label <- ifelse(data_extra_validation_table2$predicted_label$Yes > xgb_no_ACD_LT_best$threshold, "Yes", "No")

table(data_extra_validation_table2$Group)
table(data_extra_validation_table2$EL_severity)
table(data_extra_validation_table2$predicted_label)

plot_data <- data_extra_validation_table2 %>%
  mutate(

    Detailed_Group = ifelse(Group == "EL", as.character(EL_severity), as.character(Group)),

    True_Label = ifelse(Group == "EL", "Yes", "No")
  )

summary_table <- plot_data %>%
  group_by(Detailed_Group) %>%
  summarise(
    Total = n(),

    Correct = sum(predicted_label == True_Label),

    Incorrect = sum(predicted_label != True_Label)
  ) %>%

  mutate(Accuracy = sprintf("%.1f%%", (Correct / Total) * 100))

row_order <- c("Mild", "Moderate", "Severe", "Spherophakia", "Cataract", "RE")
summary_table <- summary_table %>%
  arrange(factor(Detailed_Group, levels = row_order))

ft <- flextable(summary_table) %>%
  set_header_labels(
    Detailed_Group = "Group / Severity",
    Total = "Total Cases",
    Correct = "Correctly Predicted",
    Incorrect = "Incorrectly Predicted",
    Accuracy = "Accuracy (%)"
  ) %>%
  autofit() %>%
  theme_vanilla() %>%
  add_header_lines(values = "Model Performance by Subgroup") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all")
library(officer)

doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "tables/Model_Performance_Table.docx")

library(dplyr)
library(flextable)
library(officer)
library(pROC)

pred_prob <- predict(model_no_ACD_LT_xgb, data_extra_validation_table2, type="prob")

data_extra_validation_table2$prob_Yes <- pred_prob$Yes
data_extra_validation_table2$Predicted_Label <- ifelse(data_extra_validation_table2$prob_Yes > xgb_no_ACD_LT_best$threshold, "Yes", "No")

plot_data <- data_extra_validation_table2 %>%
  mutate(

    Detailed_Group = ifelse(Group == "EL", as.character(EL_severity), as.character(Group)),

    True_Label = ifelse(Group == "EL", "Yes", "No")
  )

print(paste("Total rows:", nrow(plot_data)))
print(paste("Rows with prob_Yes:", sum(!is.na(plot_data$prob_Yes))))

control_data <- plot_data %>% filter(True_Label == "No")

if(nrow(control_data) == 0) stop("REDACTED")

subgroups <- c("Mild", "Moderate", "Severe", "Spherophakia", "Cataract", "RE")
results_list <- list()

for (sub in subgroups) {

  current_sub_data <- plot_data %>% filter(Detailed_Group == sub)

  if(nrow(current_sub_data) == 0) {
    warning(paste("REDACTED", sub, "REDACTED"))
    next
  }

  n_total <- nrow(current_sub_data)

  is_positive_group <- any(current_sub_data$True_Label == "Yes")

  if (is_positive_group) {

    n_correct <- sum(current_sub_data$Predicted_Label == "Yes")
    sensitivity <- n_correct / n_total

    auc_data <- bind_rows(current_sub_data, control_data)

    if(length(unique(auc_data$True_Label)) == 2 && !all(is.na(auc_data$prob_Yes))) {
      roc_obj <- pROC::roc(response = auc_data$True_Label, predictor = auc_data$prob_Yes, levels = c("No", "Yes"), quiet = TRUE)
      auc_val <- as.numeric(pROC::auc(roc_obj))
      ci_val <- ci.auc(roc_obj)
      auc_str <- sprintf("%.3f (%.3f-%.3f)", auc_val, ci_val[1], ci_val[3])
    } else {
      auc_str <- "NA"
    }

    res <- data.frame(
      Subgroup = sub,
      Total = n_total,
      Correct = n_correct,
      Accuracy = sprintf("%.1f%%", (n_correct/n_total)*100),
      Sensitivity = sprintf("%.1f%%", sensitivity * 100),
      Specificity = "-",
      AUC_vs_Control = auc_str
    )

  } else {

    n_correct <- sum(current_sub_data$Predicted_Label == "No")
    specificity <- n_correct / n_total

    res <- data.frame(
      Subgroup = sub,
      Total = n_total,
      Correct = n_correct,
      Accuracy = sprintf("%.1f%%", (n_correct/n_total)*100),
      Sensitivity = "-",
      Specificity = sprintf("%.1f%%", specificity * 100),
      AUC_vs_Control = "-"
    )
  }

  results_list[[sub]] <- res
}

final_table <- do.call(rbind, results_list)

final_table$Subgroup <- factor(final_table$Subgroup, levels = subgroups)
final_table <- final_table %>% arrange(Subgroup)

print(final_table)

ft <- flextable(final_table) %>%
  set_header_labels(
    Subgroup = "Group / Severity",
    Total = "Total Cases",
    Correct = "Correctly Predicted",
    Accuracy = "Accuracy",
    Sensitivity = "Sensitivity",
    Specificity = "Specificity",
    AUC_vs_Control = "AUC (vs Control)"
  ) %>%
  autofit() %>%
  theme_vanilla() %>%
  add_header_lines(values = "Diagnostic Performance by Subgroup") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  add_footer_lines("Note: AUC for EL subgroups is calculated as Subgroup vs. All Controls (Cataract + RE).")

doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "tables/Model_Performance_Detailed2.docx")

library(stringr)

plot_df <- final_table %>%
  mutate(

    Acc_Value = as.numeric(gsub("%", "", Accuracy)),

    AUC_Num = as.numeric(str_extract(AUC_vs_Control, "REDACTED")),
    CI_Low  = as.numeric(str_extract(AUC_vs_Control, "REDACTED")),
    CI_High = as.numeric(str_extract(AUC_vs_Control, "REDACTED")),

    Group_Type = ifelse(Subgroup %in% c("Cataract", "RE"), "Control Group", "EL Subgroup")
  )

plot_df$Subgroup <- factor(plot_df$Subgroup, levels = rev(subgroups))

forest_data <- plot_df %>% filter(!is.na(AUC_Num))

p1 <- ggplot(forest_data, aes(x = AUC_Num, y = Subgroup)) +

  geom_errorbarh(aes(xmin = CI_Low, xmax = CI_High), height = 0.2, color = "#2E86C1", size = 0.8) +

  geom_point(size = 2, color = "#E74C3C") +

  geom_vline(xintercept = 0.9, linetype = "dashed", color = "gray50") +

  geom_text(aes(label = sprintf("%.3f", AUC_Num)), vjust = -1.5, size = 5, color = "black") +

  scale_x_continuous(limits = c(0.8, 1.05), breaks = seq(0.8, 1, 0.1)) +

  theme_minimal() +
  labs(
    title = "AUC Performance by EL Severity",
    subtitle = "Model performance identifying specific EL subgroups vs. All Controls",
    x = "AUC (with 95% CI)",
    y = ""
  ) +
  theme_tufte()+
  theme(
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )

print(p1)
ggsave("tables/Forest_Plot_AUC.tiff", p1, width = 10, height = 5)
