# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

library(tidyr)
First <- read.xlsx("human_ai_result/sample1_result.xlsx")
Second <- read.xlsx("human_ai_result/sample2.xlsx")

First_time <- read.xlsx("human_ai_result/sample1_time.xlsx")

Second_time <- read.xlsx("human_ai_result/sample2_time.xlsx")

colnames(First)
table(First$Group)

First$EL_severity <- ifelse(First$EL_severity == "REDACTED",
                                                  "Spherophakia",
                                                  ifelse(
                                                    First$EL_severity == "REDACTED",
                                                    "Moderate",
                                                    ifelse(
                                                      First$EL_severity == "REDACTED",
                                                      "Mild",
                                                      ifelse(
                                                        First$EL_severity == "REDACTED",
                                                        "Severe",
                                                        NA
                                                      )
                                                    )
                                                  ))
Second$EL_severity <- ifelse(Second$EL_severity == "REDACTED",
                            "Spherophakia",
                            ifelse(
                              Second$EL_severity == "REDACTED",
                              "Moderate",
                              ifelse(
                                Second$EL_severity == "REDACTED",
                                "Mild",
                                ifelse(
                                  Second$EL_severity == "REDACTED",
                                  "Severe",
                                  NA
                                )
                              )
                            ))
Second

colnames(First)
table(First$Group)
table(First$EL_severity)

table(Second$Group)
table(Second$EL_severity)

pal_npg()(10)

head(First)

R
library(ggplot2)
library(tidyverse)
library(patchwork)

First <- First %>%
  mutate(Diagnosis = case_when(
    Group == "EL" ~ EL_severity,
    TRUE ~ Group
  ))

First$True_Label <- ifelse(First$Group == "EL", "Yes", "No")

predictors <- c("AI", "CZX", "LLZ", "WJ", "XYB", "ZZR", "HQY", "JWN", "WYL", "ZYL", "CXY", "JYX", "CZ")

diag_levels <- c("Mild", "Moderate", "Severe", "Spherophakia", "Cataract", "RE")

diag_colors <- c("#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF")
names(diag_colors) <- diag_levels

doc_groups <- list(
  "Ground Truth" = "True_Diagnosis",
  "AI Model" = "AI",
  "Chief" = c("JYX", "CZ"),
  "Senior Attending" = c("CZX","WJ"),
  "Attending" = c("LLZ", "JWN", "HQY"),
  "Resident" = c("XYB", "ZZR", "ZYL", "CXY", "WYL")
)

First$Diagnosis

plot_data <- First %>%
  mutate(

    Final_Diagnosis = case_when(
      Group == "EL" ~ EL_severity,
      TRUE ~ Group
    ),

    Final_Diagnosis = factor(Final_Diagnosis, levels = diag_levels),

    True_Binary = ifelse(Group == "EL", "Yes", "No")
  ) %>%
  arrange(Final_Diagnosis) %>%
  mutate(Patient_Order = 1:n())

long_df <- plot_data %>%
  dplyr::select(Patient_Order, Final_Diagnosis, True_Binary, all_of(predictors)) %>%
  pivot_longer(cols = all_of(predictors), names_to = "Rater", values_to = "Prediction")

long_df <- long_df %>%
  mutate(

    Is_Wrong = ifelse(Prediction != True_Binary, "Wrong", "Correct"),

    Rater_Group = case_when(
      Rater == "AI" ~ "AI Model",
      Rater %in% doc_groups$Chief ~ "Chief",
      Rater %in% doc_groups$`Senior Attending` ~ "Senior Attending",
      Rater %in% doc_groups$Attending ~ "Attending",
      Rater %in% doc_groups$Resident ~ "Resident"
    ),

    Rater = factor(Rater, levels = rev(c("AI",doc_groups$Chief,
                                         doc_groups$`Senior Attending`,
                                         doc_groups$Attending,doc_groups$Resident )))
  )

gt_df <- plot_data %>%
  dplyr::select(Patient_Order, Final_Diagnosis) %>%
  mutate(
    Rater = "Ground Truth",
    Rater_Group = "Ground Truth",
    Is_Wrong = "Ground Truth",
    y_pos = length(predictors) + 2
  )

combined_plot_data <- bind_rows(
  long_df %>% mutate(Fill_Color = ifelse(Is_Wrong == "Wrong", "Wrong", "Correct")),
  gt_df %>% mutate(Fill_Color = as.character(Final_Diagnosis))
)

combined_plot_data$Rater_Group <- factor(combined_plot_data$Rater_Group,
                                         levels = c("Ground Truth", "AI Model", "Chief",
                                                    "Senior Attending",
                                                    "Attending",
                                                    "Resident"))

all_raters <- c("Ground Truth", "AI", doc_groups$Chief ,doc_groups$`Senior Attending`,
                doc_groups$Attending, doc_groups$Resident)
combined_plot_data$Rater <- factor(combined_plot_data$Rater, levels = rev(all_raters))

my_colors <- c(diag_colors,
               "Wrong" = "#E64B35FF",
               "Correct" = "#F0F0F050")

p <- ggplot(combined_plot_data, aes(x = Patient_Order, y = Rater)) +

  geom_tile(aes(fill = Fill_Color), color = "black", size = 0.2) +

  scale_fill_manual(values = my_colors, na.value = "white", name = "Legend") +

  facet_grid(Rater_Group ~ Final_Diagnosis, scales = "free", space = "free") +

  theme_minimal() +
  theme(

    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),

    strip.background = element_rect(
                                    color = NA),
    strip.text.x = element_text(face = "bold", size = 10, angle = 0),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),

    legend.position = "bottom"
  ) +
  labs(title = "Diagnostic Performance Heatmap")+
  theme(
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )

print(p)
ggsave("Figures/plot_result_data_samplefirst.tiff",
       width = 16,
       height = 4,
       units = "in",
       dpi = 300)

Second <- Second %>%
  mutate(Diagnosis = case_when(
    Group == "EL" ~ EL_severity,
    TRUE ~ Group
  ))

Second$True_Label <- ifelse(Second$Group == "EL", "Yes", "No")

predictors <- c("AI", "CZX", "LLZ", "WJ", "XYB", "ZZR", "HQY", "JWN", "WYL", "ZYL", "CXY", "JYX", "CZ")

diag_levels <- c("Mild", "Moderate", "Severe", "Spherophakia", "Cataract", "RE")

diag_colors <- c("#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF")
names(diag_colors) <- diag_levels

doc_groups <- list(
  "Ground Truth" = "True_Diagnosis",
  "AI Model" = "AI",
  "Senior Attending" = c("JYX", "CZ"),
  "Attending" = c("CZX","WJ"),
  "Resident" = c("LLZ", "JWN", "HQY"),
  "Intern" = c("XYB", "ZZR", "ZYL", "CXY", "WYL")
)

Second$Diagnosis
Second$AI <- Second$predicted_label

plot_data_2 <- Second %>%
  mutate(

    Final_Diagnosis = case_when(
      Group == "EL" ~ EL_severity,
      TRUE ~ Group
    ),

    Final_Diagnosis = factor(Final_Diagnosis, levels = diag_levels),

    True_Binary = ifelse(Group == "EL", "Yes", "No")
  ) %>%
  arrange(Final_Diagnosis) %>%
  mutate(Patient_Order = 1:n())

long_df_2 <- plot_data_2 %>%
  select(Patient_Order, Final_Diagnosis, True_Binary, all_of(predictors)) %>%
  pivot_longer(cols = all_of(predictors), names_to = "Rater", values_to = "Prediction")

long_df_2 <- long_df_2 %>%
  mutate(

    Is_Wrong = ifelse(Prediction != True_Binary, "Wrong", "Correct"),

    Rater_Group = case_when(
      Rater == "AI" ~ "AI Model",
      Rater %in% doc_groups$`Senior Attending` ~ "Senior Attending",
      Rater %in% doc_groups$Attending ~ "Attending",
      Rater %in% doc_groups$Resident ~ "Resident",
      Rater %in% doc_groups$Intern ~ "Intern"
    ),

    Rater = factor(Rater, levels = rev(c("AI",doc_groups$`Senior Attending`,
                                         doc_groups$Attending, doc_groups$Resident,doc_groups$Intern )))
  )

gt_df_2 <- plot_data_2 %>%
  select(Patient_Order, Final_Diagnosis) %>%
  mutate(
    Rater = "Ground Truth",
    Rater_Group = "Ground Truth",
    Is_Wrong = "Ground Truth",
    y_pos = length(predictors) + 2
  )

combined_plot_data_2 <- bind_rows(
  long_df_2 %>% mutate(Fill_Color = ifelse(Is_Wrong == "Wrong", "Wrong", "Correct")),
  gt_df_2 %>% mutate(Fill_Color = as.character(Final_Diagnosis))
)

combined_plot_data_2$Rater_Group <- factor(combined_plot_data_2$Rater_Group,
                                         levels = c("Ground Truth", "AI Model", "Senior Attending",
                                                    "Attending",
                                                    "Resident",
                                                    "Intern"))

all_raters <- c("Ground Truth", "AI", doc_groups$`Senior Attending`,
                doc_groups$Attending, doc_groups$Resident,doc_groups$Intern)
combined_plot_data_2$Rater <- factor(combined_plot_data_2$Rater, levels = rev(all_raters))

my_colors <- c(diag_colors,
               "Wrong" = "#E64B35FF",
               "Correct" = "#F0F0F050")

p <- ggplot(combined_plot_data_2, aes(x = Patient_Order, y = Rater)) +

  geom_tile(aes(fill = Fill_Color), color = "black", size = 0.2) +

  scale_fill_manual(values = my_colors, na.value = "white", name = "Legend") +

  facet_grid(Rater_Group ~ Final_Diagnosis, scales = "free", space = "free") +

  theme_minimal() +
  theme(

    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),

    strip.background = element_rect(
      color = NA),
    strip.text.x = element_text(face = "bold", size = 10, angle = 0),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),

    legend.position = "bottom"
  ) +
  labs(title = "Diagnostic Performance Heatmap")+
  theme(
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )

print(p)
ggsave("Figures/plot_result_data_sampleSecond.tiff",
       width = 16,
       height = 4,
       units = "in",
       dpi = 300)

doctor_cols <- c("CZX", "LLZ", "WJ", "XYB", "ZZR", "HQY", "JWN", "WYL", "ZYL", "CXY", "JYX", "CZ")

calc_doc_error_rate <- function(row, true_label, doc_cols) {

  doc_diagnoses <- row[doc_cols]

  wrong_count <- sum(doc_diagnoses != true_label, na.rm = TRUE)
  total_docs <- length(doc_cols)

  return(wrong_count / total_docs)
}

First_Analysis <- First

First_Analysis$Doc_Error_Rate <- apply(First_Analysis, 1, function(x) {

  true_lbl <- x["True_Label"]
  doc_diag <- x[doctor_cols]
  mean(doc_diag != true_lbl, na.rm = TRUE)
})

First_Analysis$AI_Wrong <- ifelse(First_Analysis$AI != First_Analysis$True_Label, TRUE, FALSE)

First_Analysis$AI_Error_Score <- ifelse(First_Analysis$AI_Wrong, 1, 0)

Hard_Cases <- First_Analysis %>%
  filter(AI_Wrong == TRUE | Doc_Error_Rate > 0) %>%

  arrange(desc(Doc_Error_Rate), desc(AI_Wrong))

head(Hard_Cases)
write.xlsx(Hard_Cases, "Hard_Cases_1.xlsx")

Second_Analysis <- Second

Second_Analysis$Doc_Error_Rate <- apply(Second_Analysis, 1, function(x) {

  true_lbl <- x["True_Label"]
  doc_diag <- x[doctor_cols]
  mean(doc_diag != true_lbl, na.rm = TRUE)
})

Second_Analysis$AI_Wrong <- ifelse(Second_Analysis$AI != Second_Analysis$True_Label, TRUE, FALSE)

Second_Analysis$AI_Error_Score <- ifelse(Second_Analysis$AI_Wrong, 1, 0)

Hard_Cases2 <- Second_Analysis %>%
  filter(AI_Wrong == TRUE | Doc_Error_Rate > 0) %>%

  arrange(desc(Doc_Error_Rate), desc(AI_Wrong))

head(Hard_Cases2)
write.xlsx(Hard_Cases2, "Hard_Cases_2.xlsx")
