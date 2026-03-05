# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

validation_set_moreplot <- validation_set
validation_set_moreplot$Group <- as.character(validation_set_moreplot$Group)
actual_outcome <- ifelse(validation_set_moreplot$Group == "Yes", 1, 0)

log_probs_no_LT <- predict(model_no_LT_log, validation_set,type="prob")
knn_probs_no_LT <- predict(model_no_LT_knn, validation_set,type="prob")
rf_probs_no_LT <- predict(model_no_LT_rf, validation_set,type="prob")
SVM_probs_no_LT <- predict(model_no_LT_svm, validation_set,type="prob")
xgb_probs_no_LT <- predict(model_no_LT_xgb, validation_set,type="prob")
tree_probs_no_LT <- predict(model_no_LT_tree, validation_set,type="prob")

probs_df_no_LT <- data.frame(
  obs = validation_set_moreplot$Group,
  obs_num = actual_outcome,
  Logistic = log_probs_no_LT$Yes,
  KNN = knn_probs_no_LT$Yes,
  RandomForest = rf_probs_no_LT$Yes,
  SVM = SVM_probs_no_LT$Yes,
  XGBoost = xgb_probs_no_LT$Yes,
  DecisionTree = tree_probs_no_LT$Yes
)

model_names <- c("Logistic", "KNN", "RandomForest", "SVM", "XGBoost", "DecisionTree")

pr_long_df <- probs_df_no_LT %>%
  tidyr::pivot_longer(cols = all_of(model_names), names_to = "Model", values_to = "Prob")
pr_long_df$obs <- as.factor(pr_long_df$obs )

pr_data <- pr_long_df %>%
  group_by(Model) %>%
  pr_curve(obs, Prob, event_level = "second")

auprc_values <- pr_long_df %>%
  group_by(Model) %>%
  pr_auc(obs, Prob, event_level = "second")

auprc_labels <- paste0(auprc_values$Model, " (AUPRC=", round(auprc_values$.estimate, 3), ")")
names(auprc_labels) <- auprc_values$Model

target_levels <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree")

pr_data$Model <- as.character(pr_data$Model)
pr_data$Model[pr_data$Model == "RandomForest"] <- "Random Forest"
pr_data$Model[pr_data$Model == "DecisionTree"] <- "Decision Tree"

pr_data$Model <- factor(pr_data$Model, levels = target_levels)

auprc_values_clean <- auprc_values
auprc_values_clean$Model <- as.character(auprc_values_clean$Model)
auprc_values_clean$Model[auprc_values_clean$Model == "RandomForest"] <- "Random Forest"
auprc_values_clean$Model[auprc_values_clean$Model == "DecisionTree"] <- "Decision Tree"

auprc_labels <- paste0(auprc_values_clean$Model, " (AUPRC=", round(auprc_values_clean$.estimate, 3), ")")
names(auprc_labels) <- auprc_values_clean$Model

plot_pr_no_LT <- ggplot(pr_data, aes(x = recall, y = precision, color = Model)) +
  geom_line(size = 1) +

  labs(
    title = "Precision-Recall Curves",
    x = "Recall (Sensitivity)",
    y = "Precision (PPV)",
    color = "Model"
  ) +
  scale_fill_nejm(labels = auprc_labels)+
  scale_color_nejm(labels = auprc_labels)+
  theme_classic()+
  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text( )) +
  theme(

    legend.position = c(0.2, 0.25),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
plot_pr_no_LT
plot_pr_validation_LT <- plot_pr_no_LT
ggsave("Figures/plot_pr_validarion_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

get_calibration_data <- function(prob, obs, model_name, bins = 12) {

  obs_factor <- as.factor(obs)

  tmp_data <- data.frame(obs = obs_factor, prob = prob)

  cal <- calibration(obs ~ prob, data = tmp_data, cuts = bins, class = "Yes")

  res <- cal$data
  res$Model <- model_name
  return(res)
}
probs_df_no_LT

cal_list <- list()
for(m in model_names){
  cal_list[[m]] <- get_calibration_data(probs_df_no_LT[[m]], probs_df_no_LT$obs, m)
}
library(dplyr)
cal_df_no_LT <- bind_rows(cal_list)

cal_df_no_LT
target_levels <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree")

cal_df_no_LT$Model <- as.character(cal_df_no_LT$Model)
cal_df_no_LT$Model[cal_df_no_LT$Model == "RandomForest"] <- "Random Forest"
cal_df_no_LT$Model[cal_df_no_LT$Model == "DecisionTree"] <- "Decision Tree"

cal_df_no_LT$Model <- factor(cal_df_no_LT$Model, levels = target_levels)
plot_cal_validation_no_LT <- ggplot(cal_df_no_LT, aes(x = midpoint, y = Percent, color = Model)) +

  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 0.8, alpha =0.9) +
  theme_classic() +
  labs(
    title = "Calibration Curves",
    x = "Predicted Probability (%)",
    y = "Observed Percentage (%)",
    color = "Model"
  ) +
  scale_fill_nejm()+
  scale_color_nejm()+
  theme_classic()+
  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text( )) +
  theme(

    legend.position = c(0.2, 0.75),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
plot_cal_validation_no_LT

ggsave("Figures/plot_cal_validation_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

library(dcurves)

dca_result <- dca(
  data = probs_df_no_LT,
  formula = obs_num ~ Logistic + KNN + RandomForest + SVM + XGBoost + DecisionTree,
  thresholds = seq(0, 0.99, by = 0.01)
)

dca_df <- as_tibble(dca_result)

dca_df$label <- as.character(dca_df$label)
dca_df$label[dca_df$label == "RandomForest"] <- "Random Forest"
dca_df$label[dca_df$label == "DecisionTree"] <- "Decision Tree"
table(dca_df$label)
target_levels <- c("Logistic", "KNN", "Random Forest", "SVM", "XGBoost", "Decision Tree", "Treat None",
                   "Treat All")

dca_df$label<- factor(dca_df$label, levels = target_levels)

nejm_cols <- pal_nejm()(8)

my_colors <- c(
  "Logistic" = nejm_cols[1],
  "KNN" = nejm_cols[2],
  "Random Forest" = nejm_cols[3],
  "SVM" = nejm_cols[4],
  "XGBoost" = nejm_cols[5],
  "Decision Tree" = nejm_cols[6],
  "Treat None" = "black",
  "Treat All" = "red"
)
my_linetypes <- c(
  "Logistic" = "solid",
  "KNN" = "solid",
  "Random Forest" = "solid",
  "SVM" = "solid",
  "XGBoost" = "solid",
  "Decision Tree" = "solid",
  "Treat None" = "dashed",
  "Treat All" = "dashed"
)
table(dca_df$label)
plot_dca_no_LT <- ggplot(dca_df, aes(x = threshold, y = net_benefit, color = label,  linetype = label)) +
  geom_line(size = 1.0) +
  coord_cartesian(ylim = c(-0.05, max(dca_df$net_benefit) + 0.05)) +
  labs(
    title = "Decision Curve Analysis",
    x = "Threshold Probability",
    y = "Net Benefit",
    color = "Model",
    linetype = "Model"
  ) +

  scale_color_manual(values = my_colors) +

  scale_linetype_manual(values = my_linetypes) +
  theme_classic()+
  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text( )) +
  theme(

    legend.position = c(0.12, 0.40),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key = element_blank() ,
    legend.background = element_blank()
  )

plot_dca_no_LT
plot_dca_validation_no_LT <- plot_dca_no_LT

ggsave("Figures/plot_dca_validation_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)
pal_nejm()(10)

calc_cic_once <- function(prob, obs_num, thresholds) {
  n_total <- length(obs_num)
  res <- lapply(thresholds, function(t) {
    pred_pos_idx <- which(prob >= t)
    n_high_risk <- length(pred_pos_idx)
    if (n_high_risk > 0) {
      n_true_pos <- sum(obs_num[pred_pos_idx])
    } else {
      n_true_pos <- 0
    }

    data.frame(
      Threshold = t,
      N_High_Risk = n_high_risk / n_total * 1000,
      N_True_Positive = n_true_pos / n_total * 1000
    )
  })
  do.call(rbind, res)
}

get_cic_bootstrap <- function(data, model_col, obs_col, n_boot = 500) {

  obs_vec <- ifelse(data[[obs_col]] == "Yes", 1, 0)
  prob_vec <- data[[model_col]]
  thresholds <- seq(0, 1, 0.02)

  boot_results <- list()

  for (i in 1:n_boot) {

    indices <- sample(1:length(obs_vec), length(obs_vec), replace = TRUE)

    boot_res <- calc_cic_once(prob_vec[indices], obs_vec[indices], thresholds)
    boot_res$BootID <- i
    boot_results[[i]] <- boot_res
  }

  all_boot <- do.call(rbind, boot_results)

  summary_df <- all_boot %>%
    group_by(Threshold) %>%
    summarise(

      Risk_Mean = mean(N_High_Risk),
      Risk_Lower = quantile(N_High_Risk, 0.025),
      Risk_Upper = quantile(N_High_Risk, 0.975),

      TP_Mean = mean(N_True_Positive),
      TP_Lower = quantile(N_True_Positive, 0.025),
      TP_Upper = quantile(N_True_Positive, 0.975)
    ) %>%
    mutate(Model = model_col)

  return(summary_df)
}

model_names <- c("XGBoost", "RandomForest", "Logistic", "KNN", "SVM", "DecisionTree")

cic_plot_data_list <- list()

set.seed(123)

for (m in model_names) {
  message(paste("REDACTED", m, "..."))

  cic_plot_data_list[[m]] <- get_cic_bootstrap(probs_df_no_LT,
                                               model_col = m, obs_col = "obs", n_boot = 500)
}

cic_final_df <- do.call(rbind, cic_plot_data_list)

cic_final_df$Model <- factor(cic_final_df$Model, levels = model_names)

intersection_df <- cic_final_df %>%
  group_by(Model) %>%

  filter(Risk_Lower <= TP_Upper) %>%
  arrange(Threshold) %>%
  slice(1) %>%
  ungroup() %>%
  select(Model, Threshold, Risk_Mean)

print(intersection_df)

cic_final_df_xgb <- subset(cic_final_df,
                           cic_final_df$Model == "XGBoost")

plot_cic_facet_xgb_no_LT <- ggplot(cic_final_df_xgb, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.04),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.05, y = 900, label = paste0("T = ", 0.04)),
    hjust = -1,
    vjust = -0.2,
    family = "Times New Roman", size = 3.5, fontface = "bold") +

  scale_color_manual(name = NULL,
                     values = c("No. High Risk" = "#E64B35FF",
                                "No. High Risk with Event" = "#4DBBD5FF")) +
  scale_fill_manual(name = NULL,
                    values = c("No. High Risk" = "#E64B35FF",
                               "No. High Risk with Event" = "#4DBBD5FF")) +

  labs(
    title = "REDACTED",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 10),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_xgb_no_LT
ggsave("Figures/plot_cic_facet_validation_xgb_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

cic_final_df_rf <- subset(cic_final_df,
                          cic_final_df$Model == "RandomForest")
intersection_df
plot_cic_facet_rf_no_LT <- ggplot(cic_final_df_rf, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.24),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.4, y = 900, label = paste0("T = ", 0.24)),
    hjust = 1,
    vjust = -1,
    family = "Times New Roman", size = 3.5, fontface = "bold") +

  scale_color_manual(name = NULL,
                     values = c("No. High Risk" = "#E64B35FF",
                                "No. High Risk with Event" = "#4DBBD5FF")) +
  scale_fill_manual(name = NULL,
                    values = c("No. High Risk" = "#E64B35FF",
                               "No. High Risk with Event" = "#4DBBD5FF")) +

  labs(
    title = "Random Forest",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_rf_no_LT
ggsave("Figures/plot_cic_facet_validation_rf_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

cic_final_df_lg <- subset(cic_final_df,
                          cic_final_df$Model == "Logistic")
intersection_df
plot_cic_facet_lg_no_LT <- ggplot(cic_final_df_lg, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.34),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.5, y = 900, label = paste0("T = ", 0.34)),
    hjust = 1,
    vjust = -1,
    family = "Times New Roman", size = 3.5, fontface = "bold") +

  scale_color_manual(name = NULL,
                     values = c("No. High Risk" = "#E64B35FF",
                                "No. High Risk with Event" = "#4DBBD5FF")) +
  scale_fill_manual(name = NULL,
                    values = c("No. High Risk" = "#E64B35FF",
                               "No. High Risk with Event" = "#4DBBD5FF")) +

  labs(
    title = "Logistic",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_lg_no_LT
ggsave("Figures/plot_cic_facet_validation_lg_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

cic_final_df_knn <- subset(cic_final_df,
                           cic_final_df$Model == "KNN")
intersection_df
plot_cic_facet_knn_no_LT <- ggplot(cic_final_df_knn, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.12),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.30, y = 900, label = paste0("T = ", 0.12)),
    hjust = 1,
    vjust = -1,
    family = "Times New Roman", size = 3.5, fontface = "bold") +

  scale_color_manual(name = NULL,
                     values = c("No. High Risk" = "#E64B35FF",
                                "No. High Risk with Event" = "#4DBBD5FF")) +
  scale_fill_manual(name = NULL,
                    values = c("No. High Risk" = "#E64B35FF",
                               "No. High Risk with Event" = "#4DBBD5FF")) +

  labs(
    title = "KNN",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_knn_no_LT
ggsave("Figures/plot_cic_facet_validation_knn_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

cic_final_df_svm <- subset(cic_final_df,
                           cic_final_df$Model == "SVM")
intersection_df
plot_cic_facet_svm_no_LT <- ggplot(cic_final_df_svm, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.08),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.2, y = 900, label = paste0("T = ", 0.08)),
    hjust = 1,
    vjust = -1,
    family = "Times New Roman", size = 3.5, fontface = "bold") +

  scale_color_manual(name = NULL,
                     values = c("No. High Risk" = "#E64B35FF",
                                "No. High Risk with Event" = "#4DBBD5FF")) +
  scale_fill_manual(name = NULL,
                    values = c("No. High Risk" = "#E64B35FF",
                               "No. High Risk with Event" = "#4DBBD5FF")) +

  labs(
    title = "SVM",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_svm_no_LT
ggsave("Figures/plot_cic_facet_validation_svm_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

cic_final_df_dt <- subset(cic_final_df,
                          cic_final_df$Model == "DecisionTree")
intersection_df
plot_cic_facet_dt_no_LT <- ggplot(cic_final_df_dt, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.3),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.45, y = 900, label = paste0("T = ", 0.3)),
    hjust = 1,
    vjust = -1,
    family = "Times New Roman", size = 3.5, fontface = "bold") +

  scale_color_manual(name = NULL,
                     values = c("No. High Risk" = "#E64B35FF",
                                "No. High Risk with Event" = "#4DBBD5FF")) +
  scale_fill_manual(name = NULL,
                    values = c("No. High Risk" = "#E64B35FF",
                               "No. High Risk with Event" = "#4DBBD5FF")) +

  labs(
    title = "Decision Tree",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_dt_no_LT
ggsave("Figures/plot_cic_facet_validation_dt_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

library(patchwork)
layout <- '
AAAAAABBBBBBCCCCCC
AAAAAABBBBBBCCCCCC
AAAAAABBBBBBCCCCCC
AAAAAABBBBBBCCCCCC
AAAAAABBBBBBCCCCCC
DDDDDDEEEEEEFFFFFF
DDDDDDEEEEEEFFFFFF
DDDDDDEEEEEEFFFFFF
DDDDDDEEEEEEFFFFFF
DDDDDDEEEEEEFFFFFF
'
plot_cic_facet_rf_no_LT + plot_cic_facet_lg_no_LT +
  plot_cic_facet_dt_no_LT + plot_cic_facet_svm_no_LT +plot_cic_facet_knn_no_LT+guide_area() +
  plot_layout(guides = 'collect', design = layout)
ggsave("Figures/plot_cic_facet_validation_no_LT.tiff",
       width = 18,
       height = 8,
       units = "in",
       dpi = 300)

plotmodel_validation_set

plotmodel_validation_set_no_LT <- ggplot(roc_no_LT_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1, alpha=1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +

  labs(
    title = "ROC Curves for Models",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model"
  ) +
  scale_fill_nejm()+
  scale_color_nejm()+
  theme_classic()+
  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text( )) +
  theme(

    legend.position = c(0.75, 0.25),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
plotmodel_validation_set_no_LT
ggsave("Figures/plot_ROC_validation_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

library(SHAPforxgboost)
library(caret)
xgb_model_raw <- model_no_LT_xgb$finalModel

predictors_data_for_shap <- data_predictors_af_logistic_no_LT_train %>%
  select(-Group)

X_train_matrix <- as.matrix(predictors_data_for_shap )

shap_values_obj <- shapviz( model_no_LT_xgb$finalModel, X_pred = X_train_matrix)
colnames(shap_values_obj$X)
new_names_vector <- c("Sphere", "Cylinder", "BCVA (logMAR)", "AL", "K1", "ACD", "K2")
colnames(shap_values_obj) <- new_names_vector
p <- sv_importance(shap_values_obj,kind = "beeswarm") +

  labs(
    title = "Attributes of Characteristics in SHAP",
    x = "SHAP value",
    y = "Features"
  ) +

  theme_classic()+
  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text( )) +
  theme(

    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
p
ggsave("Figures/sv_importance_xgb_no_LT.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)
