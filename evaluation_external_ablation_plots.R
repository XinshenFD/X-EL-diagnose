# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

xgb_probs
xgb_roc
xgb_probs_no_LT
xgb_no_LT_roc
xgb_probs_no_ACD_LT
xgb_no_ACD_LT_roc
xgb_probs_no_ACD
xgb_no_ACD_roc

xgb_probs
xgb_probs_no_LT
xgb_probs_no_ACD
xgb_probs_no_ACD_LT

extra_validation_set_moreplot <-  data_extras_new1202
extra_validation_set_moreplot$Group <- as.character(extra_validation_set_moreplot$Group)
actual_outcome <- ifelse(extra_validation_set_moreplot$Group == "Yes", 1, 0)

probs_df_extra_validation <- data.frame(
  obs = extra_validation_set_moreplot$Group,
  obs_num = actual_outcome,
  `XGB-Full` = xgb_probs$Yes,
  `XGB-noLT` = xgb_probs_no_LT$Yes,
  `XGB-noACD` = xgb_probs_no_ACD$Yes,
  `XGB-noLT/ACD` = xgb_probs_no_ACD_LT$Yes
)
model_names <- c("XGB-Full",
                 "XGB-noLT",
                 "XGB-noACD",
                 "XGB-noLT/ACD")

colnames(probs_df_extra_validation) <- c("obs", "obs_num","XGB-Full",
                                         "XGB-noLT",
                                         "XGB-noACD",
                                         "XGB-noLT/ACD")
pr_long_df <- probs_df_extra_validation %>%
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

target_levels <- c("XGB-Full",
                   "XGB-noLT",
                   "XGB-noACD",
                   "XGB-noLT/ACD")
pr_data$Model <- factor(pr_data$Model, levels = target_levels)
auprc_values_clean <- auprc_values
auprc_labels <- paste0(auprc_values_clean$Model, " (AUPRC=", round(auprc_values_clean$.estimate, 3), ")")
names(auprc_labels) <- auprc_values_clean$Model
plot_pr_extra_validation <- ggplot(pr_data, aes(x = recall, y = precision, color = Model)) +
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
plot_pr_extra_validation

ggsave("Figures/plot_pr_extra_validation.tiff",
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
probs_df_extra_validation

cal_list <- list()
for(m in model_names){
  cal_list[[m]] <- get_calibration_data(probs_df_extra_validation[[m]], probs_df_extra_validation$obs, m)
}
library(dplyr)
cal_df_extra_validation <- bind_rows(cal_list)

cal_df_extra_validation
target_levels <- c("XGB-Full",
                   "XGB-noLT",
                   "XGB-noACD",
                   "XGB-noLT/ACD")

cal_df_extra_validation$Model <- as.character(cal_df_extra_validation$Model)

cal_df_extra_validation$Model <- factor(cal_df_extra_validation$Model, levels = target_levels)

plot_cal_extra_validation <- ggplot(cal_df_extra_validation, aes(x = midpoint, y = Percent, color = Model)) +

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
plot_cal_extra_validation

ggsave("Figures/plot_cal_extra_validation.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

dca_result <- dca(
  data = probs_df_extra_validation,
  formula = obs_num ~ `XGB-Full` +  `XGB-noLT` + `XGB-noACD`+`XGB-noLT/ACD`,
  thresholds = seq(0, 0.99, by = 0.01)
)

dca_df <- as_tibble(dca_result)
target_levels <- c("XGB-Full",
                   "XGB-noLT",
                   "XGB-noACD",
                   "XGB-noLT/ACD", "Treat None",
                   "Treat All")
dca_df$label<- factor(dca_df$label, levels = target_levels)

my_colors <- c(
  "XGB-Full" = nejm_cols[1],
  "XGB-noLT" = nejm_cols[2],
  "XGB-noACD" = nejm_cols[3],
  "XGB-noLT/ACD" = nejm_cols[4],
  "Treat None" = "black",
  "Treat All" = "red"
  )
my_linetypes <- c(
  "XGB-Full" = "solid",
  "XGB-noLT" = "solid",
  "XGB-noACD" = "solid",
  "XGB-noLT/ACD" = "solid",
  "Treat None" = "dashed",
  "Treat All" = "dashed"
)

plot_dca_extra_validation <- ggplot(dca_df, aes(x = threshold, y = net_benefit, color = label,  linetype = label)) +
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

plot_dca_extra_validation

ggsave("Figures/plot_dca_extra_validation.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

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

model_names <- c("XGB-Full",
                 "XGB-noLT",
                 "XGB-noACD",
                 "XGB-noLT/ACD")

cic_plot_data_list <- list()

set.seed(123)

for (m in model_names) {
  message(paste("REDACTED", m, "..."))

  cic_plot_data_list[[m]] <- get_cic_bootstrap(probs_df_extra_validation,
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

cic_final_df_full<- subset(cic_final_df,
                           cic_final_df$Model == "XGB-Full")

plot_cic_facet_full <- ggplot(cic_final_df_full, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.04),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.1, y = 900, label = paste0("T = ", 0.04)),
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
    title = "XGB-Full",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_full
ggsave("Figures/plot_cic_facet_full.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

print(intersection_df)

cic_final_df_nolt<- subset(cic_final_df,
                           cic_final_df$Model == "XGB-noLT")

plot_cic_facet_nolt <- ggplot(cic_final_df_nolt, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.12),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.2, y = 900, label = paste0("T = ", 0.12)),
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
    title = "XGB-noLT",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_nolt

ggsave("Figures/plot_cic_facet_nolt.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

print(intersection_df)

cic_final_df_noacd<- subset(cic_final_df,
                           cic_final_df$Model == "XGB-noACD")

plot_cic_facet_noacd <- ggplot(cic_final_df_noacd, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.06),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.15, y = 900, label = paste0("T = ", 0.06)),
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
    title = "XGB-noACD",
    x = "Threshold Probability",
    y = "Number per 1000 patients"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_noacd

ggsave("Figures/plot_cic_facet_noacd.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

print(intersection_df)

cic_final_df_noacdlt<- subset(cic_final_df,
                            cic_final_df$Model == "XGB-noLT/ACD")

plot_cic_facet_noacdlt <- ggplot(cic_final_df_noacdlt, aes(x = Threshold)) +

  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper, fill = "No. High Risk"), alpha = 0.2) +
  geom_line(aes(y = Risk_Mean, color = "No. High Risk"), size = 0.8) +

  geom_ribbon(aes(ymin = TP_Lower, ymax = TP_Upper, fill = "No. High Risk with Event"), alpha = 0.2) +
  geom_line(aes(y = TP_Mean, color = "No. High Risk with Event"), size = 0.8) +

  geom_vline(aes(xintercept = 0.12),
             linetype = "dashed", color = "black", size = 0.6) +

  geom_text(
    aes(x = 0.2, y = 900, label = paste0("T = ", 0.12)),
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
    axis.text = element_text(size = 15, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),

    legend.position = c(0.80, 0.90),

    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
plot_cic_facet_noacdlt

ggsave("Figures/plot_cic_facet_noacdlt.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

layout <- '
AAAAAABBBBBB
AAAAAABBBBBB
AAAAAABBBBBB
AAAAAABBBBBB
AAAAAABBBBBB
CCCCCCDDDDDD
CCCCCCDDDDDD
CCCCCCDDDDDD
CCCCCCDDDDDD
CCCCCCDDDDDD
'
plot_cic_facet_full + plot_cic_facet_nolt +
  plot_cic_facet_noacd+guide_area() +
  plot_layout(guides = 'collect', design = layout)
ggsave("Figures/plot_cic_facet_extra_validation.tiff",
       width = 12,
       height = 8,
       units = "in",
       dpi = 300)

cic_final_df$Model
xgb_probs
xgb_roc
xgb_probs_no_LT
xgb_no_LT_roc
xgb_probs_no_ACD_LT
xgb_no_ACD_LT_roc
xgb_probs_no_ACD
xgb_no_ACD_roc
roc_extra_validarion <- bind_rows(
  data.frame(
    Model = "XGB-Full",
    TPR = xgb_roc$sensitivities,
    FPR = 1 - xgb_roc$specificities
  ),
  data.frame(
    Model = "XGB-noLT",
    TPR = xgb_no_LT_roc$sensitivities,
    FPR = 1 - xgb_no_LT_roc$specificities
  ),
  data.frame(
    Model = "XGB-noACD",
    TPR = xgb_no_ACD_roc$sensitivities,
    FPR = 1 - xgb_no_ACD_roc$specificities
  ),
  data.frame(
    Model = "XGB-noLT/ACD",
    TPR = xgb_no_ACD_LT_roc$sensitivities,
    FPR = 1 - xgb_no_ACD_LT_roc$specificities
  )
)

auc_extra_validarion_values <- c(
  `XGB-Full` = pROC::auc(xgb_roc),
  `XGB-noLT`= pROC::auc(xgb_no_LT_roc),
  `XGB-noACD` = pROC::auc(xgb_no_ACD_roc),
  `XGB-noLT/ACD` = pROC::auc(xgb_no_ACD_LT_roc)
)

names(auc_extra_validarion_values) <- c("XGB-Full", "XGB-noLT",
                                        "XGB-noACD", "XGB-noLT/ACD")
roc_extra_validarion$Model <- factor(roc_extra_validarion$Model,
                              levels = names(auc_extra_validarion_values),
                              labels = paste0(names(auc_extra_validarion_values),
                                              " (AUC=", round(auc_extra_validarion_values, 3), ")")
)

plotmodel_extra_validation <- ggplot(roc_extra_validarion, aes(x = FPR, y = TPR, color = Model)) +
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

ggsave("Figures/plotmodel_extra_validation.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)
