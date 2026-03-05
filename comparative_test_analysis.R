# Sanitized script for public submission.
# Comparative test analysis for Phase 1 vs Phase 2.

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

input_file <- "data/comparative_test_input.csv"
if (!file.exists(input_file)) {
  stop("Missing input file: data/comparative_test_input.csv")
}

cmp <- readr::read_csv(input_file, show_col_types = FALSE)
if (nrow(cmp) == 0) {
  stop("Input file is empty. Please fill data/comparative_test_input.csv first.")
}

required_cols <- c(
  "case_id", "phase", "clinician_id", "clinician_level",
  "true_label", "pred_label", "time_seconds"
)
missing_cols <- setdiff(required_cols, colnames(cmp))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

cmp <- cmp %>%
  mutate(
    phase = factor(phase, levels = c("Phase1", "Phase2")),
    true_label = factor(true_label, levels = c("No", "Yes")),
    pred_label = factor(pred_label, levels = c("No", "Yes")),
    clinician_level = as.factor(clinician_level),
    time_seconds = as.numeric(time_seconds)
  )

metric_fn <- function(df) {
  tp <- sum(df$pred_label == "Yes" & df$true_label == "Yes", na.rm = TRUE)
  tn <- sum(df$pred_label == "No" & df$true_label == "No", na.rm = TRUE)
  fp <- sum(df$pred_label == "Yes" & df$true_label == "No", na.rm = TRUE)
  fn <- sum(df$pred_label == "No" & df$true_label == "Yes", na.rm = TRUE)

  acc <- (tp + tn) / max(tp + tn + fp + fn, 1)
  sen <- tp / max(tp + fn, 1)
  spe <- tn / max(tn + fp, 1)

  data.frame(
    n = tp + tn + fp + fn,
    accuracy = acc,
    sensitivity = sen,
    specificity = spe
  )
}

clinician_phase_metrics <- cmp %>%
  group_by(clinician_id, clinician_level, phase) %>%
  group_modify(~ metric_fn(.x)) %>%
  ungroup() %>%
  left_join(
    cmp %>%
      group_by(clinician_id, phase) %>%
      summarise(median_time_seconds = median(time_seconds, na.rm = TRUE), .groups = "drop"),
    by = c("clinician_id", "phase")
  )

phase_summary <- clinician_phase_metrics %>%
  group_by(phase) %>%
  summarise(
    clinicians = n_distinct(clinician_id),
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    mean_specificity = mean(specificity, na.rm = TRUE),
    median_time_seconds = median(median_time_seconds, na.rm = TRUE),
    .groups = "drop"
  )

level_summary <- clinician_phase_metrics %>%
  group_by(clinician_level, phase) %>%
  summarise(
    clinicians = n_distinct(clinician_id),
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    median_time_seconds = median(median_time_seconds, na.rm = TRUE),
    .groups = "drop"
  )

paired_metrics <- clinician_phase_metrics %>%
  select(clinician_id, phase, accuracy, median_time_seconds) %>%
  pivot_wider(
    names_from = phase,
    values_from = c(accuracy, median_time_seconds)
  ) %>%
  drop_na()

acc_test <- wilcox.test(
  paired_metrics$accuracy_Phase1,
  paired_metrics$accuracy_Phase2,
  paired = TRUE,
  exact = FALSE
)

time_test <- wilcox.test(
  paired_metrics$median_time_seconds_Phase1,
  paired_metrics$median_time_seconds_Phase2,
  paired = TRUE,
  exact = FALSE
)

test_summary <- data.frame(
  metric = c("accuracy", "median_time_seconds"),
  p_value = c(acc_test$p.value, time_test$p.value)
)

dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(clinician_phase_metrics, "results/comparative_test_by_clinician.csv")
readr::write_csv(phase_summary, "results/comparative_test_phase_summary.csv")
readr::write_csv(level_summary, "results/comparative_test_level_summary.csv")
readr::write_csv(test_summary, "results/comparative_test_p_values.csv")

p_acc <- ggplot(clinician_phase_metrics, aes(x = phase, y = accuracy, group = clinician_id)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  theme_classic(base_size = 12) +
  labs(x = NULL, y = "Accuracy")

p_time <- ggplot(clinician_phase_metrics, aes(x = phase, y = median_time_seconds, group = clinician_id)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 2) +
  theme_classic(base_size = 12) +
  labs(x = NULL, y = "Median Time (seconds)")

ggsave("figures/comparative_test_accuracy.png", p_acc, width = 6, height = 4, dpi = 300)
ggsave("figures/comparative_test_time.png", p_time, width = 6, height = 4, dpi = 300)
