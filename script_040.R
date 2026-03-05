# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

First_time
Second_time
First_accuracy <- read.xlsx("human_ai_result/result_df_1.xlsx")
Second_accuracy<- read.xlsx("human_ai_result/result_df_2.xlsx")

First_accuracy$Phase <- "First (No AI)"
Second_accuracy$Phase <- "Second (With AI)"

df_acc <- rbind(First_accuracy, Second_accuracy)

df_acc_doctors <- df_acc %>% filter(ID != "AI")

df_acc_long <- df_acc_doctors %>%
  pivot_longer(cols = c("Accuracy", "Sensitivity", "Specificity"),
               names_to = "Metric",
               values_to = "Value")

df_acc_long$Phase <- factor(df_acc_long$Phase, levels = c("First (No AI)", "Second (With AI)"))

First_time$Phase <- "First (No AI)"
Second_time$Phase <- "Second (With AI)"
First_accuracy$Phase <- "Without AI assist"
Second_accuracy$Phase <- "With AI assist"
phase_levels <- c("Without AI assist", "With AI assist")

df_time <- rbind(First_time, Second_time)
df_time$Phase <- factor(df_time$Phase, levels = c("First (No AI)", "Second (With AI)"))
library(ggpubr)
df_acc <- rbind(First_accuracy, Second_accuracy)

df_acc <- df_acc %>% filter(ID != "AI")

doc_group_map <- c(
  "CZX" = "Senior Attending", "LLZ" = "Attending", "WJ" = "Senior Attending",
  "XYB" = "Resident", "ZZR" = "Resident", "HQY" = "Attending",
  "JWN" = "Attending", "WYL" = "Resident", "ZYL" = "Resident", "CXY" = "Resident",
  "JYX" = "Chief", "CZ" = "Chief"
)
df_acc$Doc_Group <- doc_group_map[df_acc$ID]
df_acc$Doc_Group <- factor(df_acc$Doc_Group, levels = c(
  "Chief","Senior Attending",
                                                        "Attending",
                                                        "Resident"
                                                        ))

df_acc_long <- df_acc %>%
  pivot_longer(cols = c("Accuracy", "Sensitivity", "Specificity"),
               names_to = "Metric", values_to = "Value")

df_acc_long$Phase <- factor(df_acc_long$Phase, levels = phase_levels)

p_acc <- ggplot(df_acc_long, aes(x = Phase, y = Value)) +

  geom_violin(alpha=0.35,  color = NA) +
  geom_jitter(aes(fill = Doc_Group), size = 2, shape = 21, color = "white")+

  geom_boxplot(aes(), alpha = 0, width = 0.5, outlier.shape = NA) +

  facet_wrap(~ Metric, scales = "free_y") +

  stat_compare_means(method = "wilcox.test", paired = TRUE,
                     label = "p.format",
                     label.y = 1.1,label.x.npc =0.4 ) +

  scale_fill_npg() +
  ylim(0.6, 1.2)+
  theme_bw() +
  theme(

    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(y = "Score", title = "Comparison of Diagnostic Performance",
       fill = "Doctor Group")+
  theme(
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )
p_acc

ggsave("Figures/plot_result_doctor_ai_acc.tiff",
       width = 12,
       height = 4,
       units = "in",
       dpi = 300)

First_time$Phase <- "Without AI assist"
Second_time$Phase <- "With AI assist"
(median(Second_time$`Time(min)`) - median(First_time$`Time(min)`))/21.625

df_time <- rbind(First_time, Second_time)

colnames(df_time)[1] <- "ID"

df_time$Doc_Group <- doc_group_map[df_time$ID]
df_time$Doc_Group <- factor(df_time$Doc_Group, levels = c(
  "Chief","Senior Attending",
  "Attending",
  "Resident"
))
df_time$Phase <- factor(df_time$Phase, levels = phase_levels)

p_time <- ggplot(df_time, aes(x = Phase, y = `Time(min)`)) +

  geom_violin(alpha=0.35,  color = NA) +
  geom_jitter(aes(fill = Doc_Group), size = 2, shape = 21, color = "white")+
  geom_boxplot(aes(), alpha = 0, width = 0.5, outlier.shape = NA) +

  stat_compare_means(method = "wilcox.test", paired = TRUE,
                     label = "p.format",
                     label.y = 32,label.x.npc =0.4 ) +

  scale_fill_npg() +
  theme_bw() +
  theme(

    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 12)
  ) +
  labs(y = "Time (minutes)", title = "Comparison of Diagnosis Time",
       fill = "Doctor Group")+
  theme(
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.background = element_blank()
  )

print(p_time)

df_time$`Time(min)`

ggsave("Figures/plot_result_doctor_ai_time.tiff",
       width = 5,
       height = 4,
       units = "in",
       dpi = 300)
