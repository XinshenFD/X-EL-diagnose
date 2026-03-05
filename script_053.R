# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

train_data_for_table1
colnames(train_data_for_table1)
summary(train_data_for_table1)
212+380+135+74
train_data_for_pca <- train_data_for_table1
train_data_for_pca$Group <- as.character(train_data_for_pca$Group)
train_data_for_pca$EL_severity <- as.character( train_data_for_pca$EL_severity)
train_data_for_pca$EL_severity <- ifelse(train_data_for_pca$Group == "EL",
       train_data_for_pca$EL_severity,
       "No ectopia")

train_data_for_pca$gender <- factor(train_data_for_pca$gender,
                                    levels = c("Female", "Male"))
table(train_data_for_pca$EL_severity)
train_data_for_pca$EL_severity <- factor(train_data_for_pca$EL_severity,
                                         levels = c("No ectopia", "Mild", "Moderate", "Severe",
                                                    "Spherophakia"))
table(train_data_for_pca$Group)
train_data_for_pca$Group <- factor(train_data_for_pca$Group,
                                   levels = c("EL", "Cataract", "RE"))
summary(train_data_for_pca)
train_data_for_pca
train_data_for_pca_select <- dplyr::select(train_data_for_pca,
                                        laterality,
                                        gender,
                                        Age,
                                        pre_S,
                                        pre_C,
                                        AL,
                                        K1,
                                        K2,
                                        KSE, K_c, ACD, LT, WTW,   LogMAR,
                                        EL_severity, Group)
train_data_for_pca_select$laterality <- factor(train_data_for_pca_select$laterality,
                                               levels = c("Right", "Left"))

summary(train_data_for_pca_select)

df <- train_data_for_pca_select
df$laterality <- as.numeric(df$laterality)
df$gender <- as.numeric(df$gender)

numeric_cols <- c("Age", "pre_S", "pre_C", "AL", "K1", "K2", "KSE",
                  "K_c", "ACD", "LT", "WTW", "LogMAR", "laterality", "gender")

factor_cols <- c( "EL_severity", "Group")

set.seed(123)
df_imputed <- kNN(df, variable = numeric_cols, k = 5)

df_clean <- df_imputed[, 1:ncol(df)]

pca_result <- prcomp(df_clean[, numeric_cols], scale. = TRUE)

pca_data <- data.frame(pca_result$x[, 1:2])
pca_data <- cbind(pca_data, df_clean[, factor_cols])

p1 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Group, fill = Group)) +
  geom_point(alpha = 0.6, size = 3) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) +
  theme_minimal() +
  scale_color_nejm() +
  scale_fill_nejm() +
  labs(
       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)")) +

  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text(),
        plot.background = element_rect(fill = "white"))

p1

ggsave("Figures/PCA.tiff",
       width = 8,
       height = 4.5,
       units = "in",
       dpi = 300)

print(p1)

pca_data$Plot_Label <- as.character(pca_data$EL_severity)

pca_data$Plot_Label[pca_data$Group == "RE"] <- "RE"
pca_data$Plot_Label[pca_data$Group == "Cataract"] <- "Cataract"

plot_data_2 <- pca_data %>% filter(!is.na(Plot_Label) & Plot_Label != "No ectopia")

level_order <- c("RE", "Cataract", "Mild", "Moderate", "Severe", "Spherophakia")
plot_data_2$Plot_Label <- factor(plot_data_2$Plot_Label, levels = level_order)

p2 <- ggplot(plot_data_2, aes(x = PC1, y = PC2, color = Plot_Label)) +
  geom_point(alpha = 0.7, size = 2.5) +

  stat_ellipse(data = subset(plot_data_2, Plot_Label %in% c("RE", "Cataract", "Mild", "Moderate", "Severe", "Spherophakia")),
               aes(fill = Plot_Label), geom = "polygon", alpha = 0.1, level = 0.90, show.legend = FALSE) +
  theme_minimal() +

  labs(

       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"),
       color = "Diagnosis / Severity") +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text(),
        plot.background = element_rect(fill = "white"))+
  theme(legend.position = "right")
p2
ggsave("Figures/PCA_reserve.tiff",
       width = 8,
       height = 4.5,
       units = "in",
       dpi = 300)

plot_data_opt <- pca_data

plot_data_opt$Plot_Label <- as.character(plot_data_opt$EL_severity)
plot_data_opt$Plot_Label[plot_data_opt$Group == "RE"] <- "RE"
plot_data_opt$Plot_Label[plot_data_opt$Group == "Cataract"] <- "Cataract"
plot_data_opt$Plot_Label <- as.character(plot_data_opt$Plot_Label)

plot_data_opt$Plot_Label[plot_data_opt$Plot_Label == "Mild"]         <- "EL: Mild"
plot_data_opt$Plot_Label[plot_data_opt$Plot_Label == "Moderate"]     <- "EL: Moderate"
plot_data_opt$Plot_Label[plot_data_opt$Plot_Label == "Severe"]       <- "EL: Severe"
plot_data_opt$Plot_Label[plot_data_opt$Plot_Label == "Spherophakia"] <- "EL: Spherophakia"

level_order_opt <- c("RE", "Cataract",
                     "EL: Mild", "EL: Moderate", "EL: Severe", "EL: Spherophakia")

plot_data_opt$Plot_Label <- factor(plot_data_opt$Plot_Label, levels = level_order_opt)

level_order_opt <- c("RE", "Cataract",
                     "EL: Mild", "EL: Moderate", "EL: Severe", "EL: Spherophakia")
plot_data_opt$Plot_Label <- factor(plot_data_opt$Plot_Label, levels = level_order_opt)
plot_data_opt

plot_data_opt_2 <- subset(plot_data_opt,
                        is.na(plot_data_opt$EL_severity) == F)

p2_opt <- ggplot(plot_data_opt_2, aes(x = PC1, y = PC2)) +

  geom_point(aes(color = Plot_Label), alpha = 0.7, size = 2) +

  stat_ellipse(data = subset(plot_data_opt, Plot_Label %in% c("RE", "Cataract")),
               aes(fill = Plot_Label,color = Plot_Label),
               geom = "polygon", alpha = 0.15, level = 0.95, show.legend = FALSE) +

  stat_ellipse(data = subset(plot_data_opt, Group == "EL"),
               aes(group = 1),
               color = "black",  level = 0.95, show.legend = F) +

  theme_minimal() +

  labs(x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"),
       color = "Diagnosis Group") +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman",face="bold", size=12),
        axis.text.x = element_text(),
        plot.background = element_rect(fill = "white"))+
  theme(legend.position = "right")
p2_opt
ggsave("Figures/PCA_reserve_2.tiff",
       width = 8,
       height = 4.5,
       units = "in",
       dpi = 300)
