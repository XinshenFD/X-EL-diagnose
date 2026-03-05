# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

bg_X_sample <- data_predictors_af_logistic_no_ACD_LT_train
colnames(bg_X_sample)
colnames(sample2[1, model_features, drop = FALSE])
summary(bg_X_sample$Group)
shap_long <- kernelshap(model_no_ACD_LT_xgb$finalModel, X=sample2[1, model_features, drop = FALSE],
                        bg_X=bg_X_sample, feature_names =model_features)

shp <- shapviz(model_no_ACD_LT_xgb$finalModel, X_pred = as.matrix(sample2[1, model_features, drop = FALSE]))
sv_waterfall(shp, row_id = 1) +
  ggtitle("Patient 1: XGBoost Waterfall Plot")
p <- sv_waterfall(shp, row_id = 1) +
  labs(
    tag = "",
    family = "Times New Roman"
  ) +
  theme(
    axis.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    text = element_text(family = "Times New Roman", face = "bold", size = 12),
    axis.text.x = element_text(),
    plot.background = element_rect(fill = "white")
  )
library(cowplot)

ggdraw(p) +
  draw_label("Higher Risk", x = 0.05, y = 0.98, hjust = 0, vjust = 1,
             fontfamily = "Times New Roman", fontface = "bold", size = 12, color = "#BC3C29FF") +
  draw_label("Lower Risk", x = 0.95, y = 0.98, hjust = 1, vjust = 1,
             fontfamily = "Times New Roman", fontface = "bold", size = 12, color = "#0072B5FF")

plot_patient_waterfall <- function(model, patient_data_df, patient_index) {

  model_features <- model$feature_names

  patient_matrix <- as.matrix(patient_data_df[, model_features, drop = FALSE])

  shp <- shapviz(model,
                 patient_matrix)
  name_map <- c(
    "pre_S"  = "Sphere (D)",
    "pre_C"  = "Cylinder (D)",
    "LogMAR" = "BCVA (logMAR)",
    "AL"     = "Axial Length (mm)",
    "K1" = "K1 (D)",
    "K2" = "K2 (D)"
  )
  colnames(shp$X)[match(names(name_map), colnames(shp$X))] <- name_map
  colnames(shp$S)[match(names(name_map), colnames(shp$S))] <- name_map

  p <- sv_waterfall(shp,row_id = 1)+
    labs(
      tag = "",
      family = "Times New Roman"
    ) +
    theme(axis.text=element_text(size=10, face="bold", family = "Times New Roman"),
          text = element_text(family = "Times New Roman",face="bold", size=12),
          axis.text.x = element_text(),
          plot.background = element_rect(fill = "white"))

  ggdraw(p) +draw_label("Higher Risk", x = 0.05, y = 0.98, hjust = 0, vjust = 1,
               fontfamily = "Times New Roman", fontface = "bold", size = 12, color = "#BC3C29FF") +
    draw_label("Lower Risk", x = 0.95, y = 0.98, hjust = 1, vjust = 1,
               fontfamily = "Times New Roman", fontface = "bold", size = 12, color = "#0072B5FF")

}

plot_patient_waterfall (model_no_ACD_LT_xgb$finalModel,
                        patient_data_df = sample2[1, ],
                        patient_index = 1)

?sv_waterfall

predict(model_no_ACD_LT_xgb, xgb_no_ACD_LT_pred_best_misclassified[32, ], type = "prob")[,2]

plot_patient_waterfall (model_no_ACD_LT_xgb$finalModel,
                        patient_data_df = xgb_no_ACD_LT_pred_best_misclassified[32, ],
                        patient_index = 1)

getwd()
ggsave("Figures/plot_patient_waterfall.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)
