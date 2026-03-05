# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

library(ggrepel)
library(extrafont)

library(glmnet)

colnames(training_set)
x_lasso <- as.matrix(training_set[, c(1:8, 11:13)])

y_lasso <- as.matrix(training_set[, 14])
f_lasso <- glmnet(x_lasso, y_lasso, family="binomial", alpha=1)

plot(f_lasso,
     xvar = "lambda",
     label = TRUE)

cv_fit=cv.glmnet(x_lasso, y_lasso,family="binomial", intercept=F, alpha=1)
plot(cv_fit)

coef_matrix <- as.matrix(coef(f_lasso))[-1, ]
coef_df <- as.data.frame(t(coef_matrix))
coef_df$log_lambda <- log(f_lasso$lambda)
coef_long <- reshape2::melt(coef_df, id.vars = "log_lambda",
                            variable.name = "Feature", value.name = "Coefficient")

active_coefs <- coef(f_lasso, s = cv_fit$lambda.min)[-1, , drop = FALSE]
active_vars <- rownames(active_coefs)[active_coefs[, 1] != 0]

label_data <- subset(coef_long, log_lambda == max(log_lambda) & Feature %in% active_vars)

min_lambda_idx <- which.min(abs(f_lasso$lambda - cv_fit$lambda.min))
label_data <- subset(coef_long, abs(log_lambda - log(cv_fit$lambda.min)) < 0.1 & Feature %in% active_vars)

label_data <- label_data[!duplicated(label_data$Feature), ]

table(coef_long$Feature)
coef_long$Feature <- as.character(coef_long$Feature)
coef_long <- coef_long %>%
  mutate(Feature = case_match(Feature,
                              "gender" ~ "Gender",
                              "pre_S" ~ "Sphere",
                              "pre_C" ~ "Cylinder",
                              "LogMAR" ~ "BCVA (logMAR)",

                              "K1" ~ "SimK1",
                              "K2" ~ "SimK2",
                              .default = Feature
  ))
coef_long
active_vars <- c(active_vars, "Age","Gender", "Sphere", "Cylinder", "BCVA (logMAR)",
                 "mean Keratometry", "SimK1", "SimK2")
active_vars <- c(active_vars[1:10], active_vars[12:19])
coef_plot <- ggplot(coef_long, aes(x = log_lambda, y = Coefficient, group = Feature)) +

  geom_line(color = "grey80", alpha = 0.8, size = 1.2) +

  geom_line(data = subset(coef_long, Feature %in% active_vars),
            aes(color = Feature), size = 0.8) +

  geom_vline(xintercept = log(cv_fit$lambda.min),
             linetype = "dashed", color = "black", alpha = 0.6) +

  geom_vline(xintercept = log(cv_fit$lambda.1se),
             linetype = "dotted", color = "black", alpha = 0.6, size = 0.8) +

  labs(x = "Log(Lambda)", y = "Coefficients", title = "LASSO Coefficient Profiles",
       color = "Features") +
  scale_color_ucscgb() +
  theme_classic() +
  theme(axis.text = element_text(size=10, face="bold", family = "Times New Roman"),
        text = element_text(family = "Times New Roman", face="bold", size=12),
        legend.title = element_text(size = 11),
                legend.text = element_text(size = 9))

coef_plot
ggsave("Figures/Lasso_coef_plot.tiff",
       width = 7,
       height = 5,
       units = "in",
       dpi = 300)

logistic_model_varible_select<- glm(Group ~  Age+ gender+ pre_S+ pre_C+LogMAR+AL+ K1+K2+ ACD+LT+WTW,
                                    data = training_set,
                                    family = binomial(link = "logit"))
summary(logistic_model_varible_select)

library(gtsummary)
library(flextable)

table_output <- logistic_model_varible_select %>%
  tbl_regression(
    exponentiate = FALSE,
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  add_global_p() %>%
  bold_p() %>%
  as_flex_table()

save_as_docx(table_output, path = "tables/Logistic_Model_Result.docx")
