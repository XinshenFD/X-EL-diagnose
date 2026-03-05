# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

data_predictors
length(data_predictors$Age)

inner_all <- 1:1870
1870*0.7
sample(inner_all, size = 1309)
table(duplicated(sample(inner_all, size = 1309)))

training_set <- data_predictors[training, ]
training_set
validation_set <- data_predictors[-training, ]

x_lasso <- as.matrix(training_set[, c(1:13)])
y_lasso <- as.matrix(training_set[, 14])
f_lasso <- glmnet(x_lasso, y_lasso, family="binomial", alpha=1)
plot(f_lasso,
     xvar = "lambda",
     label = TRUE)

cvfit=cv.glmnet(x_lasso, y_lasso,family="binomial", intercept=F, alpha=1)
plot(cvfit)

coef1 <- predict(f_lasso, s=cvfit$lambda.1se, type = "coefficients")
coef2 <- predict(f_lasso, s=cvfit$lambda.min, type = "coefficients")

coef2
summary(data_predictors)
data_predictors_af_lasso <- dplyr::select(data_predictors,
                                          Age,
                                          gender,
                                          pre_S,
                                          pre_C,
                                          LogMAR,
                                          AL,
                                          K1,
                                          KSE,
                                          ACD,
                                          LT,
                                          WTW,
                                          Group)

table(data_predictors_af_lasso$Group)

logistic_model_varible_select<- glm(Group ~  Age+ gender+ pre_S+ pre_C+LogMAR+AL+ K1+KSE+ ACD+LT+WTW,
    data = data_predictors_af_lasso,
    family = binomial(link = "logit"))
summary(logistic_model_varible_select)

data_predictors_af_logistic <- dplyr::select(data_predictors_af_lasso,
                                             pre_S,
                                             pre_C,
                                             LogMAR,
                                             AL,
                                             K1,
                                             ACD,
                                             LT,
                                             Group)
length(data_predictors$K2)
length(data_predictors_af_logistic_no_ACD_LT$K1)
table(rownames(data_predictors_af_logistic_no_ACD_LT) == rownames(data_predictors))

summary(glm(Group ~  pre_S+ pre_C+LogMAR+AL+ K1+ ACD+LT,
                                    data = data_predictors_af_lasso,
                                    family = binomial(link = "logit")))

 trainControl()
control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
set.seed(123)

control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
data_predictors_af_logistic
colnames(data_predictors_af_logistic)
summary(data_predictors_af_logistic)

data_predictors_af_logistic$Group <- ifelse(data_predictors_af_logistic$Group == 1,
                                         "Yes",
                                         "No")
data_predictors_af_logistic$Group <- factor(data_predictors_af_logistic$Group,
                                         levels = c("No", "Yes"))

data_predictors_af_logistic
summary(data_predictors_af_logistic)
which(rownames(data_predictors_af_logistic) %in% c("2692" ,"2702" ,"3602", "3614"))

data_predictors_af_logistic[c(1240 ,1241, 1329 ,1330), 8] <- "No"
data_predictors_af_logistic$Group
summary(data_predictors_af_logistic$Group)

data_predictors_af_logistic

data_predictors_af_logistic_have_ACD_LT <- cbind(data_predictors_af_logistic,
                                               data_predictors$K2)
colnames(data_predictors_af_logistic_have_ACD_LT)[9] <- "K2"

data_predictors_af_logistic_have_ACD_LT

yunnan_add <- read.xlsx("or_data/yunnantoczx.xlsx")
colnames(yunnan_add)
data_predictors_af_logistic_have_ACD_LT
data_predictors_af_logistic_have_ACD_LT_train <- data_predictors_af_logistic_have_ACD_LT[training, ]
colnames(data_predictors_af_logistic_have_ACD_LT_train)
summary(yunnan_add)
yunnan_add$CDVA <- 1
yunnan_add$LogMAR <- 0
yunnan_add$ACD <- as.numeric(yunnan_add$ACD)
yunnan_add$LT <- as.numeric(yunnan_add$LT)
yunnan_add$AL <- as.numeric(yunnan_add$AL)
yunnan_add$K1 <- as.numeric(yunnan_add$K1)
yunnan_add$K2 <- as.numeric(yunnan_add$K2)
colnames(yunnan_add)
yunnan_add_na <- na.omit(yunnan_add)
yunnan_add_na$Group <- "No"
yunnan_add_na_trainging <-dplyr::select(yunnan_add_na,
                                        pre_S,
                                        pre_C,
                                        LogMAR,
                                        AL,
                                        K1,
                                        ACD,
                                        LT,
                                        Group,
                                        K2)
data_predictors_af_logistic_have_ACD_LT_train <- rbind(data_predictors_af_logistic_have_ACD_LT_train,
                                                       yunnan_add_na_trainging)

tail(data_predictors_af_logistic_have_ACD_LT_train)
table(data_predictors_af_logistic_have_ACD_LT_train$Group)

model_log <- train(Group ~ ., data = data_predictors_af_logistic_have_ACD_LT_train,
                   method = "glm", family = "binomial",
                   trControl = control, metric = "ROC")
model_log

model_knn <- train(Group ~ ., data = data_predictors_af_logistic_have_ACD_LT_train,
                   method = "knn", trControl = control, metric = "ROC")
model_knn

model_rf <- train(Group ~ .,  data = data_predictors_af_logistic_have_ACD_LT_train,
                  method = "rf", trControl = control, metric = "ROC")
model_rf

model_svm <- train(Group ~ ., data =  data_predictors_af_logistic_have_ACD_LT_train,
                   method = "svmRadial", trControl = control, metric = "ROC")
model_svm

model_xgb <- train(Group ~ ., data = data_predictors_af_logistic_have_ACD_LT_train,
                   method = "xgbTree", trControl = control, metric = "ROC")
model_xgb

length(data_predictors_af_logistic_have_ACD_LT_train$K2)

model_tree <- train(Group ~ ., data = data_predictors_af_logistic_have_ACD_LT_train,
                    method = "rpart", trControl = control, metric = "ROC")
model_tree

class(lgb_model)
class(model_tree)

models <- list(Logistic = model_log, KNN = model_knn, RF = model_rf,
               SVM = model_svm, XGB = model_xgb,
               Tree = model_tree)

results <- lapply(models, function(mod){
  pred <- predict(mod, data_predictors_af_logistic, type = "prob")[, "Yes"]
  roc_obj <- roc(response = data_predictors_af_logistic$Group, predictor = pred)
  auc_val <- auc(roc_obj)
  list(pred = pred, roc = roc_obj, auc = auc_val)
})

plot(results$Logistic$roc, col = "red", lwd = 2, main = "ROC Curves (Test set)")
cols <- c("blue", "darkgreen", "orange", "purple", "brown", "pink")
i <- 1
for (mod in names(results)[-1]) {
  lines(results[[mod]]$roc, col = cols[i], lwd = 2)
  i <- i + 1
}
legend("bottomright",
       legend = paste(names(results), "AUC:", round(sapply(results, `[[`, "auc"), 3)),
       col = c("red", cols), lwd = 2)

mmdata <- do.call(rbind, lapply(names(results), function(m){
  data.frame(score = results[[m]]$pred,
             label = data_predictors_af_logistic$Group,
             model = m)
}))
mmdata$label <- ifelse(mmdata$label == "Yes", 1, 0)

precrec_obj <- evalmod(scores = split(mmdata$score, mmdata$model),
                       labels = split(mmdata$label, mmdata$model))
autoplot(precrec_obj, "PRC") + theme_minimal()

data_predictors_af_logistic_1 <- data_predictors_af_logistic

data_predictors_af_logistic_1$pred_log <- results$Logistic$pred
calib <- val.prob(data_predictors_af_logistic_1$pred_log, as.numeric(data_predictors_af_logistic_1$Group) - 1, m = 10)

cal_data <- lapply(results, function(r){
  data.frame(obs = as.numeric(data_predictors_af_logistic_1$Group) - 1,
             pred = r$pred)
})
names(cal_data) <- names(results)
plotCalib <- function(df, name){
  cal <- val.prob(df$pred, df$obs, m = 10, pl = FALSE)
  lines(cal$mean.pred, cal$mean.obs, type = "b", pch = 19)
}
plot(0, 0, xlim = c(0,1), ylim = c(0,1), type = "n",
     xlab = "Predicted Probability", ylab = "Observed Probability")
abline(0,1,lty=2,col="gray")
cols <- c("red", "blue", "darkgreen", "orange", "purple", "brown")
i <- 1
for(mod in names(cal_data)){
  cal <- val.prob(cal_data[[mod]]$pred, cal_data[[mod]]$obs, m=10, pl=FALSE)
  lines(cal$mean.pred, cal$mean.obs, type='b', col=cols[i], lwd=2)
  i <- i + 1
}
legend("topleft", legend = names(cal_data), col = cols, lwd = 2)

dlist <- lapply(names(results), function(mod){
  pred <- results[[mod]]$pred
  dat_temp <- data.frame(Group = as.numeric(data_predictors_af_logistic_1$Group)-1, pred = pred)

  fit <- decision_curve(Group ~ pred,
                        data = dat_temp,
                        family = binomial(link='logit'),
                        thresholds = seq(0, 1, by = 0.01),
                        confidence.intervals = 0.95, study.design = "cohort")
  fit$label <- mod
  fit
})

plot_decision_curve(dlist, curve.names = names(results),
                    xlab = "Threshold Probability", ylab = "Net Benefit")

fit_dca <- decision_curve(Group ~ results$Logistic$pred, data = testData,
                          family = binomial, thresholds = seq(0,1,by=0.01), confidence.intervals = 0.95)
plot_clinical_impact(fit_dca, number.high.risk = 100)

performance_summary <- data.frame(
  Model = names(results),
  AUC = round(sapply(results, `[[`, "auc"), 3)
)
performance_summary
