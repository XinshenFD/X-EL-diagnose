# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

colnames(inner_train_data) %in% colnames(RE_train_data)
colnames(inner_train_data) %in% colnames(CC_TC_train_data)

colna <- colnames(inner_train_data)

RE_train_data_select <- dplyr::select(RE_train_data,
                               name,          laterality,    gender,        Age,           pre_S,
                               pre_C,         pre_c_axial,   CDVA,          AL,            K1,
                               K1_a,          K2,            K2_a,          KSE,           K_c,
                               K_c_a,         ACD,           LT,            WTW,           CCT,
                               surgery_follow,LogMAR,        name_cate    )
CC_TC_train_data_select <- dplyr::select(CC_TC_train_data,
                                      name,          laterality,    gender,        Age,           pre_S,
                                      pre_C,         pre_c_axial,   CDVA,          AL,            K1,
                                      K1_a,          K2,            K2_a,          KSE,           K_c,
                                      K_c_a,         ACD,           LT,            WTW,           CCT,
                                      surgery_follow,LogMAR,        name_cate    )

length(extra_data$name)

table(duplicated(extra_data$name))
extra_name <- extra_data$name
extra_name <- as.data.frame(extra_name)
extra_name <- extra_name[!duplicated(extra_name$extra_name), ]
data_extra_toinner <- sample(extra_name, size = 60)

data_extra_toinner <- subset(extra_data,
                             extra_data$name %in% data_extra_toinner
)

data_extra_toinner_data <- dplyr::select(data_extra_toinner,
                                         name,          laterality,    gender,        Age,           pre_S,
                                         pre_C,         pre_c_axial,   CDVA,          AL,            K1,
                                         K1_a,          K2,            K2_a,          KSE,           K_c,
                                         K_c_a,         ACD,           LT,            WTW,           CCT,
                                         surgery_follow,LogMAR,        name_cate    )
data_extra_toinner_data
length(data_extra_toinner_data$name)

train_data <- rbind(data_extra_toinner_data,
                    CC_TC_train_data_select,
                    RE_train_data_select,
                    inner_train_data)

table(train_data$surgery_follow)

train_data$EL <- ifelse(train_data$surgery_follow %in% c(
                             "F1", "F2", "F3", "S") ,
                        "Y",
                        "N")

table(train_data$EL)
summary(train_data)
train_data$Age <- as.numeric(train_data$Age)
train_data$Age
table(is.na(train_data$Age))
which(is.na(train_data$Age) == TRUE)

extra_data$Age <- as.numeric(extra_data$Age)
which(is.na(extra_data$Age) == TRUE)
colnames(extra_data)

train_data
colnames(train_data)

train_data$WTW <- as.numeric(train_data$WTW)
train_data$LT <- as.numeric(train_data$LT)
train_data$ACD <- as.numeric(train_data$ACD)
table(is.na(train_data$AL))
train_data <- subset(train_data,
                     is.na(train_data$AL) == FALSE)

train_data$gender <- ifelse(
  train_data$gender == "Male",
  1,
  0
)
train_data <- subset(train_data,
                     is.na(train_data$pre_S) == FALSE)
train_data$pre_C

table(is.na(train_data$pre_C))
ifelse(is.na(train_data$pre_C) == TRUE,
       0,
       train_data$pre_C)

train_data$pre_C <- ifelse(is.na(train_data$pre_C) == TRUE,
                           0,
                           train_data$pre_C)

summary(train_data)
summary(train_data)
train_data$LogMAR <- as.numeric(train_data$LogMAR)
train_data$K_c <- train_data$K1 - train_data$K2
train_data$Group <- ifelse(train_data$EL == "Y",
                           1,
                           0)
(train_data)
predictors <- dplyr::select(train_data,
                            Age,
                            gender,
                            pre_S,
                            pre_C,
                            LogMAR,
                            AL,
                            K1,
                            K2,
                            ACD,
                            LT,
                            WTW,
                            Group)
summary(predictors)

table(is.na(predictors$LT))
table(is.na(predictors$ACD))

data_predictors <- knnImputation(as.data.frame(predictors), k = 5)

summary(data_predictors)
colnames(data_predictors)
features_keep <- c("Age", "gender", "pre_S", "pre_C",
                   "LogMAR", "AL", "K1", "K2",
                   "ACD", "LT", "WTW")
nzv_metrics <- nearZeroVar(data_predictors[, features_keep], saveMetrics = TRUE)

cor_matrix <- cor(data_predictors[, features_keep])

X <- as.matrix(data_predictors[, features_keep])

y <- data_predictors$Group

cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 10)
plot(cv_fit)
best_lambda <- cv_fit$lambda.min
cat("REDACTED", best_lambda, "\n")
model_logistic <- glm(Group ~ Age+gender+pre_S+pre_C+LogMAR+AL+K1+K2+ACD+LT+WTW,
    data = data_predictors,
    family = binomial(link = "logit"))

summary(model_logistic)

model_logistic <- glm(Group ~ Age+gender+pre_S+pre_C+LogMAR+AL+K1+K2+ACD+LT+WTW,
                      data = data_predictors,
                      family = binomial(link = "logit"))

summary(model_logistic)
model_logistic_AIC <- stepAIC(model_logistic)
summary(model_logistic_AIC)
