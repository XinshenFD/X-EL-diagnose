# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

length(data_predictors_af_logistic_no_LT_train$pre_S)
length(validation_set$Age)

length(train_data$surgery_follow)
length(validation_copy$surgery_follow)

train_copy <- train_data[training,]
validation_copy <- train_data[-training,]

table(validation_copy$surgery_follow)
rownames(validation_copy) ==rownames(validation_set)

length(extra_data$surgery_follow)

length(data_extras$Age)

which(extra_data$name == "REDACTED" & extra_data$laterality == "Left")

colnames(extra_data)
extra_data$peidui

which(data_extras$peidui)

which(data_extras$peidui %in% c("21.9742.9343.039718866171",
                                "21.9942.9643.0448321523987",
                                "22.0542.9343.0247902382336"))

colnames(data_extras)
data_extras[c(503, 593, 646), 14]
data_extras[c(503, 593, 646),
            ]

extra_data

which(extra_data$name_cate %in% c("REDACTED",
                                  "REDACTED",
                                  "REDACTED",
                                  "REDACTED",
                                  "REDACTED",
                                  "REDACTED"))

extra_data[c(196, 214, 263, 386, 430, 452), ]

colnames(extra_data)

extra_data[196, c(13)] <- -2.5

extra_data[214,]

extra_data[214, c(12)] <- -14.75
extra_data[214, c(13)] <- -8.5
extra_data[214, c(14)] <- 160
extra_data[214, c(15)] <- 0.2
extra_data[214, c(38)] <- log10(1/0.2)

extra_data[263,]

extra_data[263, c(12)] <- -12
extra_data[263, c(13)] <- -1.5
extra_data[263, c(14)] <- 180
extra_data[263, c(15)] <- 0.15
extra_data[263, c(38)] <- log10(1/0.15)

extra_data[386,]
extra_data[386, c(13)] <- -1.5

extra_data[430,]

extra_data[430,13] <- -5

extra_data[452,]

extra_data[452,13] <- -5

colnames(extra_data)
aaaa <- extra_data[c(196, 214, 263, 386, 430, 452),40 ]

aaaa <- as.factor(aaaa)
which(data_extras$peidui %in% aaaa)

data_extras[c(696 ,714 ,761 ,884 ,928, 950), ]
colnames(data_extras)

data_extras[696, c(4)] <- -2.5

data_extras[714, 3] <- -14.75

data_extras[714, 4] <- -8.5

data_extras[714, 5] <- log10(1/0.2)

data_extras[761, 3] <- -12

data_extras[761, 4] <- -1.5

data_extras[761, 5] <- log10(1/0.15)

data_extras[884, 4] <- -1.5

data_extras[928, 4] <- -5

data_extras[950, 4] <- -5

data_extras
