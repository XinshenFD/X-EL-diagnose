# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

extra_validations
colnames(extra_validations)

cc_new_add_1201 <- read.xlsx("or_data/cc_new_add.xlsx")
colnames(cc_new_add_1201)
summary(cc_new_add_1201)
cc_new_add_1201$Age <- (cc_new_add_1201$exam_date - cc_new_add_1201$birthday)/365.25

cc_new_add_1201$Age

cc_new_add_1201$pre_V <- as.numeric(cc_new_add_1201$pre_V)
cc_new_add_1201$pre_S <- as.numeric(cc_new_add_1201$pre_S)
cc_new_add_1201$LogMAR <- log10(1/cc_new_add_1201$pre_V)
write.xlsx(cc_new_add_1201, "or_data/cc_new_add_1202.xlsx")

cc_new_add_1202 <- read.xlsx("or_data/cc_new_add_1202.xlsx")
summary(cc_new_add_1202)
cc_new_add_1202 <- subset(cc_new_add_1202,
                          is.na(cc_new_add_1202$LogMAR) == FALSE)

cc_new_add_1202$pre_S <- ifelse(is.na(cc_new_add_1202$pre_S) == FALSE,
                                cc_new_add_1202$pre_S,
                                0)
cc_new_add_1202$pre_C <- ifelse(is.na(cc_new_add_1202$pre_C) == FALSE,
                                cc_new_add_1202$pre_C,
                                0)

colnames(extra_validations)
colnames(cc_new_add_1202)
extra_validations_newcc_part <- dplyr::select(cc_new_add_1202,
                                              Age, gender, pre_S, pre_C, LogMAR, AL,
                                              K1, K2, KE, K_c, ACD, LT, WTW)

table(extra_validations_newcc_part$gender)

cc_new_add_1202$cate <- ifelse(cc_new_add_1202$pre_lens == "REDACTED",
                               "RE",
                               "CC")
cc_new_add_1202$Laterality <- ifelse(cc_new_add_1202$Laterality == "REDACTED",
                                            "Right",
                                            "Left")

cc_new_add_1202$name_cate <- paste0(cc_new_add_1202$name, "_",cc_new_add_1202$cate, "_",
                                    cc_new_add_1202$Laterality)

extra_validations_newcc_part$gender

summary(extra_validations_newcc_part)
cc_new_add_1202 <- subset(cc_new_add_1202,
                                       is.na(cc_new_add_1202$AL) == FALSE)
extra_validations_newcc_part <- subset(extra_validations_newcc_part,
                                       is.na(extra_validations_newcc_part$AL) == FALSE)
table(cc_new_add_1202$AL == extra_validations_newcc_part$AL)
summary(extra_validations_newcc_part)
colnames(extra_validations_newcc_part)[9] <- "KSE"
colnames(extra_validations_newcc_part)

median(extra_validations_newcc_part$ACD, na.rm = T)
extra_validations_newcc_part$ACD <- ifelse(is.na(extra_validations_newcc_part$ACD) == T,
                                           3.47,
                                           extra_validations_newcc_part$ACD)

median(extra_validations_newcc_part$LT, na.rm = T)
extra_validations_newcc_part$LT <- ifelse(is.na(extra_validations_newcc_part$LT) == T,
                                          3.535,
                                           extra_validations_newcc_part$LT)

extra_validations_newcc_part$LT

predict(model_no_ACD_LT_xgb, extra_validations_newcc_part)
table(predict(model_no_ACD_LT_xgb, extra_validations_newcc_part))

xgb_probs_no_ACD_LT_newcc_part <- predict(model_no_ACD_LT_xgb, extra_validations_newcc_part,type="prob")

table(ifelse(xgb_probs_no_ACD_LT_newcc_part$Yes > xgb_no_ACD_LT_best$threshold, "Yes", "No"))
table(data_extras$Group)
extra_validations
colnames(data_extras)
data_extras$Group
colnames(extra_validations_newcc_part)
extra_validations_newcc_part$KSE <- 2/(1/extra_validations_newcc_part$K1 +1/extra_validations_newcc_part$K2)
extra_validations_newcc_part$K_c <- extra_validations_newcc_part$K1 - extra_validations_newcc_part$K2
extra_validations_newcc_part$Group <- "No"
extra_validations_newcc_part$peidui <- paste0(extra_validations_newcc_part$AL, extra_validations_newcc_part$K1,
                                              extra_validations_newcc_part$KSE)

summary(extra_validations_newcc_part)
extra_validations_newcc_part$label_predict <- "No"
data_extras_new1202 <- rbind(data_extras,
                             extra_validations_newcc_part)
data_extras_new1202

summary(data_extras_new1202)
table(data_extras_new1202$Group)
