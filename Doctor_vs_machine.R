# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

table(extra_data$EL_severity)

sample_light <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%
  dplyr::sample_n(20)
sample_medium <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%dplyr::sample_n(20)
sample_heavy <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%dplyr::sample_n(10)
sample_spherical <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%dplyr::sample_n(10)

sampled_data <- dplyr::bind_rows(sample_light, sample_medium, sample_heavy, sample_spherical)

table(sampled_data$EL_severity)

sample_light <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%
  dplyr::sample_n(20)
sample_medium <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%dplyr::sample_n(20)
sample_heavy <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%
  dplyr::filter(is.na(AL) == "FALSE")%>%
  dplyr::filter(is.na(K1) == "FALSE")%>%
  dplyr::filter(is.na(K2) == "FALSE")%>%dplyr::sample_n(10)
sample_spherical <- extra_data %>% dplyr::filter(EL_severity == "REDACTED") %>% dplyr::sample_n(10)

sampled_data_2 <- dplyr::bind_rows(sample_light, sample_medium, sample_heavy, sample_spherical)

table(sampled_data_2$EL_severity)

table(extra_contral$LogMAR > 0)

sample_RE <- extra_contral %>%
  mutate(LogMAR_flag = LogMAR > 0) %>%
  group_split(LogMAR_flag) %>%
  map_dfr(~ dplyr::slice_sample(.x, n = 15))

table(sample_RE$LogMAR > 0)

sample_RE_2 <- extra_contral %>%
  mutate(LogMAR_flag = LogMAR > 0) %>%
  group_split(LogMAR_flag) %>%
  map_dfr(~ dplyr::slice_sample(.x, n = 15))

table(sample_RE_2$LogMAR > 0)

table(CC_TC_train_data_select$surgery_follow)

sample_TC_CC <- CC_TC_train_data_select %>%
  filter(surgery_follow %in% c("TC", "CC")) %>%
  filter(is.na(AL) == "FALSE") %>%
  filter(is.na(pre_S) == "FALSE") %>%

  slice_sample(n = 10) %>%
  ungroup()

table(sample_TC_CC$surgery_follow)

sample_TC_CC_2 <- CC_TC_train_data_select %>%
  filter(surgery_follow %in% c("TC", "CC")) %>%
  filter(is.na(AL) == "FALSE") %>%
  filter(is.na(pre_S) == "FALSE") %>%

  slice_sample(n = 10) %>%
  ungroup()

table(sample_TC_CC_2$surgery_follow)

colnames(sampled_data)

sampled_data_daochu <- dplyr::select(sampled_data,
                              name_cate,
                              EL_severity,
                              EL_angle,
                              AL,
                              K1,
                              K2, pre_S, pre_C, LogMAR)

sampled_data_daochu_2 <- dplyr::select(sampled_data_2,
                                     name_cate,
                                     EL_severity,
                                     EL_angle,
                                     AL,
                                     K1,
                                     K2, pre_S, pre_C, LogMAR)

sample_RE$name_cate <- sample_RE$Group
sample_RE$EL_severity <- 0
sample_RE$EL_angle <- 0

sample_RE_daochu <- dplyr::select(sample_RE,
                                  name_cate,
                                  EL_severity,
                                  EL_angle,
                                  AL,
                                  K1,
                                  K2, pre_S, pre_C, LogMAR)

sample_RE_2$name_cate <- sample_RE_2$Group
sample_RE_2$EL_severity <- 0
sample_RE_2$EL_angle <- 0

sample_RE_daochu_2 <- dplyr::select(sample_RE_2,
                                  name_cate,
                                  EL_severity,
                                  EL_angle,
                                  AL,
                                  K1,
                                  K2, pre_S, pre_C, LogMAR)

sample_TC_CC$name_cate <- paste0(sample_TC_CC$name_cate, "_", sample_TC_CC$surgery_follow)
sample_TC_CC$EL_severity <- 0
sample_TC_CC$EL_angle <- 0
sample_TC_CC_daochu <- dplyr::select(sample_TC_CC,
                                    name_cate,
                                    EL_severity,
                                    EL_angle,
                                    AL,
                                    K1,
                                    K2, pre_S, pre_C, LogMAR)

sample_TC_CC_2$name_cate <- paste0(sample_TC_CC_2$name_cate, "_", sample_TC_CC_2$surgery_follow)
sample_TC_CC_2$EL_severity <- 0
sample_TC_CC_2$EL_angle <- 0
sample_TC_CC_daochu_2 <- dplyr::select(sample_TC_CC_2,
                                     name_cate,
                                     EL_severity,
                                     EL_angle,
                                     AL,
                                     K1,
                                     K2, pre_S, pre_C, LogMAR)

sample1 <- rbind(sampled_data_daochu,
                 sample_RE_daochu,
                 sample_TC_CC_daochu)

sample2 <- rbind(sampled_data_daochu_2,
                 sample_RE_daochu_2,
                 sample_TC_CC_daochu_2)

sample1$LogMAR <- as.numeric(sample1$LogMAR)
summary(sample1)
sample1$pre_C <- ifelse(is.na(sample1$pre_C) == "TRUE",
                        0,
                        sample1$pre_C)
sample1$predicted_label <- predict(model_no_ACD_LT_xgb, sample1,type="prob")

sample1$predicted_label <- ifelse(sample1$predicted_label$Yes > xgb_no_ACD_LT_best$threshold, "Yes", "No")

table(sample1$predicted_label)

sample2$LogMAR <- as.numeric(sample2$LogMAR)
summary(sample2)
sample2$pre_C <- ifelse(is.na(sample2$pre_C) == "TRUE",
                        0,
                        sample2$pre_C)

aa <- predict(model_no_ACD_LT_xgb$finalModel, as.matrix(sample2[, model_features, drop = FALSE]),type="prob")
aa$Yes
sample2$predicted_label <- predict(model_no_ACD_LT_xgb, sample2,type="prob")
sample2$predicted_label <- ifelse(sample2$predicted_label$Yes > xgb_no_ACD_LT_best$threshold, "Yes", "No")

sample1$VA <- 1/10^(sample1$LogMAR)
sample2$VA <- 1/10^(sample2$LogMAR)
sample1$VA <- round(sample1$VA,2)
sample2$VA <- round(sample2$VA,2)

sample1 <- sample1 %>%
  slice_sample(prop = 1)

sample2 <- sample2 %>%
  slice_sample(prop = 1)

write.xlsx(sample1, "doctor_VS_AI/sample1.xlsx")
write.xlsx(sample2, "doctor_VS_AI/sample2.xlsx")
