# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

colnames(MF_follow_1_data_age15)
MF_follow_1_data_age15$AL <- as.numeric(MF_follow_1_data_age15$AL)
summary(MF_follow_1_data_age15$AL)
MF_follow_1_data_age15 <- subset(MF_follow_1_data_age15,
                                 is.na(MF_follow_1_data_age15$AL) == FALSE)
MF_follow_1_data_age15$CDVA <- as.numeric(MF_follow_1_data_age15$CDVA)
summary(MF_follow_1_data_age15$CDVA)

MF_follow_1_data_age15$pre_S <- as.numeric(MF_follow_1_data_age15$pre_S)
MF_follow_1_data_age15$pre_C <- as.numeric(MF_follow_1_data_age15$pre_C)
MF_follow_1_data_age15$pre_c_axial <- as.numeric(MF_follow_1_data_age15$pre_c_axial)

colnames(MF_follow_1_data_age15)
is.na(MF_follow_1_data_age15[, 12:15])
MF_follow_1_data_age15_1010 <- subset(MF_follow_1_data_age15,
                                is.na(MF_follow_1_data_age15$pre_S) == FALSE |
                                  is.na(MF_follow_1_data_age15$CDVA) == FALSE)
MF_follow_1_data_age15_1010$CDVA <- ifelse(is.na(MF_follow_1_data_age15_1010$CDVA) == TRUE,
                                           0.01,
                                           MF_follow_1_data_age15_1010$CDVA)

MF_follow_1_data_age15_1010$pre_S <- ifelse(is.na(MF_follow_1_data_age15_1010$pre_S) == TRUE,
                                           0,
                                           MF_follow_1_data_age15_1010$pre_S)

MF_follow_1_data_age15_1010$pre_C <- ifelse(is.na(MF_follow_1_data_age15_1010$pre_C) == TRUE,
                                            0,
                                            MF_follow_1_data_age15_1010$pre_C)
MF_follow_1_data_age15_1010$pre_c_axial <- ifelse(is.na(MF_follow_1_data_age15_1010$pre_c_axial) == TRUE,
                                            0,
                                            MF_follow_1_data_age15_1010$pre_c_axial)

MF_follow_1_data_age15_1010$K1 <- as.numeric(MF_follow_1_data_age15_1010$K1)
MF_follow_1_data_age15_1010$K1_a <- as.numeric(MF_follow_1_data_age15_1010$K1_a)
MF_follow_1_data_age15_1010$K2 <- as.numeric(MF_follow_1_data_age15_1010$K2)
MF_follow_1_data_age15_1010$K2_a <- as.numeric(MF_follow_1_data_age15_1010$K2_a)
MF_follow_1_data_age15_1010$K2 >= MF_follow_1_data_age15_1010$K1
MF_follow_1_data_age15_1010$K_c <- MF_follow_1_data_age15_1010$K1 - MF_follow_1_data_age15_1010$K2
MF_follow_1_data_age15_1010$K_c_a <- MF_follow_1_data_age15_1010$K1_a

MF_follow_1_data_age15_1010$KSE <- 2/(1/MF_follow_1_data_age15_1010$K1 +1/MF_follow_1_data_age15_1010$K2)

MF_follow_1_data_age15_1010$preIOP <- as.numeric(MF_follow_1_data_age15_1010$preIOP)
MF_follow_1_data_age15_1010$ACD <- as.numeric(MF_follow_1_data_age15_1010$ACD)
MF_follow_1_data_age15_1010$LT <-  as.numeric(MF_follow_1_data_age15_1010$LT)
MF_follow_1_data_age15_1010$CCT <-  as.numeric(MF_follow_1_data_age15_1010$CCT)

colnames(MF_follow_2_data_age15)
MF_follow_2_data_age15$AL <- as.numeric(MF_follow_2_data_age15$AL)
summary(MF_follow_2_data_age15$AL)
MF_follow_2_data_age15 <- subset(MF_follow_2_data_age15,
                                 is.na(MF_follow_2_data_age15$AL) == FALSE)
MF_follow_2_data_age15$CDVA <- as.numeric(MF_follow_2_data_age15$CDVA)
summary(MF_follow_2_data_age15$CDVA)

MF_follow_2_data_age15$pre_S <- as.numeric(MF_follow_2_data_age15$pre_S)
MF_follow_2_data_age15$pre_C <- as.numeric(MF_follow_2_data_age15$pre_C)
MF_follow_2_data_age15$pre_c_axial <- as.numeric(MF_follow_2_data_age15$pre_c_axial)

colnames(MF_follow_2_data_age15)
is.na(MF_follow_2_data_age15[, 12:15])
MF_follow_2_data_age15_1010 <- subset(MF_follow_2_data_age15,
                                      is.na(MF_follow_2_data_age15$pre_S) == FALSE |
                                        is.na(MF_follow_2_data_age15$CDVA) == FALSE)
MF_follow_2_data_age15_1010$CDVA <- ifelse(is.na(MF_follow_2_data_age15_1010$CDVA) == TRUE,
                                           0.01,
                                           MF_follow_2_data_age15_1010$CDVA)

MF_follow_2_data_age15_1010$pre_S <- ifelse(is.na(MF_follow_2_data_age15_1010$pre_S) == TRUE,
                                            0,
                                            MF_follow_2_data_age15_1010$pre_S)

MF_follow_2_data_age15_1010$pre_C <- ifelse(is.na(MF_follow_2_data_age15_1010$pre_C) == TRUE,
                                            0,
                                            MF_follow_2_data_age15_1010$pre_C)
MF_follow_2_data_age15_1010$pre_c_axial <- ifelse(is.na(MF_follow_2_data_age15_1010$pre_c_axial) == TRUE,
                                                  0,
                                                  MF_follow_2_data_age15_1010$pre_c_axial)

MF_follow_2_data_age15_1010$K1 <- as.numeric(MF_follow_2_data_age15_1010$K1)
MF_follow_2_data_age15_1010$K1_a <- as.numeric(MF_follow_2_data_age15_1010$K1_a)
MF_follow_2_data_age15_1010$K2 <- as.numeric(MF_follow_2_data_age15_1010$K2)
MF_follow_2_data_age15_1010$K2_a <- as.numeric(MF_follow_2_data_age15_1010$K2_a)
MF_follow_2_data_age15_1010$K2 >= MF_follow_2_data_age15_1010$K1
MF_follow_2_data_age15_1010$K_c <- MF_follow_2_data_age15_1010$K1 - MF_follow_2_data_age15_1010$K2
MF_follow_2_data_age15_1010$K_c_a <- MF_follow_2_data_age15_1010$K1_a

MF_follow_2_data_age15_1010$KSE <- 2/(1/MF_follow_2_data_age15_1010$K1 +1/MF_follow_2_data_age15_1010$K2)

MF_follow_2_data_age15_1010$preIOP <- as.numeric(MF_follow_2_data_age15_1010$preIOP)
MF_follow_2_data_age15_1010$ACD <- as.numeric(MF_follow_2_data_age15_1010$ACD)
MF_follow_2_data_age15_1010$LT <-  as.numeric(MF_follow_2_data_age15_1010$LT)
MF_follow_2_data_age15_1010$CCT <-  as.numeric(MF_follow_2_data_age15_1010$CCT)

colnames(MF_follow_3_data_age15)
MF_follow_3_data_age15$AL <- as.numeric(MF_follow_3_data_age15$AL)
summary(MF_follow_3_data_age15$AL)
MF_follow_3_data_age15 <- subset(MF_follow_3_data_age15,
                                 is.na(MF_follow_3_data_age15$AL) == FALSE)
MF_follow_3_data_age15$CDVA <- as.numeric(MF_follow_3_data_age15$CDVA)
summary(MF_follow_3_data_age15$CDVA)

MF_follow_3_data_age15$pre_S <- as.numeric(MF_follow_3_data_age15$pre_S)
MF_follow_3_data_age15$pre_C <- as.numeric(MF_follow_3_data_age15$pre_C)
MF_follow_3_data_age15$pre_c_axial <- as.numeric(MF_follow_3_data_age15$pre_c_axial)

colnames(MF_follow_3_data_age15)
is.na(MF_follow_3_data_age15[, 12:15])
MF_follow_3_data_age15_1010 <- subset(MF_follow_3_data_age15,
                                      is.na(MF_follow_3_data_age15$pre_S) == FALSE |
                                        is.na(MF_follow_3_data_age15$CDVA) == FALSE)
MF_follow_3_data_age15_1010$CDVA <- ifelse(is.na(MF_follow_3_data_age15_1010$CDVA) == TRUE,
                                           0.01,
                                           MF_follow_3_data_age15_1010$CDVA)

MF_follow_3_data_age15_1010$pre_S <- ifelse(is.na(MF_follow_3_data_age15_1010$pre_S) == TRUE,
                                            0,
                                            MF_follow_3_data_age15_1010$pre_S)

MF_follow_3_data_age15_1010$pre_C <- ifelse(is.na(MF_follow_3_data_age15_1010$pre_C) == TRUE,
                                            0,
                                            MF_follow_3_data_age15_1010$pre_C)
MF_follow_3_data_age15_1010$pre_c_axial <- ifelse(is.na(MF_follow_3_data_age15_1010$pre_c_axial) == TRUE,
                                                  0,
                                                  MF_follow_3_data_age15_1010$pre_c_axial)

MF_follow_3_data_age15_1010$K1 <- as.numeric(MF_follow_3_data_age15_1010$K1)
MF_follow_3_data_age15_1010$K1_a <- as.numeric(MF_follow_3_data_age15_1010$K1_a)
MF_follow_3_data_age15_1010$K2 <- as.numeric(MF_follow_3_data_age15_1010$K2)
MF_follow_3_data_age15_1010$K2_a <- as.numeric(MF_follow_3_data_age15_1010$K2_a)
MF_follow_3_data_age15_1010$K2 >= MF_follow_3_data_age15_1010$K1
MF_follow_3_data_age15_1010$K_c <- MF_follow_3_data_age15_1010$K1 - MF_follow_3_data_age15_1010$K2
MF_follow_3_data_age15_1010$K_c_a <- MF_follow_3_data_age15_1010$K1_a

MF_follow_3_data_age15_1010$KSE <- 2/(1/MF_follow_3_data_age15_1010$K1 +1/MF_follow_3_data_age15_1010$K2)

MF_follow_3_data_age15_1010$preIOP <- as.numeric(MF_follow_3_data_age15_1010$preIOP)
MF_follow_3_data_age15_1010$ACD <- as.numeric(MF_follow_3_data_age15_1010$ACD)
MF_follow_3_data_age15_1010$LT <-  as.numeric(MF_follow_3_data_age15_1010$LT)
MF_follow_3_data_age15_1010$CCT <-  as.numeric(MF_follow_3_data_age15_1010$CCT)

colnames(MF_surgery_data_age15)
MF_surgery_data_age15$AL <- as.numeric(MF_surgery_data_age15$AL)
summary(MF_surgery_data_age15$AL)
MF_surgery_data_age15 <- subset(MF_surgery_data_age15,
                                 is.na(MF_surgery_data_age15$AL) == FALSE)
MF_surgery_data_age15$CDVA <- as.numeric(MF_surgery_data_age15$CDVA)
summary(MF_surgery_data_age15$CDVA)

MF_surgery_data_age15$pre_S <- as.numeric(MF_surgery_data_age15$pre_S)
MF_surgery_data_age15$pre_C <- as.numeric(MF_surgery_data_age15$pre_C)
MF_surgery_data_age15$pre_c_axial <- as.numeric(MF_surgery_data_age15$pre_c_axial)

colnames(MF_surgery_data_age15)
is.na(MF_surgery_data_age15[, 12:15])
MF_surgery_data_age15_1010 <- subset(MF_surgery_data_age15,
                                      is.na(MF_surgery_data_age15$pre_S) == FALSE |
                                        is.na(MF_surgery_data_age15$CDVA) == FALSE)
MF_surgery_data_age15_1010$CDVA <- ifelse(is.na(MF_surgery_data_age15_1010$CDVA) == TRUE,
                                           0.01,
                                           MF_surgery_data_age15_1010$CDVA)

MF_surgery_data_age15_1010$pre_S <- ifelse(is.na(MF_surgery_data_age15_1010$pre_S) == TRUE,
                                            0,
                                            MF_surgery_data_age15_1010$pre_S)

MF_surgery_data_age15_1010$pre_C <- ifelse(is.na(MF_surgery_data_age15_1010$pre_C) == TRUE,
                                            0,
                                            MF_surgery_data_age15_1010$pre_C)
MF_surgery_data_age15_1010$pre_c_axial <- ifelse(is.na(MF_surgery_data_age15_1010$pre_c_axial) == TRUE,
                                                  0,
                                                  MF_surgery_data_age15_1010$pre_c_axial)

MF_surgery_data_age15_1010$K1 <- as.numeric(MF_surgery_data_age15_1010$K1)
MF_surgery_data_age15_1010$K1_a <- as.numeric(MF_surgery_data_age15_1010$K1_a)
MF_surgery_data_age15_1010$K2 <- as.numeric(MF_surgery_data_age15_1010$K2)
MF_surgery_data_age15_1010$K2_a <- as.numeric(MF_surgery_data_age15_1010$K2_a)
MF_surgery_data_age15_1010$K2 >= MF_surgery_data_age15_1010$K1
MF_surgery_data_age15_1010$K_c <- MF_surgery_data_age15_1010$K1 - MF_surgery_data_age15_1010$K2
MF_surgery_data_age15_1010$K_c_a <- MF_surgery_data_age15_1010$K1_a

MF_surgery_data_age15_1010$KSE <- 2/(1/MF_surgery_data_age15_1010$K1 +1/MF_surgery_data_age15_1010$K2)

MF_surgery_data_age15_1010$preIOP <- as.numeric(MF_surgery_data_age15_1010$preIOP)
MF_surgery_data_age15_1010$ACD <- as.numeric(MF_surgery_data_age15_1010$ACD)
MF_surgery_data_age15_1010$LT <-  as.numeric(MF_surgery_data_age15_1010$LT)
MF_surgery_data_age15_1010$CCT <-  as.numeric(MF_surgery_data_age15_1010$CCT)
