# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

MF_surgery_data_age15_1010
MF_follow_1_data_age15_1010
MF_follow_2_data_age15_1010
MF_follow_3_data_age15_1010

table(patient_ID_inner$province)
table(patient_ID_extra$province)
inner_MF_F1 <- subset(MF_follow_1_data_age15_1010,
                      MF_follow_1_data_age15_1010$name
                      %in% patient_ID_inner$name)

extra_MF_F1 <- subset(MF_follow_1_data_age15_1010,
       !MF_follow_1_data_age15_1010$name
       %in% patient_ID_inner$name)

inner_MF_F2 <- subset(MF_follow_2_data_age15_1010,
                      MF_follow_2_data_age15_1010$name
                      %in% patient_ID_inner$name)

extra_MF_F2 <- subset(MF_follow_2_data_age15_1010,
                      !MF_follow_2_data_age15_1010$name
                      %in% patient_ID_inner$name)

inner_MF_F3 <- subset(MF_follow_3_data_age15_1010,
                      MF_follow_3_data_age15_1010$name
                      %in% patient_ID_inner$name)

extra_MF_F3 <- subset(MF_follow_3_data_age15_1010,
                      !MF_follow_3_data_age15_1010$name
                      %in% patient_ID_inner$name)

inner_MF_S <- subset(MF_surgery_data_age15_1010,
                      MF_surgery_data_age15_1010$name
                      %in% patient_ID_inner$name)

extra_MF_S <- subset(MF_surgery_data_age15_1010,
                      !MF_surgery_data_age15_1010$name
                      %in% patient_ID_inner$name)

inner_data <- rbind(inner_MF_F1,
                    inner_MF_F2,
                    inner_MF_F3,
                    inner_MF_S)
extra_data <- rbind(extra_MF_F1,
                    extra_MF_F2,
                    extra_MF_F3,
                    extra_MF_S)
