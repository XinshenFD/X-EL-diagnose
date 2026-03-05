# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

getwd()
patient_ID <- read.xlsx("or_data/patient_ID.xlsx")
table(patient_ID$province)
patient_ID <- patient_ID[, c(1, 3)]
patient_ID
table(duplicated(patient_ID))
patient_ID <- patient_ID[!duplicated(patient_ID), ]
patient_ID
table(patient_ID$province)
inner_provience <- c("Zhejiang", "Fujian", "Shanghai", "Anhui", "Jiangsu", "Shandong")
table(patient_ID$province %in% inner_provience)
patient_ID$inner <- patient_ID$province
patient_ID$inner <- ifelse(patient_ID$province %in% inner_provience,
                           "TRUE",
                           "FALSE")
patient_ID_inner <- subset(patient_ID,
                           patient_ID$inner == "TRUE")
patient_ID_extra <- subset(patient_ID,
                           patient_ID$inner == "FALSE")
patient_ID_inner
patient_ID_extra
length(patient_ID_extra$name)
table(duplicated(patient_ID_extra$name))
