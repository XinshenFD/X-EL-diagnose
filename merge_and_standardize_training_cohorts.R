# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

colnames(RE_data)
RE_train_data <- dplyr::select(RE_data,
                               name,
                               laterality,
                               gender,
                               Age,
                               pre_S,
                               pre_C,
                               pre_c_axial,
                               CDVA,
                               AL,
                               K1,
                               K1_a,
                               K2,
                               K2_a, KSE, K_c, K_c_a, ACD, LT, WTW, CCT, surgery_follow)
summary(RE_train_data)
colnames(RE_train_data)
RE_train_data[312:314,]
which(is.na(RE_train_data$CDVA) == TRUE)

table(RE_train_data$laterality )
table(RE_train_data$gender)

RE_train_data$laterality <- ifelse(RE_train_data$laterality == "R",
                                   "Right",
                                   "Left")

RE_train_data$gender <- ifelse(RE_train_data$gender == "REDACTED",
                                   "Female",
                                   "Male")

RE_train_data$K2 >= RE_train_data$K1

RE_train_data$K_c <- RE_train_data$K1 - RE_train_data$K2

RE_train_data$KSE <- 2/(1/RE_train_data$K1 +1/RE_train_data$K2)

RE_train_data$K_c_a == RE_train_data$K1_a

inner_train_data <- dplyr::select(inner_data,
                               name,
                               laterality,
                               gender,
                               Age,
                               pre_S,
                               pre_C,
                               pre_c_axial,
                               CDVA,
                               AL,
                               K1,
                               K1_a,
                               K2,
                               K2_a, KSE, K_c, K_c_a, ACD, LT, WTW, CCT, surgery_follow)
table(inner_train_data$laterality)
table(is.na(inner_train_data$laterality))
inner_train_data$laterality <- ifelse(inner_train_data$laterality == "REDACTED",
                                      "Right",
                                      "Left")
table(inner_train_data$gender)
write.xlsx(inner_train_data, "or_data/inner_train_data_add_gender.xlsx")
inner_train_data <- read.xlsx("or_data/inner_train_data_add_gender.xlsx")

table(inner_train_data$gender)
which(inner_train_data$gender == "REDACTED")
which(inner_train_data$gender == "REDACTED")
which(inner_train_data$gender == "REDACTED")
which(inner_train_data$gender == "REDACTED")

inner_train_data$gender <- ifelse(inner_train_data$gender %in% c("REDACTED", "REDACTED"),
       "Female",
       "Male")

inner_train_data

table(is.na(inner_train_data$CCT))
table(is.na(inner_train_data$ACD))
table(is.na(inner_train_data$LT))

CC_TC_data$CCT

table(CC_TC_data$diagnose)
table(CC_TC_data$add)
table(CC_TC_data$laterality)

write.xlsx(CC_TC_data,
           "or_data/CC_TC_data_1010.xlsx")

CC_TC_data$surgery_follow

CC_TC_train_data <- read.xlsx( "or_data/CC_TC_data_1010.xlsx")

table(CC_TC_train_data$surgery_follow)

table(CC_TC_train_data$laterality)
CC_TC_train_data$laterality <- ifelse(CC_TC_train_data$laterality == "REDACTED",
                                      "Right",
                                      "Left")
table(CC_TC_train_data$gender)
CC_TC_train_data$gender <- ifelse(CC_TC_train_data$gender == "REDACTED",
                                  "Female",
                                  "Male")

CC_TC_train_data$CDVA <- as.numeric(CC_TC_train_data$CDVA)
CC_TC_train_data$LogMAR <- log10(1/CC_TC_train_data$CDVA)

RE_train_data$CDVA
RE_train_data$LogMAR <- log10(1/RE_train_data$CDVA)

inner_train_data$CDVA
inner_train_data$LogMAR <- log10(1/inner_train_data$CDVA)

inner_train_data$name_cate <- paste0(inner_train_data$name, "_",
                                     inner_train_data$surgery_follow, "_",
                                     inner_train_data$laterality)

RE_train_data$name_cate <- paste0(RE_train_data$name, "_",
                                     RE_train_data$surgery_follow, "_",
                                     RE_train_data$laterality)

CC_TC_train_data$name_cate <- paste0(CC_TC_train_data$name, "_",

                                  CC_TC_train_data$laterality)

write.xlsx(inner_train_data,"or_data/train_data_beifeng/inner_train_data.xlsx")
write.xlsx(RE_train_data,"or_data/train_data_beifeng/RE_train_data.xlsx")
write.xlsx(CC_TC_train_data,"or_data/train_data_beifeng/CC_TC_train_data.xlsx")

MF_S_va <- read.xlsx("or_data/adjust_VA/MF_S_va.xlsx")
MF_F1_va <- read.xlsx("or_data/adjust_VA/MF_F1_va.xlsx")
MF_F2_va <- read.xlsx("or_data/adjust_VA/MF_F2_va.xlsx")
MF_F3_va <- read.xlsx("or_data/adjust_VA/MF_F3_va.xlsx")
CC_TC_va<- read.xlsx("or_data/adjust_VA/CC_TC_va.xlsx")

CC_TC_va$name_cate <- paste0(CC_TC_va$name,"_", CC_TC_va$laterality)

table(CC_TC_va$cate)
is.na(CC_TC_va$cate)

CC_TC_va <- subset(CC_TC_va,
                   is.na(CC_TC_va$cate) == FALSE)
CC_TC_va

CC_TC_train_data$name_cate %in% CC_TC_va$name_cate
colnames(CC_TC_train_data)

CC_TC_train_data <- read.xlsx("or_data/train_data_beifeng/CC_TC_train_data.xlsx")
table(CC_TC_train_data$LogMAR == "not")
colnames(CC_TC_train_data)
for (i in 1:500) {
  cc_id <- CC_TC_train_data[i, 34]
  dd <- NA
  dd <- subset(CC_TC_va,
               CC_TC_va$name_cate == cc_id)
  CC_TC_train_data[i, 33] <- ifelse(length(rownames(dd)) >0,
     dd[1, 7],
     CC_TC_train_data[i, 33]
     )
}
table(CC_TC_train_data$LogMAR == "not")

MF_S_va$group <- "S"
MF_F1_va$group <- "F1"
MF_F2_va$group <- "F2"
MF_F3_va$group <- "F3"

inner_train_data$name_cate
MF_va <- rbind(
  MF_S_va,
  MF_F1_va,
  MF_F2_va,
  MF_F3_va
)
MF_va$name_cate <- paste0(MF_va$name, "_",
                          MF_va$group, "_", MF_va$laterality)

table(inner_train_data$name_cate %in%  MF_va$name_cate)
table(MF_va$cate)

table(inner_train_data$LogMAR == "not")
colnames(inner_train_data)
colnames(RE_train_data)
colnames(MF_va)
MF_va$cate <- ifelse(MF_va$cate == "S",
       MF_va$LogMAR,
       MF_va$cate)
for (i in 1:956) {
  cc_id <- inner_train_data[i, 23]
  dd <- NA
  dd <- subset(MF_va,
               MF_va$name_cate == cc_id)
  inner_train_data[i, 22] <- ifelse(length(rownames(dd)) >0,
                                    dd[1, 7],
                                    inner_train_data[i, 22]
  )
}

table(inner_train_data$LogMAR == "not")
summary(MF_va)
table(MF_va$LogMAR)
table(MF_va$cate)
colnames(MF_va)
inner_train_data$LogMAR

colnames(extra_data)
table(extra_data$laterality)
extra_data
extra_data$laterality <- ifelse(
  extra_data$laterality == "REDACTED",
  "Right",
  "Left"
)

table(extra_data$gender)
which(extra_data$gender == "REDACTED")
which(extra_data$gender == "REDACTED")

extra_data$gender <- ifelse(
  extra_data$gender %in% c("REDACTED", "REDACTED"),
  "Female",
  "Male"
  )

inner_train_data$name_cate
extra_data$name_cate <- paste0(extra_data$name, "_", extra_data$surgery_follow,
                               "_", extra_data$laterality)

extra_data$LogMAR <- log10(1/extra_data$CDVA)
write.xlsx(extra_data,
           "or_data/train_data_beifeng/extra_data.xlsx")
colnames(extra_data)
for (i in 1:514) {
  cc_id <- extra_data[i, 37]
  dd <- NA
  dd <- subset(MF_va,
               MF_va$name_cate == cc_id)
  extra_data[i, 38] <- ifelse(length(rownames(dd)) >0,
                                    dd[1, 7],
                              extra_data[i, 38]
  )
}

inner_train_data
CC_TC_train_data
extra_data
inner_train_data <- subset(inner_train_data,
                           inner_train_data$LogMAR != "not")
inner_train_data$pre_S
CC_TC_train_data$LogMAR == "not"
CC_TC_train_data <- subset(CC_TC_train_data,
                           CC_TC_train_data$LogMAR != "not")

extra_data <- subset(extra_data,
                           extra_data$LogMAR != "not")

table(inner_train_data$pre_S == 0 &inner_train_data$pre_C == 0)
table(extra_data$pre_S == 0 &extra_data$pre_C == 0)
table(CC_TC_train_data$pre_S == 0 &CC_TC_train_data$pre_C == 0)

table(duplicated(inner_train_data$name))
length(inner_train_data$name)
length(extra_data$name)
table(duplicated(extra_data$name))
table(inner_train_data$surgery_follow)
table(extra_data$surgery_follow)
length(RE_train_data$name)
RE_train_data
CC_TC_train_data
write.xlsx(inner_train_data,
           "or_data/good_data_after_clean_and_adjust/inner_train_data.xlsx")

write.xlsx(CC_TC_train_data,
           "or_data/good_data_after_clean_and_adjust/CC_TC_train_data.xlsx")

write.xlsx(extra_data,
           "or_data/good_data_after_clean_and_adjust/extra_data.xlsx")

write.xlsx(RE_train_data,
           "or_data/good_data_after_clean_and_adjust/RE_train_data.xlsx")

table(inner_train_data$pre_S == 0 &inner_train_data$pre_C == 0)
table(extra_data$pre_S == 0 &extra_data$pre_C == 0)
table(CC_TC_train_data$pre_S == 0 &CC_TC_train_data$pre_C == 0)

table(is.na(CC_TC_train_data$pre_S))
table(is.na(inner_train_data$pre_S))
inner_train_data <- subset(inner_train_data,
                           !(inner_train_data$pre_S == 0 &inner_train_data$pre_C == 0))

extra_data <- read.xlsx("or_data/good_data_after_clean_and_adjust/extra_data.xlsx")

extra_data <- subset(extra_data,
                           !(extra_data$pre_S == 0 &extra_data$pre_C == 0))

length(extra_data$name.x)
length(inner_train_data$name)
table(inner_train_data$name %in% extra_data$name.x)

table(CC_TC_train_data$surgery_follow)
