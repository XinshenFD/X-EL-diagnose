# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

RE_data <- read.xlsx("or_data/RE.xlsx")
CC_TC_data <- read.xlsx("or_data/CC_TC.xlsx")
summary(RE_data)
colnames(RE_data) == colnames(CC_TC_data)
colnames(RE_data)[8] <- colnames(CC_TC_data)[8]

summary(RE_data)
summary(CC_TC_data)
summary(CC_TC_data$exam_date)

RE_data$Age <- (RE_data$exam_date - RE_data$birthday)/365.25
summary(RE_data$Age)

CC_TC_data$Age <- (CC_TC_data$exam_date - CC_TC_data$birthday)/365.25

CC_TC_data <- CC_TC_data[1:500, ]
summary(CC_TC_data$Age)
subset(CC_TC_data,
       CC_TC_data$Age <1)

subset(CC_TC_data,
       CC_TC_data$Age <1)
which(CC_TC_data$name == "REDACTED", arr.ind = TRUE)
colnames(CC_TC_data)
CC_TC_data[435:436, 31] <- c(9.570157, 9.570157)

CC_TC_data

MF_surgery_data <- read.xlsx("or_data/MF_surgery.xlsx")
length(MF_surgery_data$name)
tail(MF_surgery_data$name)
MF_surgery_data$surgery_time
MF_surgery_data$exam_date
MF_surgery_data$birthday
class(MF_surgery_data$exam_date)
class(MF_surgery_data$birthday)
MF_surgery_data$Age <- as.numeric(MF_surgery_data$exam_date) - as.numeric(MF_surgery_data$birthday)

MF_surgery_data$Age <- MF_surgery_data$Age/365.25
table(is.na(MF_surgery_data$Age))

MF_surgery_data_agena <- subset(
  MF_surgery_data,
  is.na(MF_surgery_data$Age) == TRUE
)
MF_surgery_data_age15 <- subset(
  MF_surgery_data,
  MF_surgery_data$Age < 16
)

table(MF_surgery_data_agena$name %in% MF_surgery_data_age15$name)
MF_surgery_data_agenotna <- subset(MF_surgery_data_agena,
                                  (MF_surgery_data_agena$name %in% MF_surgery_data_age15$name) == TRUE)
MF_surgery_data_agenotna

which(MF_surgery_data_age15$name %in% MF_surgery_data_agenotna$name == TRUE,
      arr.ind = TRUE)

cc <- MF_surgery_data_age15[c(162,205, 213, 219, 225,
                              260, 267, 359, 504, 634,
                              715, 809, 976), 35]
cc <- as.data.frame(cc)
MF_surgery_data_agenotna$Age <- t(cc)

MF_surgery_data_age15 <- rbind(MF_surgery_data_age15,
                               MF_surgery_data_agenotna)

tail(MF_surgery_data_age15)

MF_surgery_data_age15

MF_follow_1_data <- read.xlsx("or_data/MF_follow_1.xlsx")
MF_follow_1_data
table(MF_follow_1_data$name)
table(is.na(MF_follow_1_data$name))
tail(MF_follow_1_data$name)
MF_follow_1_data <- MF_follow_1_data[1:664, ]
MF_follow_1_data
tail(MF_follow_1_data)

MF_follow_1_data$Age <- MF_follow_1_data$exam_date - as.numeric(MF_follow_1_data$birthday)
MF_follow_1_data$Age <- MF_follow_1_data$Age/365.25

MF_follow_1_data_agena <- subset(MF_follow_1_data,
                                 is.na(MF_follow_1_data$Age) == TRUE)

MF_follow_1_data_age15 <- subset(MF_follow_1_data,
                                 MF_follow_1_data$Age < 16)

table(MF_follow_1_data_agena$name %in% MF_follow_1_data_age15$name)

which(MF_follow_1_data_agena$name %in% MF_follow_1_data_age15$name == TRUE)
MF_follow_1_data_agena[c(14,17), ]
which(MF_follow_1_data_age15$name %in% c("REDACTED", "REDACTED"), arr.ind = TRUE)
MF_follow_1_data_age15[c(277, 278), 35]

MF_follow_1_data_agenotna <- MF_follow_1_data_agena[c(14,17), ]
MF_follow_1_data_age15[c(277, 278),]
MF_follow_1_data_agenotna$Age <- MF_follow_1_data_age15[c(277, 278),35]

MF_follow_1_data_age15 <- rbind(MF_follow_1_data_age15,
                                MF_follow_1_data_agenotna)

MF_follow_1_data_age15

MF_follow_2_data <- read.xlsx("or_data/MF_follow_2.xlsx")
MF_follow_2_data
table(MF_follow_2_data$name)
table(is.na(MF_follow_2_data$name))
tail(MF_follow_2_data$name)

MF_follow_2_data
tail(MF_follow_2_data)

MF_follow_2_data$Age <- MF_follow_2_data$exam_date - as.numeric(MF_follow_2_data$birthday)
MF_follow_2_data$Age <- MF_follow_2_data$Age/365.25

MF_follow_2_data_agena <- subset(MF_follow_2_data,
                                 is.na(MF_follow_2_data$Age) == TRUE)

MF_follow_2_data_age15 <- subset(MF_follow_2_data,
                                 MF_follow_2_data$Age < 16)

table(MF_follow_2_data_agena$name %in% MF_follow_2_data_age15$name)

which(MF_follow_2_data_agena$name %in% MF_follow_2_data_age15$name == TRUE)
MF_follow_2_data_agena[c(3,6), ]
which(MF_follow_2_data_age15$name %in% c("REDACTED", "REDACTED"), arr.ind = TRUE)
MF_follow_2_data_age15[c(169,170), 35]

MF_follow_2_data_agenotna <- MF_follow_2_data_agena[c(3, 6), ]
MF_follow_2_data_age15[c(169,170),]
MF_follow_2_data_agenotna$Age <- MF_follow_2_data_age15[c(169,170),35]

MF_follow_2_data_age15 <- rbind(MF_follow_2_data_age15,
                                MF_follow_2_data_agenotna)

MF_follow_2_data_age15

MF_follow_3_data <- read.xlsx("or_data/MF_follow_3.xlsx")
MF_follow_3_data
table(MF_follow_3_data$name)
table(is.na(MF_follow_3_data$name))
tail(MF_follow_3_data$name)

MF_follow_3_data
tail(MF_follow_3_data)

MF_follow_3_data$Age <- MF_follow_3_data$exam_date - as.numeric(MF_follow_3_data$birthday)
MF_follow_3_data$Age <- MF_follow_3_data$Age/365.25

MF_follow_3_data_agena <- subset(MF_follow_3_data,
                                 is.na(MF_follow_3_data$Age) == TRUE)

MF_follow_3_data_age15 <- subset(MF_follow_3_data,
                                 MF_follow_3_data$Age < 16)

table(MF_follow_3_data_agena$name %in% MF_follow_3_data_age15$name)

MF_follow_3_data_age15

MF_follow_3_data_age15$surgery_follow <- "F3"
MF_follow_2_data_age15$surgery_follow <- "F2"
MF_follow_1_data_age15$surgery_follow <- "F1"
MF_surgery_data_age15$surgery_follow <- "S"

CC_TC_data$surgery_follow <- "CC_TC"
RE_data$surgery_follow <- "RE"
