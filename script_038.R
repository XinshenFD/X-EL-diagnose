# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

extra_data
extra_data$name_cate
add_EL_F1 <- read.xlsx("or_data/add_EL/EL_F1.xlsx")
add_EL_F1$surgery_follow <- "F1"
add_EL_F2 <- read.xlsx("or_data/add_EL/EL_F2.xlsx")
add_EL_F2$surgery_follow <- "F2"
add_EL_F3 <- read.xlsx("or_data/add_EL/EL_F3.xlsx")
add_EL_F3$surgery_follow <- "F3"
add_EL_S <- read.xlsx("or_data/add_EL/EL_S.xlsx")
add_EL_S$surgery_follow <- "S"

add_EL <- rbind(add_EL_F1,
                add_EL_F2,
                add_EL_F3,
                add_EL_S)

table(add_EL$Laterality)
add_EL$Laterality <- ifelse(add_EL$Laterality == "REDACTED",
                            "Left",
                            "Right")

add_EL$name_cate <- paste0(add_EL$name,"_",add_EL$surgery_follow ,"_",add_EL$Laterality)

extra_data <- left_join(extra_data,
                        add_EL,
                         by = "name_cate")
table(extra_data$EL_severity)
table(add_EL$EL_severity)
train_data

length(train_data$name_cate)

train_data <- left_join(train_data,
                        add_EL,
                        by= "name_cate")
