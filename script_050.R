# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

1870 - (256+621)
993

212+380+135+74 +256+621
801
263+471+167+92+256+621
212/801 *993
380/801 *993
135/801 *993
74/801 *993
263+
471+
167+
92

104/212*263
129/263
108/212*263

193/380*471
239/471
187/380*471

64/135*167
79/167
71/135*167
88/167
39/74*92
48/92
35/74*92
44/92

88/212*263
109/263
124/212*263
154/263

153/380*471
190/471
281/471

66/135*167
82/167
69/135*167
85/167

fisher.test(rbind(
   c(129, 239, 79, 48),
   c(134, 232, 88, 44)))

fisher.test(rbind(
  c(109, 190, 82, 33),
  c(154, 281, 85, 41)))

122/256
134/256
314/621
307/621

112/256
144/256

294/621
327/621

104+254+83+20+70+532
263+471+167+92

70+532
104+254+83+20
54/104
50/104
70*0.86
249/252
98/104
522/532
77/254
177/254
60/70
32/83
51/83
854 - 575

6/20
14/20

29/70
41/70
281/532
251/532

654+655

654+285+655+276

129+239+79+48+122+314
134+232+88+44+134+307
655/1309
654/1309

276/561
285/561

564 +247
745+314

92*0.45
92*0.55

41/92
51/92

109+190+82+51+112+294
154+281+85+41+144+327

(109+190+82+51+112+294)*0.7
(109+190+82+51+112+294)*0.3

(154+281+85+41+144+327)*0.7
(154+281+85+41+144+327)*0.3
27

587/1309
722/1309

251/561
310/561

fisher.test(rbind(
  c(655, 276),
  c(654, 285)))

fisher.test(rbind(
  c(587, 251),
  c(722, 310)))
aa <- read.xlsx("doctor_VS_AI/yongxiang2.xlsx")

aa <- subset(aa,
             aa$ID)

256+621
70+532

R.version

fisher.test(rbind(
  c(129, 239, 79, 48),
  c(134, 232, 88, 44)))

109+190+ 82+ 33
154+ 281+ 85+ 41
122+314 +134+307

129+239+79+48+134+232+ 88+44

fisher.test(rbind(
  c(495, 436),
  c(498, 441)))

112+294
144+327

fisher.test(rbind(
  c(414, 406),
  c(561, 471)))

fisher.test(rbind(
  c(655, 276),
  c(654, 285)))

fisher.test(rbind(
  c(587, 251),
  c(722, 310)))

table(train_data_for_table1$Group)

train_data_for_table1$Group_2 <- ifelse(train_data_for_table1$Group == "EL",
                                        "T",
                                        "C")

mean(subset(train_data_for_table1,train_data_for_table1$Group_2 == "T")$Age)
mean(subset(train_data_for_table1,train_data_for_table1$Group_2 == "C")$Age)

summary(subset(train_data_for_table1,train_data_for_table1$Group_2 == "T")$Age)
summary(subset(train_data_for_table1,train_data_for_table1$Group_2 == "C")$Age)

sd(subset(train_data_for_table1,train_data_for_table1$Group_2 == "T")$Age)
sd(subset(train_data_for_table1,train_data_for_table1$Group_2 == "C")$Age)
wilcox.test(Age ~ Group_2,
            data = train_data_for_table1)

wilcox.test(AL ~ Group_2,
            data = train_data_for_table1)

wilcox.test(K1 ~ Group_2,
            data = train_data_for_table1)

wilcox.test(KSE ~ Group_2,
            data = train_data_for_table1)

wilcox.test(K_c ~ Group_2,
            data = train_data_for_table1)

wilcox.test(ACD ~ Group_2,
            data = train_data_for_table1)

wilcox.test(LT ~ Group_2,
            data = train_data_for_table1)

wilcox.test(WTW ~ Group_2,
            data = train_data_for_table1)

263/993
471/993
167/993
92/993

data_extra_validation_table

54+77+32+6+50+177+51+14+29+281+41+251

fisher.test(rbind(
  c(169,310 ),
  c(292, 292)))

266+348+449
fisher.test(rbind(
  c(236+213,614 ),

  c(811, 1059)))
449/1063
614
476/1063
1063-587
614/1063

602/1063
461/1063
1-877/1870

table(data_extra_validation_table$Group)

data_extra_validation_table$groups <- ifelse(data_extra_validation_table$Group == "EL",
                                             "T",
                                             "C")

wilcox.test(Age ~groups,
            data = data_extra_validation_table)

wilcox.test(AL ~groups,
            data = data_extra_validation_table)

wilcox.test(K1 ~groups,
            data = data_extra_validation_table)
wilcox.test(K2 ~groups,
            data = data_extra_validation_table)

wilcox.test(K_c ~groups,
            data = data_extra_validation_table)

data_extra_validation_table$WTW <- as.numeric(data_extra_validation_table$WTW)
wilcox.test(WTW ~groups,
            data = data_extra_validation_table)
