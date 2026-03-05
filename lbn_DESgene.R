# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

LBN_counts <- read.xlsx(("LBN/coding_counts.xlsx"))
LBN_fpkm <- read.xlsx(("LBN/coding_fpkm.xlsx"))
rownames(LBN_counts) <- LBN_counts$id
LBN_counts <- LBN_counts[, -1]
head(LBN_counts)

samples_with_outlier <- c("vehicle_1", "vehicle_2", "vehicle_3", "vehicle_4",
                          "POA_1", "POA_2", "POA_3", "POA_4")
counts_data_1 <- LBN_counts[, samples_with_outlier]

colData_1 <- data.frame(
  row.names = colnames(counts_data_1),
  condition = factor(c(rep("vehicle", 4), rep("POA", 4)))
)

print(colData_1)

library(DESeq2)
dds_1 <- DESeqDataSetFromMatrix(countData = counts_data_1,
                                colData = colData_1,
                                design = ~ condition)

dds_1$condition <- relevel(dds_1$condition, ref = "vehicle")

dds_1 <- DESeq(dds_1)

res_1 <- results(dds_1, contrast = c("condition", "POA", "vehicle"))

res_1_ordered <- res_1[order(res_1$padj), ]
head(res_1_ordered)
res_1_ordered$Gene <- rownames(res_1_ordered)
write.xlsx(res_1_ordered, "LBN/res_1_ordered.xlsx")

samples_no_outlier <- c("vehicle_1", "vehicle_2", "vehicle_3", "vehicle_4",
                        "POA_2", "POA_3", "POA_4")
counts_data_2 <- LBN_counts[, samples_no_outlier]

colData_2 <- data.frame(
  row.names = colnames(counts_data_2),
  condition = factor(c(rep("vehicle", 4), rep("POA", 3)))
)

print(colData_2)

dds_2 <- DESeqDataSetFromMatrix(countData = counts_data_2,
                                colData = colData_2,
                                design = ~ condition)

dds_2$condition <- relevel(dds_2$condition, ref = "vehicle")

dds_2 <- DESeq(dds_2)

res_2 <- results(dds_2, contrast = c("condition", "POA", "vehicle"))

res_2_ordered <- res_2[order(res_2$padj), ]
head(res_2_ordered)
res_2_ordered$Gene <- rownames(res_2_ordered)
write.xlsx(res_2_ordered, "LBN/res_2_ordered.xlsx")

plotCounts(dds_1, gene="A4GALT", intgroup="condition")
