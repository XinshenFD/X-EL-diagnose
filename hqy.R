# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

library(ggsankeyfier)
library(magrittr)
library(ggalluvial)
library(RColorBrewer)

hqy <- read.xlsx("hqy/SPSS_select.xlsx")
colnames(hqy)
hqy
select_name <- c("name", "NMC", "Z_AL", "Z_CCR",
                 "AL", "CCT", "Mutation type", "WTW", "CCR")
hqy_select <- hqy
colnames(hqy_select) <-select_name

hqy_select$NMC <- factor(hqy_select$NMC,
                         levels = c("N-terminal",
                                    "Middle",
                                    "C-terminal"))

hqy_select$AL <- as.numeric(hqy_select$AL)
hqy_select$CCT <- as.numeric(hqy_select$CCT)

hqy_select$WTW <- as.numeric(hqy_select$WTW)
table(hqy_select$`Mutation type`)

hqy_select <- subset(hqy_select,
                     !(hqy_select$`Mutation type` %in% c("DN(Inframe)")))

colnames(hqy_select)

colnames(hqy_select)[2] <- "Mutation region"
colnames(hqy_select)[3] <- "Z-AL"
colnames(hqy_select)[4] <- "Z-CCR"

table(hqy_select$`Mutation type`)

hqy_select <- subset(hqy_select,
                     is.na(hqy_select$`Mutation type`) == F)

table(hqy_select$`Mutation type`)

hqy_select$`Mutation type` <- ifelse(hqy_select$`Mutation type` %in% c("CaB-C-loop", "CaB-C-loop-conserved 5",
                                                                       "CaB-N-loop-conserved 5"),
                                     "DN(CaB)",
                                     hqy_select$`Mutation type`)

library(tidyverse)
library(ggsankeyfier)

library(RColorBrewer)
library(patchwork)
library(ggsankey)

pal_aaas()(10)

table(hqy_select$`Mutation type`)

plot_ggsankeyfier <- function(data, var_name, color_palette_name, title_text) {

  plot_data_binned <- data %>%
    dplyr::select(`Mutation region`, target_var = all_of(var_name), `Mutation type`) %>%
    mutate(

      bin = ntile(target_var, 4),

      stratum_label = factor(paste0(var_name, " Q", bin), levels = paste0(var_name, " Q", 1:4))
    )

  long_data <- make_long(plot_data_binned, `Mutation region`, stratum_label, `Mutation type`)
  mid_levels <- paste0(var_name, " Q", 1:4)
  desired_order_top_to_bottom <- c(
    "N-terminal", "Middle", "C-terminal",
    mid_levels,
    "DN(-Cys)",       "DN(Other)","DN(CaB)", "HI"
  )

  levels_order <- rev(desired_order_top_to_bottom)
  long_data$node <- factor(long_data$node, levels = levels_order)
  long_data$next_node <- factor(long_data$next_node, levels = levels_order)

  cols_region <- c("N-terminal" = "#7fc97f", "Middle" = "#beaed4", "C-terminal" = "#fdc086")

  cols_severity <- c("DN(-Cys)" = "#66c2a5", "DN(CaB)" = "#fc8d62", "DN(Other)" = "#8da0cb", "HI" =  "#631879FF")

  cols_mid <- brewer.pal(6, color_palette_name)[3:6]
  names(cols_mid) <-  mid_levels

  all_colors <- c(cols_region, cols_mid, cols_severity)

  ggplot(long_data, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                        fill = factor(node), label = node)) +
    geom_sankey(flow.alpha = 0.7, node.color = "gray30", width = 0.2) +
    geom_sankey_label(aes(x = x, node = node), size = 3, color = "black", fill = "white",
                      stat = "sankey_node") +
    scale_fill_manual(values = all_colors) +
    labs(title = title_text, x = "", y = "") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "none"
    )
}

hqy_select_al <- subset(hqy_select,
                        is.na(hqy_select$`Mutation region`) == F & is.na(hqy_select$AL) == F)

p1_sankeyfier <- plot_ggsankeyfier(hqy_select_al , "AL", "Greens", "Mutation region -> AL -> Mutation type")

hqy_select_cct <- subset(hqy_select,
                        is.na(hqy_select$`Mutation region`) == F & is.na(hqy_select$CCT) == F)

p2_sankeyfier <- plot_ggsankeyfier(hqy_select_cct, "CCT", "Blues", "Mutation region -> CCT ->  Mutation type")

hqy_select_wtw <- subset(hqy_select,
                         is.na(hqy_select$`Mutation region`) == F & is.na(hqy_select$WTW) == F)

p4_sankeyfier <- plot_ggsankeyfier(hqy_select_wtw, "WTW", "Purples", "Mutation region -> WTW ->  Mutation type")

hqy_select_zal <- subset(hqy_select,
                         is.na(hqy_select$`Mutation region`) == F & is.na(hqy_select$`Z-AL`) == F)
p5_sankeyfier <- plot_ggsankeyfier(hqy_select_zal, "Z-AL", "RdPu", "Mutation region -> Z-AL ->  Mutation type")

hqy_select_zccr <- subset(hqy_select,
                         is.na(hqy_select$`Mutation region`) == F & is.na(hqy_select$`Z-CCR`) == F)
p6_sankeyfier <- plot_ggsankeyfier(hqy_select_zccr, "Z-CCR", "PuBuGn", "Mutation region -> Z-CCT ->  Mutation type")

hqy_select_ccr <- subset(hqy_select,
                          is.na(hqy_select$`Mutation region`) == F & is.na(hqy_select$CCR) == F)

p3_sankeyfier <- plot_ggsankeyfier(hqy_select_ccr , "CCR", "Oranges", "Mutation region -> CCR ->  Mutation type")

combined_plot_sankeyfier <- (p1_sankeyfier + p2_sankeyfier) / (p3_sankeyfier + p4_sankeyfier)

print(combined_plot_sankeyfier)

ggsave("hqy/p1_sankeyfier_AL.tiff", p1_sankeyfier, width = 8, height = 2,
       dpi = 300)
ggsave("hqy/p2_sankeyfier_CCT.tiff", p2_sankeyfier, width = 8, height = 2,
       dpi = 300)
ggsave("hqy/p3_sankeyfier_CCR.tiff", p3_sankeyfier, width = 8, height = 2,
       dpi = 300)
ggsave("hqy/p4_sankeyfier_WTW.tiff", p4_sankeyfier, width = 8, height = 2,
       dpi = 300)
ggsave("hqy/p5_sankeyfier_Z-AL.tiff", p5_sankeyfier, width = 8, height = 2,
       dpi = 300)
ggsave("hqy/p6_sankeyfier_Z-CCR.tiff", p6_sankeyfier, width = 8, height = 2,
       dpi = 300)
