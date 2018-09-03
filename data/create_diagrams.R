#!/usr/bin/env Rscript

# ---- Requirements ----
# Clear environment
rm(list=ls())
# install.packages(c("ggplot2","RColorBrewer","scales", "viridis"))
library(ggplot2); library(scales); library(grid); library(RColorBrewer); library(reshape2); library(viridis)

# ---- Import data ----
get_run_file <- function(name, id) read.csv(paste(name, id, ".csv", sep = ""), colClasses = c("NULL", NA, NA), col.names = c(NA, "step", name), check.names = FALSE)
get_values <- function(runs, id) {
  df = data.frame()
  for (run in runs) {
    loss = get_run_file(run, id)
    if (length(df) == 0) {
      df = loss
    } else {
      df = merge(df, loss, by = "step", all.x = TRUE)
    }
  }
  return(df)
}

# ---- Plotting ----
plot_values <- function(values, by="step") {
  melted = melt(values, id.var=by)
  melted = na.omit(melted)
  return(ggplot(melted, aes_string(by, y="value", color="variable")) + # lty=variable ?
    geom_line(size = .5) +
    # geom_smooth(method = lm, se = FALSE) +
    # theme_bw() +
    # theme_minimal() +
    theme(legend.position="none", # Remove legend
          axis.title.x=element_blank(), # Remove axis titles
          axis.title.y=element_blank(),
          text=element_text(size=10)
          # plot.margin=margin(10, 10, 5, 5)
    ) +
    # scale_colour_brewer(palette="Set1") +
    scale_color_viridis(discrete=TRUE) #+ # option="plasma" ?
    # scale_x_continuous(labels=scientific) +
    # scale_y_continuous(labels=scientific) +
    # labs(x="Trainingsschritt", y="Wert")
  )
}
export_plot <- function(name) ggsave(paste(name, "pdf", sep="."), dpi=300, width=4, height=3)

process <- function(values, filename) {
  plot = plot_values(values)
  # print(plot)
  export_plot(filename)
  return(plot)
}

values <- new.env(hash=TRUE)
values$disc <- "-discr-loss"
values$gen <- "-gen-loss"
values$l1 <- "-l1-loss"
values$iou <- "-iou-val"
process_full <- function(runs, name, iou=TRUE) {
  # runs: A vector of filename prefixes to import data and export plots for all runs.
  # name: Output files name prefix
  # iou: Specify if iou-val file exists.
  for (v in ls(values)) {
    if (iou || v != "iou") {
      process(get_values(runs, id=values[[v]]), paste(name, v, sep="-"))
    }
  }
}

# ---- Create all plots ----

# Initial
process_full(c("2017_initial"), "ini", iou=FALSE)
process_full(c("2017_min_samples"), "min_samples", iou=FALSE)

# Batch size
process_full(c("2017_b01_A", "2017_b02_A", "2017_b04_A", "2017_b04_B"), "b010204")
process_full(c("2017_b03_A", "2017_b03_B"), "b03")
process_full(c("2017_b45_A", "2017_b45_B", "2017_b64_A", "2017_b64_B"), "b4564")
process_full(c("2017_b03_A", "2017_b04_A", "2017_b08_B", "2017_b16_B", "2017_b32_B"), "main_worst")
process_full(c("2017_b03_B", "2017_b04_B", "2017_b08_A", "2017_b16_A", "2017_b32_A"), "main_best")
process_full(c("2017_b08_A", "2017_b08_B", "2017_b16_A", "2017_b16_B", "2017_b32_A", "2017_b32_B"), "b081632")

# Augmentations
process_full(c("2018_zoom_A", "2018_rotate_A", "2018_shear_B", "2018_all_aug_A"), "aug_best")
process_full(c("2018_zoom_B", "2018_rotate_B", "2018_shear_C", "2018_all_aug_B"), "aug_worst")
process_full(c("2018_shear_A", "2018_shear_B", "2018_shear_C"), "shear")

# Misc
process_full(c("2017_btoa"), "btoa", iou=FALSE)
process_full(c("2018_baseline_A", "2018_baseline_B", "2018_baseline_C", "2018_only_L1_A", "2018_only_L1_B", "2018_only_L1_C"), "baselinel1")

# Early stopping main
es_iou_val <- read.csv2("batch_size_es.csv", sep=";", colClasses=c(NA, "NULL", "NULL", "NULL", NA, NA, NA), check.names = FALSE)
plot_values(es_iou_val, by="batch_size") +
  scale_x_continuous(breaks=unique(es_iou_val$batch_size), trans="log2") +
  theme(axis.title.x=element_text(margin=margin(2, 0, 0, 0)), axis.title.y=element_text(angle=90, margin=margin(0, 7, 0, 0))) +
  labs(x="Batch-Größe (log2)", y="Early-Stopping-Wert IoU auf Val.")
export_plot("main_es")
