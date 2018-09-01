#!/usr/bin/env Rscript

# ---- Requirements ----
# Clear environment
rm(list=ls())
# install.packages(c("ggplot2","RColorBrewer","scales", "viridis"))
library(ggplot2); library(scales); library(grid); library(RColorBrewer); library(reshape2); library(viridis)

# ---- Import data ----
get_run_file <- function(name, id) {
  return(read.csv(paste(name, id, ".csv", sep = ""), colClasses = c("NULL", NA, NA), col.names = c(NA, "step", name), check.names = FALSE))
}
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
step_melt <- function(df) {
  melted = melt(df, id.var = "step")
  return(na.omit(melted))
}
plot_values <- function(values, name) {
  melted = step_melt(values)
  return(ggplot(melted, aes(step, y=value, color=variable)) + # lty=variable ?
    # geom_point() +
    geom_line(size = .5) +
    # geom_smooth(method = lm, se = FALSE) +
    # theme_bw() +
    # theme_minimal() +
    theme(legend.position="none", # Remove legend
          text=element_text(size=10),
          plot.margin=margin(10, 10, 5, 5)
    ) +
    # scale_colour_brewer(palette="Set1") +
    scale_color_viridis(discrete=TRUE) + # option="plasma" ?
    # scale_x_continuous(labels=scientific) +
    # scale_y_continuous(labels=scientific) +
    labs(x="Trainingsschritt", y="Wert")
  )
}
export_plot <- function(name) ggsave(paste(name, "pdf", sep="."), dpi=300, width=4, height=3)

process <- function(values, filename) {
  plot = plot_values(values)
  # print(plot)
  export_plot(filename)
  return(plot)
}
process_disc <- function(runs, filename_prefix) process(get_values(runs, id = "-discr-loss"), paste(filename_prefix, "disc", sep="-"))
process_gen <- function(runs, filename_prefix) process(get_values(runs, id = "-gen-loss"), paste(filename_prefix, "gen", sep="-"))
process_l1 <- function(runs, filename_prefix) process(get_values(runs, id = "-l1-loss"), paste(filename_prefix, "l1", sep="-"))
process_iou <- function(runs, filename_prefix) process(get_values(runs, id = "-iou-val"), paste(filename_prefix, "iou", sep="-"))
process_full <- function(runs, name, iou=TRUE) {
  process_disc(runs, name)
  process_gen(runs, name)
  process_l1(runs, name)
  if (iou) {
    process_iou(runs, name)
  }
}

# ---- Create all plots ----

process_full(c("2017_initial"), "ini", iou=FALSE)
process_full(c("2017_min_samples"), "min_samples", iou=FALSE)
process_full(c("2017_b01_A", "2017_b02_A", "2017_b04_A", "2017_b04_B"), "b010204")
process_full(c("2017_b03_A", "2017_b03_B"), "b03")
process_full(c("2017_b45_A", "2017_b45_B", "2017_b64_A", "2017_b64_B"), "b4564")
process_full(c("2017_b03_A", "2017_b04_A", "2017_b08_B", "2017_b16_B", "2017_b32_B"), "main_worst")
process_full(c("2017_b03_B", "2017_b04_B", "2017_b08_A", "2017_b16_A", "2017_b32_A"), "main_best")
process_full(c("2017_b08_A", "2017_b08_B", "2017_b16_A", "2017_b16_B", "2017_b32_A", "2017_b32_B"), "b081632")
process_full(c("2017_btoa"), "btoa", iou=FALSE)
process_full(c("2018_baseline_A", "2018_baseline_B", "2018_baseline_C", "2018_only_L1_A", "2018_only_L1_B", "2018_only_L1_C"), "baselinel1")
