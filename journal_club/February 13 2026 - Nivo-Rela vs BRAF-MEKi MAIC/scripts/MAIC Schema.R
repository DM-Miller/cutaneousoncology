library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(tibble)

######################################################################################################
## 1) Create Tables and Plots (GROBs)
######################################################################################################

# === Dummy KM Plot (COMBI) ===
combi_km_data <- tibble(time = c(0, 5, 10, 15, 20, 25, 30),
                        surv = c(1, 0.9, 0.75, 0.6, 0.4, 0.2, 0.1))

combi_km_plot <- ggplot(combi_km_data, aes(x = time, y = surv)) +
  geom_step(size = 1.2) +
  theme_minimal() +
  labs(title = "Kaplan-Meier Curve") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )
combi_km_grob <- ggplotGrob(combi_km_plot)

# === Dummy KM Plot (RELA) ===
rela_km_data <- tibble(time = c(0, 5, 10, 15, 20),
                       surv = c(1, 0.85, 0.7, 0.5, 0.3))

rela_km_plot <- ggplot(rela_km_data, aes(x = time, y = surv)) +
  geom_step(size = 1.2) +
  theme_minimal() +
  labs(title = "Kaplan-Meier Curve") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )
rela_km_grob <- ggplotGrob(rela_km_plot)

# === Overlayed KM Plot (MAIC Applied) ===
overlay_km_plot <- ggplot() +
  geom_step(data = combi_km_data, aes(x = time, y = surv), size = 1.2) +
  geom_step(data = rela_km_data, aes(x = time, y = surv), size = 1.2) +
  theme_minimal() +
  labs(title = "MAIC Applied KM Curves") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )
overlay_km_grob <- ggplotGrob(overlay_km_plot)

# === Fake Survival Tables ===
pseudo_ipd_surv_df <- tibble(
  time  = c(0.386, 0.802, 1.03, 1.302, 1.522),
  event = c(0, 1, 0, 1, 0)
)

rela_ipd_surv_df <- tibble(
  time  = c(0.42, 0.88, 1.12, 1.45, 1.9),
  event = c(0, 1, 1, 0, 1)
)

# Smaller table text here via base_size + padding
pseudo_ipd_surv_grob <- tableGrob(
  pseudo_ipd_surv_df,
  rows = NULL,
  theme = ttheme_default(
    base_size = 6.2,
    padding = unit(c(0.5, 0.5), "mm")
  )
)

rela_ipd_surv_grob <- tableGrob(
  rela_ipd_surv_df,
  rows = NULL,
  theme = ttheme_default(
    base_size = 6.2,
    padding = unit(c(0.5, 0.5), "mm")
  )
)

# === Dataframes for IPD tables ===
rela_df <- tibble(
  ID = 1:4,
  Female = c(1, 0, 1, 0),
  Age = c(47, 52, 77, 61),
  ECOG = c(0, 0, 1, 0),
  Time = c(5, 10, 15, 20),
  Event = c(1, 0, 1, 1),
  trt = rep("NIVO+RELA", 4)
)

rela_df_weighted <- rela_df |> mutate(Weight = c(1.2, 0.8, 1.0, 1.1))

combi_df <- tibble(
  Female   = 0.433,
  `Age <55` = 0.500,
  `ECOG 0` = 0.715,
  trt      = "DAB+TRAM"
)

# Shrink these as needed too
rela_grob <- tableGrob(
  rela_df,
  rows = NULL,
  theme = ttheme_default(
    base_size = 6.2,
    padding = unit(c(0.7, 0.7), "mm")
  )
)

rela_weighted_grob <- tableGrob(
  rela_df_weighted,
  rows = NULL,
  theme = ttheme_default(
    base_size = 6.2,
    padding = unit(c(0.7, 0.7), "mm")
  )
)

combi_grob <- tableGrob(
  combi_df,
  rows = NULL,
  theme = ttheme_default(
    base_size = 6.2,
    padding = unit(c(0.7, 0.7), "mm")
  )
)

######################################################################################################
## 2) Helper functions to build staged plot
######################################################################################################

base_canvas <- function() {
  ggplot() +
    xlim(0, 20) + ylim(0, 10) +
    theme_void() +
    # Background regions
    geom_rect(aes(xmin = 0.0, xmax = 10.0, ymin = 0.8, ymax = 9.2),
              fill = "#B3CDE3", alpha = 0.2, color = "black") +
    geom_rect(aes(xmin = 10.0, xmax = 20.0, ymin = 0.8, ymax = 9.2),
              fill = "#F4A582", alpha = 0.2, color = "black") +
    # Background headings
    annotate("text", x = 5.75, y = 9.4, label = "Data Acquisition & Processing",
             size = 4, fontface = "bold") +
    annotate("text", x = 15.5, y = 9.4, label = "Matched Analysis & Outcomes",
             size = 4, fontface = "bold") +
    # Title
    annotate("text", x = 10, y = 9.99, label = "Overall Schema of MAIC Process",
             size = 5.5, fontface = "bold", hjust = 0.5, vjust = 1)
}

add_col1 <- function(p) {
  p +
    # Column 1 boxes
    geom_rect(aes(xmin = 0.125, xmax = 4.875, ymin = 5.1, ymax = 9),
              fill = "#BBDEFB", color = "black") +
    geom_rect(aes(xmin = 0.125, xmax = 4.875, ymin = 1, ymax = 4.9),
              fill = "#FFCDD2", color = "black") +
    
    # Labels
    annotate("text", x = 2.5, y = 8.9,
             label = "Published Trials\nSummary-Level Data &\nKaplan-Meier Curves",
             size = 4, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 2.5, y = 4.75,
             label = "RELATIVITY-047\nIndividual Patient Data",
             size = 4, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    annotate("text", x = 2.5, y = 7.7,
             label = "Patient Characteristics - Summarized",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 2.5, y = 4.0,
             label = "Patient Characteristics - IPD",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    # Content: KM + tables
    annotation_custom(combi_km_grob, xmin = 0.75, xmax = 4, ymin = 5.1, ymax = 6.6) +
    annotation_custom(rela_km_grob,  xmin = 0.75, xmax = 4, ymin = 1.1, ymax = 2.6) +
    
    annotation_custom(combi_grob, xmin = 1.5, xmax = 3.5, ymin = 6.8, ymax = 7.8) +
    annotation_custom(rela_grob,  xmin = 1.5, xmax = 3.5, ymin = 2.7, ymax = 4.0)
}

add_col2 <- function(p) {
  p +
    # Column 2 boxes
    geom_rect(aes(xmin = 5.0, xmax = 9.875, ymin = 5.1, ymax = 9),
              fill = "#D7F7E4", color = "black") +
    geom_rect(aes(xmin = 5.0, xmax = 9.875, ymin = 1, ymax = 4.9),
              fill = "#D7F7E4", color = "black") +
    
    # Labels
    annotate("text", x = 7.5, y = 8.9,
             label = "Published Trials\nSummary-Level Data &\n Pseudo-IPD Survival Table",
             size = 4, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 7.5, y = 4.75,
             label = "RELATIVITY-047\nIndividual Patient Data",
             size = 4, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    annotate("text", x = 7.5, y = 7.7,
             label = "Patient Characteristics - Summarized",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 7.5, y = 4.0,
             label = "Patient Characteristics - IPD",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    annotate("text", x = 7.5, y = 6.5, label = "Survival Table",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 7.5, y = 2.55, label = "Survival Table",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    # Content: tables
    annotation_custom(combi_grob, xmin = 6.5, xmax = 8.5, ymin = 6.8, ymax = 7.8) +
    annotation_custom(rela_grob,  xmin = 6.5, xmax = 8.5, ymin = 2.7, ymax = 4.0) +
    
    annotation_custom(
      grob = grobTree(pseudo_ipd_surv_grob, vp = viewport(width = 0.7, height = 0.6)),
      xmin = 7, xmax = 8, ymin = 5.25, ymax = 6.4
    ) +
    annotation_custom(
      grob = grobTree(rela_ipd_surv_grob, vp = viewport(width = 0.7, height = 0.6)),
      xmin = 7, xmax = 8, ymin = 1.3, ymax = 2.45
    ) +
    
    # Arrow helpers (WebPlotDigitizer + Observed IPD)
    geom_rect(aes(xmin = 4.0, xmax = 6.0, ymin = 5.9, ymax = 6.5),
              fill = "#FFD580", color = "black") +
    geom_rect(aes(xmin = 4.0, xmax = 6.0, ymin = 2.18, ymax = 2.5),
              fill = "#FFD580", color = "black") +
    
    annotate("text", x = 5.0, y = 6.4,
             label = "Extract Pseudo-IPD\n(WebPlotDigitizer)",
             size = 3, vjust = 1, hjust = 0.5) +
    annotate("text", x = 5.0, y = 2.4,
             label = "Use Observed IPD",
             size = 3, vjust = 1, hjust = 0.5) +
    
    geom_curve(aes(x = 4.5, xend = 5.5, y = 5.8, yend = 5.8),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0) +
    geom_curve(aes(x = 4.5, xend = 5.5, y = 2, yend = 2),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0)
}

add_col3 <- function(p) {
  p +
    # Column 3 boxes
    geom_rect(aes(xmin = 10.125, xmax = 14.875, ymin = 5.1, ymax = 9),
              fill = "#FFF0B3", color = "black") +
    geom_rect(aes(xmin = 10.125, xmax = 14.875, ymin = 1, ymax = 4.9),
              fill = "#FFF0B3", color = "black") +
    
    # Labels
    annotate("text", x = 12.5, y = 8.9,
             label = "Published Trials\nSummary Level Data &\n Pseudo-IPD Survival Table",
             size = 4, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 12.5, y = 4.75,
             label = "RELATIVITY-047\n Weighted IPD",
             size = 4, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    annotate("text", x = 12.5, y = 7.7,
             label = "Patient Characteristics - Summarized",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 12.5, y = 4.0,
             label = "Patient Characteristics - Weighted IPD",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    annotate("text", x = 12.5, y = 6.5, label = "Survival Table",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    annotate("text", x = 12.5, y = 2.55, label = "Survival Table",
             size = 3.5, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    # Content: tables
    annotation_custom(combi_grob, xmin = 11.5, xmax = 13.5, ymin = 6.8, ymax = 7.8) +
    annotation_custom(rela_weighted_grob, xmin = 12.0, xmax = 13.0, ymin = 2.7, ymax = 4.0) +
    
    annotation_custom(
      grob = grobTree(pseudo_ipd_surv_grob, vp = viewport(width = 0.7, height = 0.6)),
      xmin = 12, xmax = 13, ymin = 5.25, ymax = 6.4
    ) +
    annotation_custom(
      grob = grobTree(rela_ipd_surv_grob, vp = viewport(width = 0.7, height = 0.6)),
      xmin = 12, xmax = 13, ymin = 1.3, ymax = 2.45
    ) +
    
    # Matching process badge
    geom_rect(aes(xmin = 8.9, xmax = 10.0, ymin = 4.38, ymax = 5.5),
              fill = "#FFD580", color = "black") +
    annotate("text", x = 9.45, y = 5.4,
             label = "Matching\nProcess\n(Weights\nEstimated)",
             size = 3, fontface = "italic") +
    
    # Arrows col2 -> col3 plus “matching” curves
    geom_curve(aes(x = 9.5, xend = 10.5, y = 5.8, yend = 5.8),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0) +
    geom_curve(aes(x = 9.5, xend = 10.5, y = 2, yend = 2),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0) +
    
    geom_curve(aes(x = 9.4, xend = 10.1, y = 7.1, yend = 4.0),
               arrow = arrow(length = unit(0.3, "cm")), curvature = -0.3) +
    geom_curve(aes(x = 9.4, xend = 10.1, y = 4.0, yend = 4.0),
               arrow = arrow(length = unit(0.3, "cm")), curvature = -0.3)
}

add_col4 <- function(p) {
  p +
    # Column 4 box
    geom_rect(aes(xmin = 15.0, xmax = 19.875, ymin = 1, ymax = 9),
              fill = "#FFB74D", color = "black") +
    
    annotate("text", x = 17.5, y = 8.9,
             label = "MAIC\n Published Trials &\nRELATIVITY-047",
             size = 4, fontface = "bold", vjust = 1, hjust = 0.5) +
    
    # Final overlay KM plot
    annotation_custom(overlay_km_grob, xmin = 16, xmax = 19.25, ymin = 4.5, ymax = 6.2) +
    
    # Arrows into final overlay KM
    geom_curve(aes(x = 14.99, xend = 16.0, y = 7.1, yend = 5.25),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0) +
    geom_curve(aes(x = 14.99, xend = 16.0, y = 6.0, yend = 5.25),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0) +
    geom_curve(aes(x = 14.99, xend = 16.0, y = 3.5, yend = 5.25),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0) +
    geom_curve(aes(x = 14.99, xend = 16.0, y = 2.5, yend = 5.25),
               arrow = arrow(length = unit(0.3, "cm")), curvature = 0)
}

build_schema <- function(stage = 4) {
  p <- base_canvas()
  if (stage >= 1) p <- add_col1(p)
  if (stage >= 2) p <- add_col2(p)
  if (stage >= 3) p <- add_col3(p)
  if (stage >= 4) p <- add_col4(p)
  p
}


