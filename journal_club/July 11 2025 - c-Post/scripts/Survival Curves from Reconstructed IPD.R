# Load Packages
library(tidyverse)
library(survival)
library(survminer)
library(tidyverse)
source(here::here("scripts", "open most recent file.R"))
source(here::here("scripts", "save files.R"))

# Load Data
dir <- here::here("files/reconstructed survival tables")
# CPOST RFS
subD <- paste("CPOST","rfs", sep = "/")
# CPOST Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
cpost_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(cpost_placebo_rfs)
# add trt
cpost_placebo_rfs <- cpost_placebo_rfs |> mutate(trt = "CPOST Placebo")
print(cpost_placebo_rfs, n = 206)
# CPOST Cemiplimab
drug <- "cemiplimab"
file_path <- file.path(dir, subD, drug)
file_path
cpost_cemiplimab_rfs <- open_recent_file(
  directory = file_path
)
nrow(cpost_cemiplimab_rfs)
# add trt
cpost_cemiplimab_rfs <- cpost_cemiplimab_rfs |> mutate(trt = "Cemiplimab")
print(cpost_cemiplimab_rfs, n = 209)



# KN630 RFS
subD <- paste("KN630","rfs", sep = "/")
# KN630 Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
kn630_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(kn630_placebo_rfs)
# add trt
kn630_placebo_rfs <- kn630_placebo_rfs |> mutate(trt = "KN630 Placebo")
# KN630 Pembro
drug <- "pembrolizumab"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
kn630_pembro_rfs <- open_recent_file(
  directory = file_path
) 
nrow(kn630_pembro_rfs)
# add trt
kn630_pembro_rfs <- kn630_pembro_rfs |> mutate(trt = "Pembrolizumab")

# Combine data sets
rfs_data_combined <- rbind(
  cpost_placebo_rfs,
  cpost_cemiplimab_rfs,
  kn630_placebo_rfs,
  kn630_pembro_rfs
)
nrow(rfs_data_combined)

# Recode trt to clean names
rfs_data_combined <- rfs_data_combined %>%
  mutate(
    trt_clean = recode(trt,
                       "Cemiplimab" = "Cemiplimab",
                       "CPOST Placebo" = "CPOST Placebo",
                       "KN630 Placebo" = "KN630 Placebo",
                       "Pembrolizumab" = "Pembrolizumab"
    )
  )


# Combine data: `rfs_data_combined` already exists
# It must have at least:
# - time
# - status
# - trt

# Fit the survival model stratified by treatment
fit_combined <- survfit(Surv(time, status) ~ trt_clean, data = rfs_data_combined)
fit_combined_sum <- summary(fit_combined)

# --- Create base ggsurvplot with risk table ---
ggsurv_combined <- ggsurvplot(
  fit_combined,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = FALSE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 66),
  ylim = c(0, 1),
  risk.table.height = 0.25,   # Make risk table bigger
  risk.table.title = "Number at Risk",
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  legend.labs = c(
    "Cemiplimab",
    "CPOST Placebo",
    "KN630 Placebo",
    "Pembrolizumab"
  ),
  legend.title = ""
)


# Extract ggplot
p <- ggsurv_combined$plot

# Define time points
time_points <- c(12, 24, 36)

# Summarize survival
sumsurv <- summary(fit_combined, times = time_points)

# Labels
surv_probs <- sumsurv$surv
surv_labels <- paste0(round(surv_probs * 100, 1), "%")

# Annot data frame
annot_df <- tibble(
  time = rep(time_points, length(unique(rfs_data_combined$trt))),
  y = surv_probs,
  label = surv_labels,
  group_raw = sumsurv$strata
) %>%
  mutate(
    time = time + 0.75,
    vjust = case_when(
      grepl("CPOST Placebo", group_raw) ~ 2,                          # Move CPOST Placebo labels down
      grepl("KN630 Placebo", group_raw) & time < 15 ~ 2,              # Move KN630 Placebo 12-month label down
      TRUE ~ -0.5
    ),
    # Clean label for coloring only
    group = case_when(
      grepl("Cemiplimab", group_raw) ~ "Cemiplimab",
      grepl("CPOST Placebo", group_raw) ~ "CPOST Placebo",
      grepl("KN630 Placebo", group_raw) ~ "KN630 Placebo",
      grepl("Pembrolizumab", group_raw) ~ "Pembrolizumab"
    )
  )


# Add vertical lines + annotations
p <- p +
  geom_vline(
    xintercept = time_points,
    linetype = "dashed",
    color = "black"
  ) +
  geom_text(
    data = annot_df,
    aes(
      x = time,
      y = y,
      label = label,
      color = group
    ),
    size = 4.2,
    fontface = "bold",
    hjust = 0,
    vjust = annot_df$vjust
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "CPOST and KN630 Recurrence-Free Survival",
    subtitle = "(Reconstructed)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    legend.title = element_blank()
  ) 


# Update the plot in the ggsurv object
ggsurv_combined$plot <- p

# Print final combined plot
print(ggsurv_combined)

save_files(
  save_object = ggsurv_combined,
  filename = "KM Plot of Combined RFS Data",
  subD = "img/reconstructed KM curves/KN630 and CPOST Combined"
)

#------------------------


#--------------------------------
# try different plot
# Combine data sets
rfs_data_combined <- rbind(
  cpost_placebo_rfs,
  cpost_cemiplimab_rfs
)
nrow(rfs_data_combined)

# Recode trt to clean names
rfs_data_combined <- rfs_data_combined %>%
  mutate(
    trt_clean = recode(trt,
                       "Cemiplimab" = "Cemiplimab",
                       "CPOST Placebo" = "Placebo",
    )
  )


# Combine data: `rfs_data_combined` already exists
# It must have at least:
# - time
# - status
# - trt

# Fit the survival model stratified by treatment
fit_combined <- survfit(Surv(time, status) ~ trt_clean, data = rfs_data_combined)
fit_combined_sum <- summary(fit_combined)

# --- Create base ggsurvplot with risk table ---
ggsurv_combined <- ggsurvplot(
  fit_combined,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = FALSE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 66),
  ylim = c(0, 1),
  risk.table.height = 0.25,   # Make risk table bigger
  risk.table.title = "Number at Risk",
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  legend.labs = c(
    "Cemiplimab",
    "Placebo"
  ),
  legend.title = ""
)


# Extract ggplot
p <- ggsurv_combined$plot

# Define time points
time_points <- c(12, 24, 36)

# Summarize survival
sumsurv <- summary(fit_combined, times = time_points)

# Labels
surv_probs <- sumsurv$surv
surv_labels <- paste0(round(surv_probs * 100, 1), "%")

# Annot data frame
annot_df <- tibble(
  time = rep(time_points, length(unique(rfs_data_combined$trt))),
  y = surv_probs,
  label = surv_labels,
  group_raw = sumsurv$strata
) %>%
  mutate(
    time = time + 0.75,
    vjust = -0.5,
    # Clean label for coloring only
    group = case_when(
      grepl("Cemiplimab", group_raw) ~ "Cemiplimab",
      grepl("Placebo", group_raw) ~ "Placebo"
    )
  )


# Add vertical lines + annotations
p <- p +
  geom_vline(
    xintercept = time_points,
    linetype = "dashed",
    color = "black"
  ) +
  geom_text(
    data = annot_df,
    aes(
      x = time,
      y = y,
      label = label,
      color = group
    ),
    size = 4.2,
    fontface = "bold",
    hjust = 0,
    vjust = annot_df$vjust
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "CPOST Recurrence-Free Survival",
    subtitle = "(Reconstructed)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    legend.title = element_blank()
  ) 


# Update the plot in the ggsurv object
ggsurv_combined$plot <- p
rfs_data_combined$trt_clean <- factor(
  rfs_data_combined$trt_clean,
  levels = c("Placebo", "Cemiplimab")
)

# Fit Cox model
cox_model <- coxph(Surv(time, status) ~ trt_clean, data = rfs_data_combined)

# Get HR and CI
hr_summary <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# Extract HR and CI as nicely formatted string
hr_text <- glue::glue(
  "HR: {round(hr_summary$estimate, 2)} ",
  "(95% CI: {round(hr_summary$conf.low, 2)}–{round(hr_summary$conf.high, 2)})"
)
# Add the annotation to your plot `p`
p <- p +
  annotate(
    "text",
    x = Inf,          # right edge
    y = -Inf,         # bottom edge
    label = hr_text,
    hjust = 1,        # right align
    vjust = -1,       # push below axis a bit
    size = 4.5,
    fontface = "bold"
  )



# Update the plot in the ggsurv object
ggsurv_combined$plot <- p

# Print final combined plot
print(ggsurv_combined)


save_files(
  save_object = ggsurv_combined,
  filename = "KM Plot of RFS Data for CPOST",
  subD = "img/reconstructed KM curves/CPOST"
)
#------------------------
# KN630 Only
# Load Data
dir <- here::here("files/reconstructed survival tables")
subD <- paste("KN630","rfs", sep = "/")
# KN630 Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
kn630_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(kn630_placebo_rfs)
# add trt
kn630_placebo_rfs <- kn630_placebo_rfs |> mutate(trt = "Placebo")
# KN630 Pembro
drug <- "pembrolizumab"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
kn630_pembro_rfs <- open_recent_file(
  directory = file_path
) 
nrow(kn630_pembro_rfs)
# add trt
kn630_pembro_rfs <- kn630_pembro_rfs |> mutate(trt = "Pembrolizumab")

# Combine data sets
rfs_data_combined <- rbind(
  kn630_placebo_rfs,
  kn630_pembro_rfs
)
nrow(rfs_data_combined)

# Recode trt to clean names
rfs_data_combined <- rfs_data_combined %>%
  mutate(
    trt_clean = recode(trt,
                       "Pembrolizumab" = "Pembrolizumab",
                       "Placebo" = "Placebo",
    )
  )


# Combine data: `rfs_data_combined` already exists
# It must have at least:
# - time
# - status
# - trt

# Fit the survival model stratified by treatment
fit_combined <- survfit(Surv(time, status) ~ trt_clean, data = rfs_data_combined)
fit_combined_sum <- summary(fit_combined)

# --- Create base ggsurvplot with risk table ---
ggsurv_combined <- ggsurvplot(
  fit_combined,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = FALSE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 66),
  ylim = c(0, 1),
  risk.table.height = 0.25,   # Make risk table bigger
  risk.table.title = "Number at Risk",
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  legend.labs = c(
    "Pembrolizumab",
    "Placebo"
  ),
  legend.title = ""
)


# Extract ggplot
p <- ggsurv_combined$plot

# Define time points
time_points <- c(12, 24, 36)

# Summarize survival
sumsurv <- summary(fit_combined, times = time_points)

# Labels
surv_probs <- sumsurv$surv
surv_labels <- paste0(round(surv_probs * 100, 1), "%")

# Annot data frame
annot_df <- tibble(
  time = rep(time_points, length(unique(rfs_data_combined$trt))),
  y = surv_probs,
  label = surv_labels,
  group_raw = sumsurv$strata
) %>%
  mutate(
    time = time + 0.75,
    vjust = case_when(
      grepl("Placebo", group_raw) & time < 15 ~ 3,  # Move KN630 Placebo 12-month label down
      grepl("Placebo", group_raw) & time > 32 ~ 2,  # Move KN630 Placebo 12-month label down
      TRUE ~ -0.5
    ),
    # Clean label for coloring only
    group = case_when(
      grepl("Pembrolizumab", group_raw) ~ "Pembrolizumab",
      grepl("Placebo", group_raw) ~ "Placebo",
    )
  )

# Compute end points
# Create a tidy data frame of survival steps
# Create a tidy data frame of survival steps
fit_df <- broom::tidy(fit_combined)

# For each group, take the last time point
end_labels <- fit_df %>%
  group_by(strata) %>%
  filter(time == max(time)) %>%
  mutate(
    label = case_when(
      grepl("Pembrolizumab", strata) ~ "Pembrolizumab",
      grepl("Placebo", strata) ~ "Placebo"
    ),
    color = case_when(
      label == "Pembrolizumab" ~ "#E41A1C",
      label == "Placebo" ~ "#377EB8"
    )
  ) %>%
  select(time, estimate, label, color)

# Add vjust parameter so Cemiplimab stays above, Placebo goes below
end_labels <- end_labels %>%
  mutate(
    vjust = ifelse(label == "Placebo", 1.5, -0.5)
  )


# Add vertical lines + annotations
p <- p +
  geom_vline(
    xintercept = time_points,
    linetype = "dashed",
    color = "black"
  ) +
  geom_text(
    data = annot_df,
    aes(
      x = time,
      y = y,
      label = label,
      color = group
    ),
    size = 4.2,
    fontface = "bold",
    hjust = 0,
    vjust = annot_df$vjust
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "KN630 Recurrence-Free Survival",
    subtitle = "(Reconstructed)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold"
    ),
    legend.title = element_blank()
  ) 

# Remove the legend
p <- p + theme(legend.position = "none")

# Add text annotations
p <- p + geom_text(
  data = end_labels,
  aes(
    x = time -5,
    y = estimate,
    label = label,
    color = label,
    vjust = vjust
  ),
  hjust = 0,
  fontface = "bold",
  size = 5
)


rfs_data_combined$trt_clean <- factor(
  rfs_data_combined$trt_clean,
  levels = c("Placebo", "Pembrolizumab")
)

# Fit Cox model
cox_model <- coxph(Surv(time, status) ~ trt_clean, data = rfs_data_combined)

# Get HR and CI
hr_summary <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# Extract HR and CI as nicely formatted string
hr_text <- glue::glue(
  "HR: {round(hr_summary$estimate, 2)} ",
  "(95% CI: {round(hr_summary$conf.low, 2)}–{round(hr_summary$conf.high, 2)})"
)
# Add the annotation to your plot `p`
p <- p +
  annotate(
    "text",
    x = Inf,          # right edge
    y = -Inf,         # bottom edge
    label = hr_text,
    hjust = 1,        # right align
    vjust = -1,       # push below axis a bit
    size = 4.5,
    fontface = "bold"
  )



# Update the plot in the ggsurv object
ggsurv_combined$plot <- p

# Print final combined plot
print(ggsurv_combined)

save_files(
  save_object = ggsurv_combined,
  filename = "KM Plot of RFS Data for KN630",
  subD = "img/reconstructed KM curves/KN630"
)



#------------------------------------------
# Placego Only
# Load Data
dir <- here::here("files/reconstructed survival tables")
# CPOST RFS
subD <- paste("CPOST","rfs", sep = "/")
# CPOST Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
cpost_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(cpost_placebo_rfs)
# add trt
cpost_placebo_rfs <- cpost_placebo_rfs |> mutate(trt = "CPOST Placebo")
print(cpost_placebo_rfs, n = 206)



# KN630 RFS
subD <- paste("KN630","rfs", sep = "/")
# KN630 Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
kn630_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(kn630_placebo_rfs)
# add trt
kn630_placebo_rfs <- kn630_placebo_rfs |> mutate(trt = "KN630 Placebo")


# Combine data sets
rfs_data_combined <- rbind(
  cpost_placebo_rfs,
  kn630_placebo_rfs
)
nrow(rfs_data_combined)

# Recode trt to clean names
rfs_data_combined <- rfs_data_combined %>%
  mutate(
    trt_clean = recode(trt,
                       "CPOST Placebo" = "CPOST Placebo",
                       "KN630 Placebo" = "KN630 Placebo"
    )
  )


# Combine data: `rfs_data_combined` already exists
# It must have at least:
# - time
# - status
# - trt

# Fit the survival model stratified by treatment
fit_combined <- survfit(Surv(time, status) ~ trt_clean, data = rfs_data_combined)
fit_combined_sum <- summary(fit_combined)

# --- Create base ggsurvplot with risk table ---
ggsurv_combined <- ggsurvplot(
  fit_combined,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = FALSE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 66),
  ylim = c(0, 1),
  risk.table.height = 0.25,   # Make risk table bigger
  risk.table.title = "Number at Risk",
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  legend.labs = c(
    "CPOST Placebo",
    "KN630 Placebo"
  ),
  legend.title = ""
)


# Extract ggplot
p <- ggsurv_combined$plot

# Define time points
time_points <- c(12, 24, 36)

# Summarize survival
sumsurv <- summary(fit_combined, times = time_points)

# Labels
surv_probs <- sumsurv$surv
surv_labels <- paste0(round(surv_probs * 100, 1), "%")

# Annot data frame
annot_df <- tibble(
  time = rep(time_points, length(unique(rfs_data_combined$trt))),
  y = surv_probs,
  label = surv_labels,
  group_raw = sumsurv$strata
) %>%
  mutate(
    time = time + 0.75,
    vjust = case_when(
      grepl("CPOST Placebo", group_raw) ~ 2,                          # Move CPOST Placebo labels down
      grepl("KN630 Placebo", group_raw) & time < 15 ~ -0.5,              # Move KN630 Placebo 12-month label down
      TRUE ~ -0.5
    ),
    # Clean label for coloring only
    group = case_when(
      grepl("CPOST Placebo", group_raw) ~ "CPOST Placebo",
      grepl("KN630 Placebo", group_raw) ~ "KN630 Placebo"
    )
  )


# Add vertical lines + annotations
p <- p +
  geom_vline(
    xintercept = time_points,
    linetype = "dashed",
    color = "black"
  ) +
  geom_text(
    data = annot_df,
    aes(
      x = time,
      y = y,
      label = label,
      color = group
    ),
    size = 4.2,
    fontface = "bold",
    hjust = 0,
    vjust = annot_df$vjust
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "CPOST and KN630 Recurrence-Free Survival",
    subtitle = "(Placebo Only)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    legend.title = element_blank()
  ) 


rfs_data_combined$trt_clean <- factor(
  rfs_data_combined$trt_clean,
  levels = c("KN630 Placebo", "CPOST Placebo")
)

# Fit Cox model
cox_model <- coxph(Surv(time, status) ~ trt_clean, data = rfs_data_combined)

# Get HR and CI
hr_summary <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# Extract HR and CI as nicely formatted string
hr_text <- glue::glue(
  "HR: {round(hr_summary$estimate, 2)} ",
  "(95% CI: {round(hr_summary$conf.low, 2)}–{round(hr_summary$conf.high, 2)})"
)
# Add the annotation to your plot `p`
p <- p +
  annotate(
    "text",
    x = Inf,          # right edge
    y = -Inf,         # bottom edge
    label = hr_text,
    hjust = 1,        # right align
    vjust = -1,       # push below axis a bit
    size = 4.5,
    fontface = "bold"
  )


# Update the plot in the ggsurv object
ggsurv_combined$plot <- p

# Print final combined plot
print(ggsurv_combined)

save_files(
  save_object = ggsurv_combined,
  filename = "KM Plot of Combined RFS Data For Placebo Groups",
  subD = "img/reconstructed KM curves/KN630 and CPOST Placebo Only"
)


#-----------------
# Cemi vs. Pembro
# Load Data
dir <- here::here("files/reconstructed survival tables")
# CPOST RFS
subD <- paste("CPOST","rfs", sep = "/")
# CPOST Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
cpost_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(cpost_placebo_rfs)
# add trt
cpost_placebo_rfs <- cpost_placebo_rfs |> mutate(trt = "CPOST Placebo")
print(cpost_placebo_rfs, n = 206)
# CPOST Cemiplimab
drug <- "cemiplimab"
file_path <- file.path(dir, subD, drug)
file_path
cpost_cemiplimab_rfs <- open_recent_file(
  directory = file_path
)
nrow(cpost_cemiplimab_rfs)
# add trt
cpost_cemiplimab_rfs <- cpost_cemiplimab_rfs |> mutate(trt = "Cemiplimab")
print(cpost_cemiplimab_rfs, n = 209)



# KN630 RFS
subD <- paste("KN630","rfs", sep = "/")
# KN630 Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
kn630_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(kn630_placebo_rfs)
# add trt
kn630_placebo_rfs <- kn630_placebo_rfs |> mutate(trt = "KN630 Placebo")
# KN630 Pembro
drug <- "pembrolizumab"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
kn630_pembro_rfs <- open_recent_file(
  directory = file_path
) 
nrow(kn630_pembro_rfs)
# add trt
kn630_pembro_rfs <- kn630_pembro_rfs |> mutate(trt = "Pembrolizumab")

# Combine data sets
rfs_data_combined <- rbind(
  cpost_cemiplimab_rfs,
  kn630_pembro_rfs
)
nrow(rfs_data_combined)

# Recode trt to clean names
rfs_data_combined <- rfs_data_combined %>%
  mutate(
    trt_clean = recode(trt,
                       "Cemiplimab" = "Cemiplimab",
                       "Pembrolizumab" = "Pembrolizumab"
    )
  )


# Combine data: `rfs_data_combined` already exists
# It must have at least:
# - time
# - status
# - trt

# Fit the survival model stratified by treatment
fit_combined <- survfit(Surv(time, status) ~ trt_clean, data = rfs_data_combined)
fit_combined_sum <- summary(fit_combined)

# --- Create base ggsurvplot with risk table ---
ggsurv_combined <- ggsurvplot(
  fit_combined,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = FALSE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 66),
  ylim = c(0, 1),
  risk.table.height = 0.25,   # Make risk table bigger
  risk.table.title = "Number at Risk",
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  legend.labs = c(
    "Cemiplimab",
    "Pembrolizumab"
  ),
  legend.title = ""
)


# Extract ggplot
p <- ggsurv_combined$plot

# Define time points
time_points <- c(12, 24, 36)

# Summarize survival
sumsurv <- summary(fit_combined, times = time_points)

# Labels
surv_probs <- sumsurv$surv
surv_labels <- paste0(round(surv_probs * 100, 1), "%")

# Annot data frame
annot_df <- tibble(
  time = rep(time_points, length(unique(rfs_data_combined$trt))),
  y = surv_probs,
  label = surv_labels,
  group_raw = sumsurv$strata
) %>%
  mutate(
    time = time + 0.75,
    vjust = -0.5,
    # Clean label for coloring only
    group = case_when(
      grepl("Cemiplimab", group_raw) ~ "Cemiplimab",
      grepl("Pembrolizumab", group_raw) ~ "Pembrolizumab"
    )
  )


# Add vertical lines + annotations
p <- p +
  geom_vline(
    xintercept = time_points,
    linetype = "dashed",
    color = "black"
  ) +
  geom_text(
    data = annot_df,
    aes(
      x = time,
      y = y,
      label = label,
      color = group
    ),
    size = 4.2,
    fontface = "bold",
    hjust = 0,
    vjust = annot_df$vjust
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "RFS: Cemiplimab vs. Pembrolizumab",
    subtitle = "(Reconstructed)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    legend.title = element_blank()
  ) 

rfs_data_combined$trt_clean <- factor(
  rfs_data_combined$trt_clean,
  levels = c("Pembrolizumab", "Cemiplimab")
)

# Fit Cox model
cox_model <- coxph(Surv(time, status) ~ trt_clean, data = rfs_data_combined)

# Get HR and CI
hr_summary <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# Extract HR and CI as nicely formatted string
hr_text <- glue::glue(
  "HR: {round(hr_summary$estimate, 2)} ",
  "(95% CI: {round(hr_summary$conf.low, 2)}–{round(hr_summary$conf.high, 2)})"
)
# Add the annotation to your plot `p`
p <- p +
  annotate(
    "text",
    x = Inf,          # right edge
    y = -Inf,         # bottom edge
    label = hr_text,
    hjust = 1,        # right align
    vjust = -1,       # push below axis a bit
    size = 4.5,
    fontface = "bold"
  )


# Update the plot in the ggsurv object
ggsurv_combined$plot <- p

# Print final combined plot
print(ggsurv_combined)

save_files(
  save_object = ggsurv_combined,
  filename = "KM Plot of Cemi vs Pembro",
  subD = "img/reconstructed KM curves/Cemi vs Pembro"
)




#------------------------------
# CPOST Only
# Load Data
dir <- here::here("files/reconstructed survival tables")
# CPOST RFS
subD <- paste("CPOST","rfs", sep = "/")
# CPOST Placebo
drug <- "placebo"
file_path <- file.path(dir, subD, drug)
file_path
file.exists(file_path)
cpost_placebo_rfs <- open_recent_file(
  directory = file_path
) 
nrow(cpost_placebo_rfs)
# add trt
cpost_placebo_rfs <- cpost_placebo_rfs |> mutate(trt = "Placebo")
print(cpost_placebo_rfs, n = 206)
# CPOST Cemiplimab
drug <- "cemiplimab"
file_path <- file.path(dir, subD, drug)
file_path
cpost_cemiplimab_rfs <- open_recent_file(
  directory = file_path
)
nrow(cpost_cemiplimab_rfs)
# add trt
cpost_cemiplimab_rfs <- cpost_cemiplimab_rfs |> mutate(trt = "Cemiplimab")
print(cpost_cemiplimab_rfs, n = 209)


# Combine data sets
rfs_data_combined <- rbind(
  cpost_placebo_rfs,
  cpost_cemiplimab_rfs
)
nrow(rfs_data_combined)

# Recode trt to clean names
rfs_data_combined <- rfs_data_combined %>%
  mutate(
    trt_clean = recode(trt,
                       "Cemiplimab" = "Cemiplimab",
                       "Placebo" = "Placebo",
    )
  )


# Combine data: `rfs_data_combined` already exists
# It must have at least:
# - time
# - status
# - trt

# Fit the survival model stratified by treatment
fit_combined <- survfit(Surv(time, status) ~ trt_clean, data = rfs_data_combined)
fit_combined_sum <- summary(fit_combined)

# --- Create base ggsurvplot with risk table ---
ggsurv_combined <- ggsurvplot(
  fit_combined,
  conf.int = TRUE,
  risk.table = TRUE,
  pval = FALSE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 66),
  ylim = c(0, 1),
  risk.table.height = 0.25,   # Make risk table bigger
  risk.table.title = "Number at Risk",
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  legend.labs = c(
    "Cemiplimab",
    "Placebo"),
  legend.title = ""
)


# Extract ggplot
p <- ggsurv_combined$plot

# Define time points
time_points <- c(12, 24, 36)

# Summarize survival
sumsurv <- summary(fit_combined, times = time_points)

# Labels
surv_probs <- sumsurv$surv
surv_labels <- paste0(round(surv_probs * 100, 1), "%")

# Annot data frame
annot_df <- tibble(
  time = rep(time_points, length(unique(rfs_data_combined$trt))),
  y = surv_probs,
  label = surv_labels,
  group_raw = sumsurv$strata
) %>%
  mutate(
    time = time + 0.75,
    vjust =  -0.5,
    # Clean label for coloring only
    group = case_when(
      grepl("Cemiplimab", group_raw) ~ "Cemiplimab",
      grepl("Placebo", group_raw) ~ "Placebo"
    )
  )

# Compute end points
# Create a tidy data frame of survival steps
# Create a tidy data frame of survival steps
fit_df <- broom::tidy(fit_combined)

# For each group, take the last time point
end_labels <- fit_df %>%
  group_by(strata) %>%
  filter(time == max(time)) %>%
  mutate(
    label = case_when(
      grepl("Cemiplimab", strata) ~ "Cemiplimab",
      grepl("Placebo", strata) ~ "Placebo"
    ),
    color = case_when(
      label == "Cemiplimab" ~ "#E41A1C",
      label == "Placebo" ~ "#377EB8"
    )
  ) %>%
  select(time, estimate, label, color)


# Add vertical lines + annotations
p <- p +
  geom_vline(
    xintercept = time_points,
    linetype = "dashed",
    color = "black"
  ) +
  geom_text(
    data = annot_df,
    aes(
      x = time,
      y = y,
      label = label,
      color = group
    ),
    size = 4.2,
    fontface = "bold",
    hjust = 0,
    vjust = annot_df$vjust
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "CPOST Recurrence-Free Survival",
    subtitle = "(Reconstructed)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold"
    ),
    legend.title = element_blank()
  ) 

# Remove the legend
p <- p + theme(legend.position = "none")

# Add text annotations
p <- p + geom_text(
  data = end_labels,
  mapping = aes(
    x = time,
    y = estimate + 0.02,
    label = label,
    color = label  # map to label so color follows palette
  ),
  hjust = 0.5,         # centered horizontally
  vjust = -0.5,        # above
  fontface = "bold",
  size = 5
)
rfs_data_combined$trt_clean <- factor(
  rfs_data_combined$trt_clean,
  levels = c("Placebo", "Cemiplimab")
)

# Fit Cox model
cox_model <- coxph(Surv(time, status) ~ trt_clean, data = rfs_data_combined)

# Get HR and CI
hr_summary <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# Extract HR and CI as nicely formatted string
hr_text <- glue::glue(
  "HR: {round(hr_summary$estimate, 2)} ",
  "(95% CI: {round(hr_summary$conf.low, 2)}–{round(hr_summary$conf.high, 2)})"
)
# Add the annotation to your plot `p`
p <- p +
  annotate(
    "text",
    x = Inf,          # right edge
    y = -Inf,         # bottom edge
    label = hr_text,
    hjust = 1,        # right align
    vjust = -1,       # push below axis a bit
    size = 4.5,
    fontface = "bold"
  )



# Update the plot in the ggsurv object
ggsurv_combined$plot <- p

# Print final combined plot
print(ggsurv_combined)