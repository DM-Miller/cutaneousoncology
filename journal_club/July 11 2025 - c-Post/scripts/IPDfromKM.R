library(IPDfromKM)
library(tidyverse)


#pembro_data <- read.csv("pembro_curve.csv")
placebo_data <- read.csv("files/webplot digitizer/KN630_rfs_placebo_cleaned.csv")
placebo_data_6 <- read_csv(
  "files/webplot digitizer/KN630_rfs_placebo_6_months.csv",
  col_names = FALSE
  )
placebo_data_6
names(placebo_data_6) <- c("Time", "Survival")
placebo_data_6
names(placebo_data) <- c("Time", "Survival")

placebo_data_18 <- placebo_data |> filter(Time < 18)
nrow(placebo_data_18)

# Specify the number at risk
#pembro_n_risk <- c(225, 172, 119, 73, 22, 7, 1)
placebo_n_risk <- c(225, 169, 124, 101, 80, 58, 43, 33, 16, 7, 1)
placebo_n_risk_18 <- c(225, 169, 124, 101) 
placebo_n_risk_6 <- c(225,220,210,200,190,180,170)
placebo_n_risk_6
# Time intervals where number at risk is reported
time_points <- c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60)
time_points_18 <- c(0, 6, 12, 18) 
time_points_6 <- c(0.5,1,2,3,4,5,6)


plot(placebo_data_6$Time, placebo_data_6$Survival, type="l")

# Preprocess
prep_placebo <- preprocess(
  dat = placebo_data,
  trisk = time_points,
  nrisk = placebo_n_risk,
  maxy = 100
)
prep_placebo_18 <- preprocess(
  dat = placebo_data_18,
  trisk = time_points_18,
  nrisk = placebo_n_risk_18,
  maxy = 100
)
prep_placebo_6 <- preprocess(
  dat = placebo_data_6,
  trisk = time_points_6,
  nrisk = placebo_n_risk_6,
  maxy = 100
)

# Reconstruct IPD
ipd_placebo <- getIPD(
  prep = prep_placebo,
  armID = 0,
  tot.events = 57
)
ipd_placebo_6 <- getIPD(
  prep = prep_placebo_6,
  armID = 0,
  tot.events = 30
)
ipd_placebo_18 <- getIPD(
  prep = prep_placebo_18,
  armID = 0,
  tot.events = 43
)



# Inspect reconstructed IPD
head(ipd_placebo$IPD)
sum(ipd_placebo$Points$censor)
sum(ipd_placebo$Points$event)
# Optional: Plot reconstructed KM vs digitized KM
plot(ipd_placebo)

# Optional: Summary report
summary(ipd_placebo)


# Inspect reconstructed IPD
head(ipd_placebo_6$IPD)
summary(ipd_placebo_6)

ipd_placebo_6$IPD
sum(ipd_placebo_6$Points$censor)
sum(ipd_placebo_6$Points$event)
# Optional: Plot reconstructed KM vs digitized KM
plot(ipd_placebo_6)

# Optional: Summary report
summary(ipd_placebo_6)

# Inspect reconstructed IPD
head(ipd_placebo_18$IPD)
placebo_data_6
ipd_placebo_18$IPD
sum(ipd_placebo_18$Points$censor)
sum(ipd_placebo_18$Points$event)
# Optional: Plot reconstructed KM vs digitized KM
plot(ipd_placebo_18)

# Optional: Summary report
summary(ipd_placebo_18)


# Load necessary libraries
library(survival)
library(survminer)
library(dplyr)

# If you haven't already, extract your reconstructed IPD
placebo_ipd_df <- ipd_placebo$IPD

# Confirm the data format
head(placebo_ipd_df)
# Should have columns: time, status, treat

# Create a Surv object
km_fit <- survfit(Surv(time, status) ~ 1, data = placebo_ipd_df)

# Make a ggplot with survminer
ggsurv <- ggsurvplot(
  km_fit,
  data = placebo_ipd_df,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.title = "Number at risk",
  risk.table.y.text.col = TRUE,
  risk.table.y.text = FALSE,
  xlab = "Time (Months)",
  ylab = "Recurrence-Free Survival Probability",
  break.time.by = 6,
  surv.scale = "percent",
  ggtheme = theme_minimal(base_size = 14),
  palette = "darkred"
)

# Print the plot
print(ggsurv)

#--------------------------------------
# Interval 6-12 months
# There are 45 events/censors from 169-124
## Maybe 15 events and 30 censors
# expected survival should drop from 85.3 to 78.5

event_times1 <- 7
event_times2 <- runif(9, min = 8, max = 9)
event_times3 <- 9.5
event_times4 <- runif(4, min = 11, max = 11.6)
event_times <- c(event_times1, event_times2, event_times3, event_times4)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(15, min=8, max=8.9)
censor_times2 <- 9.7
censor_times3 <- runif(14, min = 11, max = 11.9)
censor_times <- c(censor_times1, censor_times2, censor_times3)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- rbind(
  df_events,
  df_censor
)

# Sort
df_raw <- df_raw %>% arrange(time)

# Aggregate events and censorings
agg <- df_raw %>%
  group_by(time) %>%
  summarise(
    events = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  ) %>%
  arrange(time)

print(agg)

# Prepare vectors
time_points <- agg$time
events <- agg$events
censored <- agg$censored

# Set initial n and starting survival (adjust to your context)
initial_n <- 169
starting_survival <- 0.853

# Cumulative counts
cum_events <- cumsum(events)
cum_censored <- cumsum(censored)

# Compute risk sets
n_risk_start <- initial_n - c(0, head(cum_events + cum_censored, -1))

# Initialize storage vectors
decrements <- numeric(length(events))
cumulative_proportions <- numeric(length(events))
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop over time points
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Combine into data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points) - 0.5,
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Review
print(df_plot)

ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)

df_interval_6_12 <- df_raw
nrow(df_interval_6_12)
# Sort by time for clarity
df_interval_6_12 <- df_interval_6_12[order(df_interval_6_12$time), ]
nrow(df_interval_6_12)
# Preview
head(df_interval_6_12, 10)

#--------------------------------------
# Interval 12-18
# There are 23 events/censors from 124-101
## 9 events and 14 censors
# expected survival should drop from 78.5 to 72

event_times <- c(12.1, 12.2, 13, 13.2, 14, 14.1, 15, 15.1, 17.8) # n = 9 
df_events <- data.frame(
  time = event_times,
  status = 1
)
censor_times <- runif(14, min=13.1, max=15)
df_censor <- data.frame(
  time = censor_times,
  status = 0
)

df_raw <- rbind(
  df_events,
  df_censor
)

# Sort
df_raw <- df_raw %>% arrange(time)

# Aggregate events and censorings
agg <- df_raw %>%
  group_by(time) %>%
  summarise(
    events = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  ) %>%
  arrange(time)

print(agg)

# Prepare vectors
time_points <- agg$time
events <- agg$events
censored <- agg$censored

# Set initial n and starting survival (adjust to your context)
initial_n <- 124
starting_survival <- 0.785

# Cumulative counts
cum_events <- cumsum(events)
cum_censored <- cumsum(censored)

# Compute risk sets
n_risk_start <- initial_n - c(0, head(cum_events + cum_censored, -1))

# Initialize storage vectors
decrements <- numeric(length(events))
cumulative_proportions <- numeric(length(events))
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop over time points
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Combine into data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points) - 0.5,
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Review
print(df_plot)

ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)

df_interval_12_18 <- df_raw
nrow(df_interval_12_18)
# Sort by time for clarity
df_interval_12_18 <- df_interval_12_18[order(df_interval_12_18$time), ]
nrow(df_interval_12_18)
# Preview
head(df_interval_12_18, 10)


#--------------------------------------
# Interval from 18 - 24 months

# Assume initial number at risk
initial_n <- 101
starting_survival <- 0.72

# Recreate your raw data # 5 events and 16 censors
time <- c(
  18.1, #1 event
  18.5, #1 censor
  19.1, #1 event
  19.2, 19.3, 19.4, 19.5, 19.6, 20.9, # 6 censors
  21, #1 event,
  22,  #1 censor
  22.1,  #1 event
  22.2, 22.3, 22.4, #3 censors
  22.6, #1 event,
  22.7, 22.75, 23, 23.01, 23.5 #5 censors
  )
length(time)
status <- c(
  1, 
  0,
  1,
  0,0,0,0,0,0, #6 censors
  1,
  0,
  1,
  0,0,0,
  1,
  0,0,0,0,0
)
length(status)


# Convert to data frame
df_raw <- data.frame(time=time, status=status)

# Use dplyr to group and summarize
agg <- df_raw %>%
  group_by(time) %>%
  summarise(
    events = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  ) %>%
  arrange(time)

print(agg)

# Expand counts
time_points <- agg$time
time_points
events <- agg$events
events
censored <- agg$censored
censored

cum_events <- cumsum(events)
cum_events
cum_censored <- cumsum(censored)
cum_censored

n_risk_start <- initial_n - c(0, head(cum_events + cum_censored, -1))
n_risk_start

print(data.frame(time=time_points, n_risk_start=n_risk_start, events=events, censored=censored))


# Initialize storage
decrements <- numeric(length(events))
decrements
cumulative_proportions <- numeric(length(events))
cumulative_proportions
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Create main data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points)-0.5,  # to show clearly before first event
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Check
print(df_plot)


ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)


# Build data frame
df_interval_18_24 <- data.frame(
  time = time,
  status = status
)

print(df_interval_18_24)
#--------------------------------------
# Interval from 24 - 30 months

# Assume initial number at risk
initial_n <- 80
starting_survival <- 0.686

# Recreate your raw data
time <- c(
  24.4,24.45,24.46,24.48,  #4 censored
  24.5,                    #1 event
  24.6,24.65,24.66,24.66,24.67,24.7,24.75,24.8,24.9,  #9 censored
  25,                      #1 event
  27,27.2,27.5,27.7,27.8,  #5 censored
  28,                      #1 event
  29                       #1 event
)
status <- c(
  0,0,0,0,
  1,
  0,0,0,0,0,0,0,0,0,
  1,
  0,0,0,0,0,
  1,
  1
)

# Convert to data frame
df_raw <- data.frame(time=time, status=status)

# Use dplyr to group and summarize
agg <- df_raw %>%
  group_by(time) %>%
  summarise(
    events = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  ) %>%
  arrange(time)

print(agg)

# Expand counts
time_points <- agg$time
time_points
events <- agg$events
events
censored <- agg$censored
censored

# Keep relevant columns
initial_n <- 80

cum_events <- cumsum(events)
cum_events
cum_censored <- cumsum(censored)
cum_censored

n_risk_start <- initial_n - c(0, head(cum_events + cum_censored, -1))
n_risk_start

print(data.frame(time=time_points, n_risk_start=n_risk_start, events=events, censored=censored))


# Initialize storage
decrements <- numeric(length(events))
decrements
cumulative_proportions <- numeric(length(events))
cumulative_proportions
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Create main data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points)-0.5,  # to show clearly before first event
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Check
print(df_plot)


ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)

# Create vectors row by row
time <- c(
  24.4, 24.45, 24.46, 24.48, #4 before the first event
  24.6, 24.65, 24.66, 24.66, 24.67, 24.7, 24.75, 24.8, 24.9, #9 after the first event
  27.0, 27.2, 27.5, 27.7, 27.8,  # 5 censorings after the second event
  24.5, 25, 28, 29         # 4 events (assuming last time has 2 simultaneous events)
)

status <- c(
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0, 0, # 18 censored
  1,1,1,1     # 4 events
)


# Build data frame
df_interval_24_30 <- data.frame(
  time = time,
  status = status
)

print(df_interval_24_30)

#--------------------------------------
# Interval 30-36
event_time <- 30.65
set.seed(1)
censor_time1 <- 30.6
censor_times <- runif(13, min=30.66, max=36)
length(censor_times)
time <- c(censor_time1, event_time, censor_times)
length(time)
status <- c(
  0,        # censor_time1
  1,        # event_time
  rep(0, length(censor_times))
)
length(status)

df_raw <- data.frame(
  time = time,
  status = status
)

# Sort
df_raw <- df_raw %>% arrange(time)

# Aggregate events and censorings
agg <- df_raw %>%
  group_by(time) %>%
  summarise(
    events = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  ) %>%
  arrange(time)

print(agg)

# Prepare vectors
time_points <- agg$time
events <- agg$events
censored <- agg$censored

# Set initial n and starting survival (adjust to your context)
initial_n <- 58
starting_survival <- 0.642

# Cumulative counts
cum_events <- cumsum(events)
cum_censored <- cumsum(censored)

# Compute risk sets
n_risk_start <- initial_n - c(0, head(cum_events + cum_censored, -1))

# Initialize storage vectors
decrements <- numeric(length(events))
cumulative_proportions <- numeric(length(events))
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop over time points
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Combine into data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points) - 0.5,
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Review
print(df_plot)

ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)

df_interval_30_36 <- data.frame(
  time = time,
  status = status
)
nrow(df_interval_30_36)
# Sort by time for clarity
df_interval_30_36 <- df_interval_30_36[order(df_interval_30_36$time), ]
nrow(df_interval_30_36)
# Preview
head(df_interval_30_36, 10)

#--------------------------------------
# Interval 36-42
set.seed(1)
censor_times <- runif(10, min=36.01, max=42)
status <- c(
  rep(0, length(censor_times))
)

df_interval_36_42 <- data.frame(
  time = censor_times ,
  status = status
)

# Sort by time for clarity
df_interval_36_42 <- df_interval_36_42[order(df_interval_36_42$time), ]

# Preview
head(df_interval_36_42, 10)

#--------------------------------------
# Interval 42-48
set.seed(1)
censor_times <- runif(17, min=42.01, max=48)
status <- c(
  rep(0, length(censor_times))
)

df_interval_42_48 <- data.frame(
  time = censor_times,
  status = status
)

# Sort by time for clarity
df_interval_42_48 <- df_interval_42_48[order(df_interval_42_48$time), ]

# Preview
head(df_interval_42_48, 10)
#--------------------------------------

# Interval from 48 - 54 months

# Example vectors
events <-       c(0, 0, 1, 1, 1)
censored <-     c(0, 6, 0, 0, 0)
n_risk_start <- c(16, 16, 10, 9, 8)
time_points <-  c(48, 49, 51.6, 52.0, 53.0)  # <<-- your actual times
starting_survival <- 0.63

# Initialize storage
decrements <- numeric(length(events))
cumulative_proportions <- numeric(length(events))
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Create main data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points)-0.5,  # to show clearly before first event
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Check
print(df_plot)

library(ggplot2)

ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)

# Create vectors row by row
time <- c(
  49.0, 49.0, 49.0, 49.1, 49.2,  # 5 censorings
  51.6, 52.0, 53.0, 53.0         # 4 events (assuming last time has 2 simultaneous events)
)

status <- c(
  0,0,0,0,0,  # 5 censored
  1,1,1,1     # 4 events
)


# Build data frame
df_interval_48_54 <- data.frame(
  time = time,
  status = status
)

print(df_interval_48_54)



# Interval from 48 - 54 months

# Example vectors
events <-       c(0, 0, 0, 0, 0, 0, 0)
censored <-     c(0, 1, 1, 1, 1, 1, 1)
n_risk_start <- c(7, 7, 6, 5, 4, 3, 2)
time_points <-  c(54, 54.1, 55, 55.1, 55.2, 57.5, 60)  # <<-- your actual times
starting_survival <- 0.44

# Initialize storage
decrements <- numeric(length(events))
cumulative_proportions <- numeric(length(events))
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Create main data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points)-0.5,  # to show clearly before first event
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Check
print(df_plot)

library(ggplot2)

ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)


# Interval 54 to 60
# Create vectors row by row
time <- c(
  54.1, 55, 55.1, 55.2, 57.5, 60   # 6 censorings

)

status <- c(
  0,0,0,0,0,0  # 6 censored
              # 0 events
)

# Build data frame
df_interval_54_60 <- data.frame(
  time = time,
  status = status
)

print(df_interval_54_60)


# Combine data frames
df_combined <- rbind(
  df_interval_6_12,
  df_interval_12_18,
  df_interval_18_24,
  df_interval_24_30,
  df_interval_30_36,
  df_interval_36_42,
  df_interval_42_48,
  df_interval_48_54,
  df_interval_54_60
)

# Sort by time
df_combined <- df_combined[order(df_combined$time), ]

sum(df_combined$status)
nrow(df_combined)
sum(df_combined$status == 0)
# Inspect
print(head(df_combined, 10))
library(survival)
fit <- survfit(Surv(time, status) ~ 1, data = df_combined)
library(survminer)

ggsurv <- ggsurvplot(
  fit,
  conf.int = FALSE,
  risk.table = TRUE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 6,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent"
)

print(ggsurv)


# Manual extraction
# Sort
df_raw <- df_combined %>% arrange(time)
nrow(df_raw)
# Aggregate events and censorings
agg <- df_raw %>%
  group_by(time) %>%
  summarise(
    events = sum(status == 1),
    censored = sum(status == 0),
    .groups = "drop"
  ) %>%
  arrange(time)

print(agg, n = 151)

# Prepare vectors
time_points <- agg$time
events <- agg$events
censored <- agg$censored

# Set initial n and starting survival (adjust to your context)
initial_n <- 169
starting_survival <- 0.853

# Cumulative counts
cum_events <- cumsum(events)
cum_censored <- cumsum(censored)

# Compute risk sets
n_risk_start <- initial_n - c(0, head(cum_events + cum_censored, -1))

# Initialize storage vectors
decrements <- numeric(length(events))
cumulative_proportions <- numeric(length(events))
survival_estimates <- numeric(length(events))
hazard_increments <- numeric(length(events))
cumulative_hazard <- numeric(length(events))

# Loop over time points
current_survival <- 1
current_hazard <- 0

for (i in seq_along(events)) {
  n_risk_adj <- n_risk_start[i] - censored[i]
  if (n_risk_adj <= 0) stop("Risk set cannot be zero or negative after censoring.")
  
  decrement <- 1 - (events[i] / n_risk_adj)
  decrements[i] <- decrement
  
  current_survival <- current_survival * decrement
  cumulative_proportions[i] <- current_survival
  
  hazard_increment <- -log(decrement)
  hazard_increments[i] <- hazard_increment
  current_hazard <- current_hazard + hazard_increment
  cumulative_hazard[i] <- current_hazard
  
  survival_estimates[i] <- starting_survival * current_survival
}

# Combine into data frame
df <- data.frame(
  Time = time_points,
  Events = events,
  Censored = censored,
  Risk_Set_Start = n_risk_start,
  Risk_Set_Adjusted = n_risk_start - censored,
  Incremental_Decrement = round(decrements,4),
  Cumulative_Proportion = round(cumulative_proportions,4),
  Survival_Estimate = round(survival_estimates,4),
  Incremental_Hazard = round(hazard_increments,4),
  Cumulative_Hazard = round(cumulative_hazard,4)
)

# Add baseline row
baseline <- data.frame(
  Time = min(time_points) - 0.5,
  Events = 0,
  Censored = 0,
  Risk_Set_Start = NA,
  Risk_Set_Adjusted = NA,
  Incremental_Decrement = NA,
  Cumulative_Proportion = 1,
  Survival_Estimate = starting_survival,
  Incremental_Hazard = 0,
  Cumulative_Hazard = 0
)

# Combine
df_plot <- rbind(baseline, df)

# Review
print(df_plot)

ggplot(df_plot, aes(x = Time, y = Survival_Estimate)) +
  geom_step(color = "firebrick", size = 1.2, direction = "hv") +
  geom_point(aes(size = Events), color = "firebrick", alpha = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        Events == 0 & Censored == 0,
        paste0("Start\nS=", round(Survival_Estimate,2)),
        paste0("E", Events, "\nC", Censored)
      )
    ),
    vjust = -1.2,
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Stepwise Survival Estimate with Events and Censoring",
    x = "Time (Months)",
    y = "Estimated Survival Probability"
  ) +
  theme_minimal(base_size = 14)
