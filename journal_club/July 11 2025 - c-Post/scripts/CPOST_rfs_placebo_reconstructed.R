# Load Packages
library(tidyverse)
library(survminer)
library(survival)
source(here::here("scripts", "save files.R"))

#--------------------------------------
# Number at risk at beginning is 206, Number of events in total 65, thus there 141 censored events
# Interval 0-4 months
# There are 45 events/censors from 206-161
## Maybe 19 events and 26 censors
# expected survival should drop from 100 to 89
# Set initial n and starting survival (adjust to your context)
initial_n <- 206
starting_survival <- 1

set.seed(123)
event_times1 <- 0.3
event_times2 <- 1.8
event_times3 <- c(2.0,2.0)
event_times4 <- runif(15, min = 2.1, max = 4.0)
event_times <- c(event_times1, event_times2, event_times3, event_times4)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(2, min = 0.1, max = 0.1)
censor_times2 <- runif(24, min = 2.5, max = 2.8)
censor_times <- c(censor_times1, censor_times2)
df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- rbind(
  df_events,
  df_censor
)
nrow(df_raw)

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

df_interval_0_4 <- df_raw |> arrange(time)
nrow(df_interval_0_4)


#--------------------------------------
# Interval 4-8 months
# There are 31 events/censors from 161-130
## Maybe 13 events and 18 censors
# expected survival should drop from 88 to 72
# Set initial n and starting survival (adjust to your context)
initial_n <- 161
starting_survival <- 0.88

event_times1 <- 4.1
event_times2 <- runif(6, min = 4.4, max = 5.4)
event_times3 <- runif(6, min = 6.0, max = 7.5)
event_times <- c(event_times1, event_times2, event_times3)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(17, min=4.7, max=5.4)
censor_times2 <- 6.0
censor_times <- c(censor_times1, censor_times2)

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

print(agg)

# Prepare vectors
time_points <- agg$time
events <- agg$events
censored <- agg$censored

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


df_interval_4_8 <- df_raw |> arrange(time)
nrow(df_interval_4_8)
# Preview
head(df_interval_4_8, 10)

#--------------------------------------
# Interval 8-12
# There are 36 events/censors from 130-94
## 18 events and 18 censors
# expected survival should drop from 78 to 69.5
# Set initial n and starting survival (adjust to your context)
initial_n <- 130
starting_survival <- 0.78

event_times <- runif(18, min = 9, max = 11.8)
df_events <- data.frame(
  time = event_times,
  status = 1
  )
censor_times1 <- runif(9, min=8.1, max=8.9)
censor_times2 <- 8.9
censor_times3 <- runif(8, min = 10, max = 11.8)
censor_times <- c(censor_times1, censor_times2, censor_times3)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)

df_raw <- rbind(
  df_events,
  df_censor
)
nrow(df_raw)
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

df_interval_8_12 <- df_raw |> arrange(time)
nrow(df_interval_8_12)

#--------------------------------------
# Interval from 12 - 16 months
# There are 12 events/censors from 94-82
## 2 events and 10 censors
# expected survival should drop from 69.5 to 67
# Assume initial number at risk
initial_n <- 94
starting_survival <- 0.695

event_times1 <- 13.4
event_times2 <- 14
event_times <- c(event_times1, event_times2)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)

# censoring
censor_times1 <- runif(10, min = 13.6, max = 15)
censor_times <- c(censor_times1)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- rbind(
  df_events,
  df_censor
)
nrow(df_raw)
# Sort
df_raw <- df_raw %>% arrange(time)

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

# Build data frame
df_interval_12_16 <- df_raw |> arrange(time)

print(df_interval_12_16)

#--------------------------------------
# Interval from 16 - 20 months
# There are 13 events/censors from 82-69
## 2 events and 11 censors
# expected survival should drop from 68 to 67
# Assume initial number at risk
initial_n <- 82
starting_survival <- 0.68

event_times <- c(17, 18.5)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)

# censoring
censor_times1 <- runif(10, min=17.1, max=18.7)
censor_times2 <- 19.5
censor_times <- c(censor_times1, censor_times2)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- rbind(
  df_events,
  df_censor
)
nrow(df_raw)
# Sort
df_raw <- df_raw %>% arrange(time)

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

# Build data frame
df_interval_16_20 <- df_raw |> arrange(time)


print(df_interval_16_20)

#--------------------------------------
# Interval 20-24
# There are 16 events/censors from 69-53
## 2 events and 14 censors
# expected survival should drop from 66 to 64.1
# Assume initial number at risk
initial_n <- 69
starting_survival <- 0.66

event_times <- c(21.5, 22)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(10, min=20.5, max=22.1)
censor_times2 <- runif(4, min=23.1, max=23.8)
censor_times <- c(censor_times1, censor_times2)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- rbind(
  df_events,
  df_censor
)
nrow(df_raw)
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

df_interval_20_24 <- df_raw |> arrange(time)
nrow(df_interval_20_24)

#--------------------------------------
# Interval 24-28
# There are 11 events/censors from 53-42
## 1 events and 10 censors
# expected survival should drop from 64.1 to 62
# Assume initial number at risk
initial_n <- 53
starting_survival <- 0.641

event_times <- c(27.5)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)

# censoring
censor_times1 <- runif(9, min=24.9, max=27.2)
censor_times2 <- c(27.9)
censor_times <- c(censor_times1, censor_times2)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- rbind(
  df_events,
  df_censor
)
nrow(df_raw)
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

df_interval_24_28 <- df_raw |> arrange(time)
nrow(df_interval_24_28)

#--------------------------------------
# Interval 28-32
# There are 6 events/censors from 42-36
## 0 events and 6 censors
# expected survival should drop from 62-62
# Assume initial number at risk
initial_n <- 36
starting_survival <- 0.62

#event_times <- 30
#df_events <- data.frame(
#  time = event_times,
#  status = 1
#)

# censoring
censor_times1 <- runif(6, min=29, max=31.5)
#censor_times2 <- runif(6, min=30.1, max=31.6)
censor_times <- c(censor_times1)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <-   df_censor
nrow(df_raw)
# Sort
df_raw <- df_raw %>% arrange(time)

df_interval_28_32 <- df_raw

print(df_interval_28_32)

#--------------------------------------
# Interval from 32 - 36 months
# There are 10 events/censors from 36-26
## 1 events and 9 censors
# expected survival should drop from 62 to 60.4
# Assume initial number at risk
initial_n <- 36
starting_survival <- 0.62

event_times <- c(34)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(8, min=32.5, max=33.4)
censor_times2 <- 35
censor_times <- c(censor_times1, censor_times2)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- rbind(
  df_events,
  df_censor
  )

nrow(df_raw)

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

# Cumulative counts
cum_events <- cumsum(events)
cum_censored <- cumsum(censored)

# Compute risk sets
n_risk_start <- initial_n - c(0, head(cum_events + cum_censored, -1))


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

# Build data frame
df_interval_32_36 <- df_raw |> arrange(time)

print(df_interval_32_36)


#-----------------------------
# Interval from 36-60 months
# There are 26 events/censors from 26-0
## 2 events and 22 censors
# expected survival should drop from 60.4 to 48%
# Assume initial number at risk
initial_n <- 24
starting_survival <- 0.604

event_times <- c(48.5, 49.5)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)

# censoring
censor_times1 <- runif(2, min=36, max=40)
censor_times2 <- runif(6, min = 40.1, max = 44)
censor_times3 <- runif(8, min=44.1, max=48)
censor_times4 <- runif(2, min = 48.9, max = 49)
censor_times5 <- runif(2, min = 48.1, max = 52)
censor_times6 <- runif(2, min = 52.1, max = 56)
censor_times7 <- runif(2, min = 56.2, max = 58)
censor_times <- c(
  censor_times1, censor_times2, censor_times3, censor_times4, censor_times5, censor_times6, censor_times7
  )
df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <-   rbind(
  df_events,
  df_censor
)
nrow(df_raw)
# Sort
df_raw <- df_raw %>% arrange(time)

df_interval_36_60 <- df_raw |> arrange(time)

print(df_interval_36_60)

#-------------------------
# Combine data frames
df_combined <- rbind(
  df_interval_0_4,
  df_interval_4_8,
  df_interval_8_12,
  df_interval_12_16,
  df_interval_16_20,
  df_interval_20_24,
  df_interval_24_28,
  df_interval_28_32,
  df_interval_32_36,
  df_interval_36_60
)

# Sort by time
df_combined <- df_combined |> arrange(time)

sum(df_combined$status)
nrow(df_combined)
sum(df_combined$status == 0)
# Inspect
print(head(df_combined, 10))

fit <- survfit(Surv(time, status) ~ 1, data = df_combined)
fit
summary(fit)$n.risk
summary(fit)$time

sumtab <- summary(fit)
df_sumtab <- data.frame(
  time = sumtab$time,
  n.risk = sumtab$n.risk,
  n.event = sumtab$n.event,
  n.censor = sumtab$n.censor,
  surv = sumtab$surv
)
ggsurv <- ggsurvplot(
  fit,
  conf.int = FALSE,
  risk.table = TRUE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent"
)

print(ggsurv)
adjust_specific_times <- function(
    df,
    times,
    offsets,
    time_col = "time",
    tolerance = 1e-5
) {
  # Safety checks
  if (length(times) != length(offsets)) {
    stop("times and offsets must be the same length.")
  }
  
  df_new <- df
  
  # For tracking
  adjusted_rows <- list()
  
  for (i in seq_along(times)) {
    diffs <- abs(df_new[[time_col]] - times[i])
    idx <- which(diffs < tolerance)
    
    if (length(idx) == 0) {
      warning(sprintf(
        "Time %s not found within tolerance %g in %s column.",
        times[i], tolerance, time_col
      ))
    } else {
      df_new[idx, time_col] <- df_new[idx, time_col] + offsets[i]
      adjusted_rows[[i]] <- df_new[idx, ]
      message(sprintf(
        "Adjusted %d row(s) where %s approx. %s by %+g.",
        length(idx), time_col, times[i], offsets[i]
      ))
    }
  }
  
  return(df_new)
}
# adjust times to match number at risk
df_combined_adj <- adjust_specific_times(
  df = df_combined,
  times = c(
    15.202500, 23.987060),
  offsets = c(
    .8, .02
    )
)


fit_adj <- survfit(Surv(time, status) ~ 1, data = df_combined_adj)

ggsurv_adj <- ggsurvplot(
  fit_adj,
  conf.int = FALSE,
  risk.table = TRUE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 4,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 60),          # Extend x-axis
  ylim = c(0, 1),           # Force y to 0–1
)

# Manually set y-axis breaks every 10%
ggsurv_adj$plot <- ggsurv_adj$plot +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  )
print(ggsurv_adj)


save_files(
  save_object = df_combined_adj,
  filename = "CPOST Placebo Survival Table Reconstructed",
  subD = "files/reconstructed survival tables/CPOST/rfs/placebo",
  extension = ".csv"
)

# First, extract the ggplot object
p <- ggsurv_adj$plot

sumsurv <- summary(fit_adj, times = c(12, 24, 36))
surv_probs <- sumsurv$surv
surv_probs
surv_labels <- paste0(round(surv_probs * 100, 1), "%")

# Modify the internal ggplot object
ggsurv_adj$plot <- ggsurv_adj$plot +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  geom_vline(xintercept = c(12, 24, 36), linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = c(12, 24, 36) + 0.75,
    y = surv_probs,
    label = surv_labels,
    color = "firebrick",
    size = 5,
    fontface = "bold",
    vjust = -0.5,
    hjust = 0
  )

# Print the *whole* ggsurvplot object
print(ggsurv_adj)

sum(df_combined$status)