# Load Packages
library(tidyverse)
library(survminer)
library(survival)
source(here::here("scripts", "save files.R"))

#--------------------------------------
# Number at risk at beginning is 225, Number of events in total 68, thus there 157 censored events
# Interval 0-6 months
# There are 53 events/censors from 225-172
## Maybe 18 events and 35 censors
# expected survival should drop from 100 to 90
set.seed(123)
event_times1 <- runif(4, min = 1.8, max = 1.9)
event_times2 <- runif(4, min = 2, max = 2.9)
event_times3 <-runif(5, min = 4, max = 4.9)
event_times4 <- runif(5, min = 5, max = 5.99)
event_times <- c(event_times1, event_times2, event_times3, event_times4)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(11, min=1.5, max=1.9)
censor_times2 <- runif(8, min=2.5, max=3)
censor_times3 <- runif(11, min = 4, max = 4.5)
censor_times4 <- runif(6, min = 5.5, max = 5.9)
censor_times <- c(censor_times1, censor_times2, censor_times3, censor_times4)

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

# Set initial n and starting survival (adjust to your context)
initial_n <- 225
starting_survival <- 1

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

df_interval_0_6 <- df_raw
nrow(df_interval_0_6)
# Sort by time for clarity
df_interval_0_6 <- df_interval_0_6 |> arrange(time)
nrow(df_interval_0_6)
# Preview
head(df_interval_0_6, 10)

#--------------------------------------
# Interval 6-12 months
# There are 35 events/censors from 172-137
## Maybe 14 events and 21 censors
# expected survival should drop from 90 to 83.2

event_times1 <- 6.2
event_times2 <- runif(7, min = 8, max = 9)
event_times3 <- 9.5
event_times4 <- runif(5, min = 11, max = 11.6)
event_times <- c(event_times1, event_times2, event_times3, event_times4)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(10, min=8, max=8.9)
censor_times2 <- 9.7
censor_times3 <- runif(10, min = 11, max = 11.9)
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
initial_n <- 172
starting_survival <- 0.90

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
df_interval_6_12 <- df_interval_6_12 |> arrange(time)
nrow(df_interval_6_12)
# Preview
head(df_interval_6_12, 10)

#--------------------------------------
# Interval 12-18
# There are 18 events/censors from 137-119
## 3 events and 15 censors
# expected survival should drop from 83.2 to 80.5

event_times <- c(12.3, 12.8, 12.8) # n = 3
df_events <- data.frame(
  time = event_times,
  status = 1
)
censor_times1 <- runif(7, min=13, max=15)
censor_times2 <- 14.955021
censor_times3 <- runif(7, min=16.2, max=17.5)
censor_times <- c(censor_times1, censor_times2, censor_times3)
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
initial_n <- 137
starting_survival <- 0.832

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
# There are 24 events/censors from 119-95
## 5 events and 19 censors
# expected survival should drop from 81 to 78
# Assume initial number at risk
initial_n <- 119
starting_survival <- 0.81

event_times <- c(18.1, 19, 20, 23, 24)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(2, min=19.1, max=19.5)
censor_times2 <- runif(5, min=20.1, max=22.9)
censor_times3 <- runif(12, min = 23, max = 23.9)
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
df_interval_18_24 <- df_raw

print(df_interval_18_24)
#--------------------------------------
# Interval from 24 - 30 months
# There are 22 events/censors from 95-73
## 2 events and 20 censors
# expected survival should drop from 78.3 to 76
# Assume initial number at risk
initial_n <- 95
starting_survival <- 0.783

event_times <- c(25, 27)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(11, min=24.5, max=26.7)
censor_times2 <- runif(9, min=28, max=29)
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
df_interval_24_30 <- df_raw


print(df_interval_24_30)

#--------------------------------------
# Interval 30-36
# There are 21 events/censors from 73-52
## 5 events and 16 censors
# expected survival should drop from 76 to 69.4
# Assume initial number at risk
initial_n <- 73
starting_survival <- 0.76

event_times <- c(30.8, 31, 31.2, 32.9, 35.8)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(13, min=30.1, max=30.9)
censor_times2 <- runif(2, min=31.5, max=32.0)
censor_times3 <- 35.6
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

df_interval_30_36 <- df_raw
nrow(df_interval_30_36)
# Sort by time for clarity
df_interval_30_36 <- df_interval_30_36[order(df_interval_30_36$time), ]
nrow(df_interval_30_36)
# Preview
head(df_interval_30_36, 10)

#--------------------------------------
# Interval 36-42
# There are 19 events/censors from 52-33
## 2 events and 17 censors
# expected survival should drop from 69.4 to 66
# Assume initial number at risk
initial_n <- 52
starting_survival <- 0.694

event_times <- c(39, 39.1)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(14, min=36.1, max=38.5)
censor_times2 <- c(40.5, 41.4, 41.9)
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

df_interval_36_42 <- df_raw
nrow(df_interval_36_42)
# Sort by time for clarity
df_interval_36_42 <- df_interval_36_42[order(df_interval_36_42$time), ]
nrow(df_interval_36_42)
# Preview
head(df_interval_36_42, 10)

#--------------------------------------
# Interval 42-48
# There are 11 events/censors from 33-22
## 0 events and 11 censors
# expected survival should drop from 65
# Assume initial number at risk
initial_n <- 33
starting_survival <- 0.65


# censoring
censor_times1 <- runif(11, min=42.1, max=47.9)

df_censor <- data.frame(
  time = censor_times1,
  status = 0
)
nrow(df_censor)

df_raw <-   df_censor
nrow(df_raw)
# Sort
df_raw <- df_raw %>% arrange(time)

df_interval_42_48 <- df_raw

print(df_interval_42_48)

#--------------------------------------
# Interval from 48 - 54 months
# There are 15 events/censors from 22-7
## 3 events and 12 censors
# expected survival should drop from 65 to 49.5
# Assume initial number at risk
initial_n <- 22
starting_survival <- 0.65

event_times <- c(48.1, 51, 53)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(11, min=48.01, max=50)
censor_times2 <- 51.5
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
df_interval_48_54 <- df_raw

print(df_interval_48_54)


#-----------------------------
# Interval from 54-61 months
# There are 6 events/censors from 7 to 1
## 0 events and 6 censors
# expected survival should drop from 48
# Assume initial number at risk
initial_n <- 7
starting_survival <- 0.48


# censoring
censor_times1 <- runif(5, min=54, max=56)
censor_times2 <- 61
censor_times <- c(censor_times1, censor_times2)
df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <-   df_censor
nrow(df_raw)
# Sort
df_raw <- df_raw %>% arrange(time)

df_interval_54_60 <- df_raw

print(df_interval_54_60)


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

#-------------------------
# Combine data frames
df_combined <- rbind(
  df_interval_0_6,
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
  break.time.by = 6,
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
    5.890827, 11.709376, 14.955021, 23.0, 
    28.979822, 35.600000, 41.900000, 47.173158, 
    53.000000),
  offsets = c(
    0.11, 0.3, 4, 0, 
    1.1, 1.12, 0.5, 0.11, 
    1.1
    )
)


fit_adj <- survfit(Surv(time, status) ~ 1, data = df_combined_adj)

ggsurv_adj <- ggsurvplot(
  fit_adj,
  conf.int = FALSE,
  risk.table = TRUE,
  xlab = "Time (Months)",
  ylab = "Estimated Survival Probability",
  break.time.by = 6,
  ggtheme = theme_minimal(base_size = 14),
  surv.scale = "percent",
  xlim = c(0, 66),          # Extend x-axis
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
  filename = "KN630 RFS Pembro Survival Table Reconstructed",
  subD = "files/reconstructed survival tables/KN630/rfs/pembrolizumab",
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