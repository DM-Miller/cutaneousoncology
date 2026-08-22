# Load Packages
library(tidyverse)
library(survminer)
library(survival)
source(here::here("scripts", "save files.R"))

#--------------------------------------
# Number at risk at beginning is 209, Number of events in total 24, thus there 185 censored events
# Interval 0-4 months
# There are 37 events/censors from 209-172
## Maybe 9 events and 29 censors
# expected survival should drop from 100 to 90
# Set initial n and starting survival (adjust to your context)
initial_n <- 209
starting_survival <- 1
set.seed(123)
event_times1 <- runif(4, min = 0.53, max = 1.4)
event_times2 <- runif(4, min = 2, max = 2.9)
event_times3 <- 3.99
event_times <- c(event_times1, event_times2, event_times3)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(8, min = 0.5, max = 0.51)
censor_times2 <- 3
censor_times3 <- runif(19, min = 3.1, max = 3.3)
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

df_interval_0_4 <- df_raw
nrow(df_interval_0_4)
# Sort by time for clarity
df_interval_0_4 <- df_interval_0_4 |> arrange(time)
nrow(df_interval_0_4)
# Preview
head(df_interval_0_4, 10)

#--------------------------------------
# Interval 4-8 months
# There are 15 events/censors from 172-157
## Maybe 4 events and 11 censors
# expected survival should drop from 97-95.5
# Set initial n and starting survival (adjust to your context)
initial_n <- 172
starting_survival <- 0.97
event_times1 <- c(5.4, 5.8)
event_times2 <- c(6.8, 7.0)
event_times <- c(event_times1, event_times2)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(10, min=5.0, max=6.0)
censor_times2 <- 7.8
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

df_interval_4_8 <- df_raw
nrow(df_interval_4_8)
# Sort by time for clarity
df_interval_4_8 <- df_interval_4_8 |> arrange(time)
nrow(df_interval_4_8)
# Preview
head(df_interval_4_8, 10)

#--------------------------------------
# Interval 8-12
# There are 25 events/censors from 157-132
## 1 events and 24 censors
# expected survival should drop from 95 to 92.4
# Set initial n and starting survival (adjust to your context)
initial_n <- 157
starting_survival <- 0.95

event_times <- 10
df_events <- data.frame(
  time = event_times,
  status = 1
  )
censor_times1 <- runif(20, min=8.1, max=8.5)
censor_times2 <- runif(4, min=10.5, max= 12)
censor_times <- c(censor_times1, censor_times2)

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

df_interval_8_12 <- df_raw
nrow(df_interval_8_12)

# Sort by time for clarity
df_interval_8_12 <- df_interval_8_12[order(df_interval_8_12$time), ]
nrow(df_interval_8_12)

# Preview
head(df_interval_8_12, 10)

#--------------------------------------
# Interval from 12 - 16 months
# There are 16 events/censors from 132-116
## 1 events and 15 censors
# expected survival should drop from 92.4 to 91
# Assume initial number at risk
initial_n <- 132
starting_survival <- 0.924

event_times <- 13.8
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- 12
censor_times2 <- runif(6, min=13, max=13.8)
censor_times3 <- runif(8, min=14, max=15.8)
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
df_interval_12_16 <- df_raw

print(df_interval_12_16)

#--------------------------------------
# Interval from 16 - 20 months
# There are 12 events/censors from 116-104
## 1 events and 11 censors
# expected survival should drop from 91 to 90
# Assume initial number at risk
initial_n <- 116
starting_survival <- 0.91

event_times <- 17.5
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)

# censoring
censor_times1 <- runif(2, min=16.8, max=17.3)
censor_times2 <- runif(9, min=17.6, max=18)
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
df_interval_16_20 <- df_raw


print(df_interval_16_20)

#--------------------------------------
# Interval 20-24
# There are 21 events/censors from 104-83
## 4 events and 17 censors
# expected survival should drop from 90 to 87.1
# Assume initial number at risk
initial_n <- 104
starting_survival <- 0.90

event_times <- c(20.8, 21.3, 23, 23.4)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(3, min=20.9, max=21.2)
censor_times2 <- runif(5, min=21.4, max=21.7)
censor_times3 <- runif(7, min=22, max=22.8)
censor_times4 <- runif(2, min=23.8, max=24)
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

df_interval_20_24 <- df_raw |> arrange(time)
nrow(df_interval_20_24)

#--------------------------------------
# Interval 24-28
# There are 17 events/censors from 83-66
## 2 events and 15 censors
# expected survival should drop from 87.1 to 85
# Assume initial number at risk
initial_n <- 83
starting_survival <- 0.871

event_times <- c(25, 26)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times1 <- runif(13, min=24, max=25.8)
censor_times2 <- c(27.2, 27.9)
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

df_interval_24_28 <- df_raw |> arrange(time)
nrow(df_interval_24_28)

#--------------------------------------
# Interval 28-32
# There are 19 events/censors from 66-47
## 1 events and 18 censors
# expected survival should drop from 85-84
# Assume initial number at risk
initial_n <- 66
starting_survival <- 0.85

event_times <- 30
df_events <- data.frame(
  time = event_times,
  status = 1
)

# censoring
censor_times1 <- runif(12, min=28.2, max=29.9)
censor_times2 <- runif(6, min=30.1, max=31.6)
censor_times <- c(censor_times1, censor_times2)

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

df_interval_28_32 <- df_raw

print(df_interval_28_32)

#--------------------------------------
# Interval from 32 - 36 months
# There are 14 events/censors from 47-33
## 0 events and 14 censors
# expected survival should drop from 83.1
# Assume initial number at risk
initial_n <- 47
starting_survival <- 0.831

#event_times <- c(48.1, 51, 53)
#df_events <- data.frame(
#  time = event_times,
#  status = 1
#)
#nrow(df_events)
# censoring
censor_times1 <- runif(13, min=32.5, max=34)
censor_times2 <- 35
censor_times <- c(censor_times1, censor_times2)

df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <- df_censor
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
# There are 6 events/censors from 33 to 0
## 1 event and 32 censors
# expected survival should drop from 83.1 to 82
# Assume initial number at risk
initial_n <- 33
starting_survival <- 0.831
event_times <- 37
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)

# censoring
censor_times1 <- runif(2, min=36, max=36.2)
censor_times2 <- runif(4, min = 37, max = 40)
censor_times3 <- runif(5, min=40.1, max=44)
censor_times4 <- runif(13, min = 44.1, max = 48)
censor_times5 <- runif(3, min = 48.1, max = 52)
censor_times6 <- runif(5, min = 52, max = 56)
censor_times7 <- 57
censor_times <- c(censor_times1, censor_times2, censor_times3, censor_times4, censor_times5, censor_times6, censor_times7)
df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)

df_raw <-   df_censor
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
  filename = "CPOST Cemiplimab Survival Table Reconstructed",
  subD = "files/reconstructed survival tables/CPOST/rfs/cemiplimab",
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