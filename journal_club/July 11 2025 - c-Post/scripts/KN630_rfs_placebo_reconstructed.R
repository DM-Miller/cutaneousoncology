# Load Packages
library(tidyverse)
library(survminer)
library(survival)
source(here::here("scripts", "save files.R"))

set.seed(123)
#--------------------------------------
# Interval 0-6 months
# There are 56 events/censors from 225-169
## Maybe 22 events and 35 censors
# expected survival should drop from 100 to 85.3

event_times0 <- c(0.4, 0.6)
event_times1 <- runif(5, min = 0.5, max = 2.0)
event_times2 <- runif(8, min = 2.9, max = 3.4)
event_times3 <-runif(4, min = 4, max = 4.9)
event_times4 <- runif(4, min = 5.3, max = 5.99)
event_times <- c(
  event_times0,
  event_times1, event_times2, 
  event_times3, event_times4)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)

# censoring
censor_times1 <- runif(33, min=2.0, max=2.3)
censor_times2 <- runif(1, min=5.5, max=5.9)
censor_times3 <- 5.914729
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
df_interval_0_6 <- df_interval_0_6[order(df_interval_0_6$time), ]
nrow(df_interval_0_6)
# Preview
head(df_interval_0_6, 10)




#--------------------------------------
# Interval 6-12 months
# There are 45 events/censors from 169-124
## Maybe 17 events and 28 censors
# expected survival should drop from 85.3 to 78.5

event_times1 <- 7
event_times2 <- runif(4, min = 8, max = 9)
event_times3 <- 9.5
event_times4 <- runif(10, min = 10, max = 11.95)
event_times <- c(event_times1, event_times2, event_times3, event_times4)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times0 <- runif(1, min=6.1, max=7)
censor_times1 <- runif(24, min=7.8, max=8.1)
censor_times2 <- 9.7
censor_times3 <- runif(1, min = 11.5, max = 11.6)
censor_times4 <- 11.835754
censor_times <- c(
  censor_times0,
  censor_times1, censor_times2, censor_times3, censor_times4)

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

event_times <- c(12.1, 12.2, 13, 14, 14.5, 15, 15.2, 16.5, 17.8) # n = 9 
df_events <- data.frame(
  time = event_times,
  status = 1
)
censor_times1 <- runif(9, min=12.5, max = 12.9)
censor_times2 <- c(15.5, 15.6)
censor_times3 <- c(17.9, 17.9, 17.9)
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
# 80-58, 4 events, 18 censors
# Survival should drop from 68.6 to 66
# Assume initial number at risk
initial_n <- 80
starting_survival <- 0.686
event_times <- c(24.5, 25, 28, 29)
df_events <- data.frame(
  time = event_times,
  status = 1
)
nrow(df_events)
# censoring
censor_times <- c(
  24.4,24.45,24.46,24.48,  #4 censored
  24.6,24.65,24.66,24.66,24.67,24.7,24.75,24.8,24.9,  #9 censored
  27,27.2,27.5,27.7,27.8  #5 censored
)
# Recreate your raw data
df_censor <- data.frame(
  time = censor_times,
  status = 0
)
nrow(df_censor)
# Convert to data frame
df_raw <- rbind(
  df_events,
  df_censor
)

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


df_interval_24_30 <- df_raw
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
# 9 events/censoring
# 3 events and 6 censors

# Example vectors
event_times <-  c(51.6, 52.0, 53.0)
df_events <- data.frame(
  time = event_times,
  status = 1
)
censored_times <- c(49.0, 49.01, 49.02, 49.1, 49.2, 49.21)
df_censor <- data.frame(
  time = censored_times,
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
initial_n <- 16
starting_survival <- 0.63

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
# Interval from 54 months

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
    5.9147290,
    5.9137821, 11.835754, 
    17.8, 23.5, 29, 35.704566, 41.668605,
    47.668605, 53.000000),
  offsets = c(
    0.1,
    0.1, .2, 
    .3, .6, 1.1, 0.31, 0.5, 0.5, 1.1)
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

save_files(
  save_object = df_combined_adj,
  filename = "KN630 RFS Placebo Survival Table Reconstructed",
  subD = "files/reconstructed survival tables/KN630/rfs/placebo",
  extension = ".csv"
)
sum(df_combined$status)




