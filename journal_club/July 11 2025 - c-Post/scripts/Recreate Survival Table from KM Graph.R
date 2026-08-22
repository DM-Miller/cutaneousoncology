## Clear workspace for reproducibility
rm(list = ls())

# Function to load/install packages (avoids clutter & repetition)
load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Load core data manipulation and plotting packages
core_packages <- c(
  "tidyverse",   # dplyr, ggplot2, readr, etc.
  "data.table",  # Fast reading and manipulation
  "MASS",        # Statistical models
  "splines",     # Spline functions for survival curves
  "survival",    # Survival analysis
  "survminer",   # Publication-ready survival plots
  "ggplot2",     # Visualization
  "scales",      # Axis scaling for ggplots
  "grid", "gridExtra",  # Grid layouts for complex plots
  "gt", "gtsummary",    # Beautiful tables
  "knitr", "kableExtra", # Reporting & enhanced tables
  "here",        # Reproducible project paths
  "jpeg",        # Import JPEG images
  "sandwich",    # Robust standard errors
  "survey",      # Complex survey design / weighting
  "htmltools",   # HTML manipulation (for scrollable tables)
  "shiny"        # Optional: Shiny app integration
)
# Load all core packages
lapply(core_packages, load_pkg)


# Define file paths using here::here() for portability
# Read digitized KM curve data and number at risk table
data_path <- here::here("files/webplot digitizer") # Change to accomodate the path of your files
data_path
km_file <- file.path(data_path, "KN630_rfs_placebo_cleaned.csv")
file.exists(km_file)

nrisk_file <- file.path(data_path, "KN630_rfs_placebo_NumAtRisk.csv")
file.exists(nrisk_file)

digC <- fread(km_file)
# Load digitized Kaplan-Meier curve data
digC <- fread(km_file) %>%
  mutate(Curve1 = RFS, x = Time) %>%  # Rename 'y' to 'Curve1' for clarity
  dplyr::select(x, Curve1) |> 
  mutate(Curve1 = Curve1 / 100)


# Load number-at-risk data
nRisk <- fread(nrisk_file) 
# Load number-at-risk data and remove rows where Nrisk is zero (done for analysis purposes)
nRisk <- fread(nrisk_file) %>%
  filter(Nrisk != 0)

# 📘 1. Load and prepare the KM data
colnames(digC) <- c("t", "p")

if (sum(digC$t == 0) == 0) {
  digC <- rbind(
    data.frame(t=0, p=1),
    digC
  )
} else {
  digC$p[which(digC$t == 0)] <- 1
}


# 📘 2. Prepare the number-at-risk data

t.risk0 <- nRisk$Time
n.risk0 <- nRisk$Nrisk
print(t.risk0)
print(n.risk0)

# 📘 3. Clean the number-at-risk intervals (formerly Nrisk_low_up)
## (a) Remove intervals without any KM data inside them:
t_risk_work <- t.risk0
n_risk_work <- n.risk0

for (i in 2:(length(t_risk_work) - 1)) {
  temp <- which((t_risk_work[i] <= digC$t) & (digC$t < t_risk_work[i + 1]))
  if (length(temp) == 0) {
    t_risk_work[i] <- -1
    n_risk_work[i] <- -1
    cat("No events between", t.risk0[i], "and", t.risk0[i+1], "\n")
  }
}
# (b) Remove invalid intervals:
t_risk1 <- t_risk_work[t_risk_work != -1]
t_risk1
n_risk1 <- n_risk_work[n_risk_work != -1]
n_risk1

# 📘 4. Identify lower and upper indices in KM times
# (a) Compute lower indices:
lower1 <- sapply(
  1:(length(t_risk1)-1),
  function(i) {
    inds <- which((t_risk1[i] <= digC$t) & (digC$t < t_risk1[i+1]))
    if (length(inds)==0) NA else min(inds)
  }
)
lower1
# (b) Compute upper indices:
upper1 <- sapply(
  1:(length(t_risk1)-1),
  function(i) {
    inds <- which((t_risk1[i] <= digC$t) & (digC$t < t_risk1[i+1]))
    if (length(inds)==0) NA else max(inds)
  }
)
upper1
# (c) Append the last interval:
inds_last <- which(t_risk1[length(t_risk1)] <= digC$t)
inds_last
if (length(inds_last) > 0) {
  lower1_last <- min(inds_last)
  upper1_last <- max(inds_last)
  
  lower1 <- c(lower1, lower1_last)
  upper1 <- c(upper1, upper1_last)
} else {
  cat("No KM points beyond last t_risk. Skipping final interval.\n")
}
lower1
upper1
# Remove invalid indices (Inf, -Inf, NA)
valid <- !(is.na(lower1) | is.na(upper1) | is.infinite(lower1) | is.infinite(upper1))

lower1 <- lower1[valid]
upper1 <- upper1[valid]
lower1
upper1


# 5. Initialize All Vectors
# Number of intervals (number-at-risk times)
n.int <- length(lower1) + 1

# e.g., 11 time points
print(paste("n.int =", n.int))

# Get the max time index from the upper bounds
n.t <- upper1[length(upper1)]  
# e.g., 174 digitized KM time points
print(paste("n.t =", n.t))

# Initialize vector to store estimated censors in each interval
n.censor <- rep(0, n.int - 1)  
# length = 10 intervals between the 11 at-risk time points
length(n.censor)
print(n.censor)

# Estimated number at risk at each KM time point (will be updated in the loop)
n.hat <- rep(n_risk1[1] + 1, n.t)  
# starts as all 226 (225+1) - note: the +1 helps ensure convergence in Guyot's approach
length(n.hat)
print(n.hat)

# Number of censorings occurring at each KM time point
cen <- rep(0, n.t)  
length(cen)
print(cen)

# Number of events occurring at each KM time point
d <- rep(0, n.t)  
print(d)

# Estimated Kaplan-Meier survival probability at each KM time point
KM.hat <- rep(1, n.t)  
length(KM.hat)
print(KM.hat)

# Tracks the last time point within each interval where an event occurred
last.i <- rep(1, n.int)  
print(last.i)

# 📘 6. Estimate events and censoring per interval
## Loop through intervals:
for (i in 1:(length(lower1)-1)) {
  
  # Estimate censor count
  n.censor[i] <- round(n_risk1[i] * digC$p[lower1[i+1]] / digC$p[lower1[i]] - n_risk1[i+1])
  cat("\n--- Interval", i, "---\n")
  cat("lower1[i+1] =", lower1[i+1], "\n")
  cat("n.hat[lower1[i+1]] =", n.hat[lower1[i+1]], "\n")
  cat("n_risk1[i+1] =", n_risk1[i+1], "\n")
  cat("n.censor[i] =", n.censor[i], "\n")
  
  while ((n.hat[lower1[i+1]] > n_risk1[i+1]) ||
         ((n.hat[lower1[i+1]] < n_risk1[i+1]) && (n.censor[i]>0))) {
    
    if (n.censor[i]<=0) {
      cen[lower1[i]:upper1[i]] <- 0
      n.censor[i] <- 0
    } else {
      cen.t <- digC$t[lower1[i]] + (1:n.censor[i]) * 
        (digC$t[lower1[i+1]] - digC$t[lower1[i]])/(n.censor[i]+1)
      cen[lower1[i]:upper1[i]] <- hist(
        cen.t,
        breaks=digC$t[lower1[i]:lower1[i+1]],
        plot=FALSE
      )$counts
    }
    
    n.hat[lower1[i]] <- n_risk1[i]
    last <- last.i[i]
    if (is.null(last) || is.na(last)) {
      last <- lower1[i]
    }
    
    
    for (k in lower1[i]:upper1[i]) {
      ratio <- digC$p[k]/KM.hat[last]
      if (is.na(ratio) || ratio>1) ratio <- 1
      
      d[k] <- if (i==1 && k==lower1[i]) 0 else round(n.hat[k]*(1 - ratio))
      if (is.na(d[k]) || d[k]<0) d[k] <- 0
      
      KM.hat[k] <- KM.hat[last]*(1 - (d[k]/n.hat[k]))
      n.hat[k+1] <- n.hat[k] - d[k] - cen[k]
      
      if (d[k]!=0) last <- k
    }
    
    n.censor[i] <- n.censor[i] + (n.hat[lower1[i+1]] - n_risk1[i+1])
  }
  
  # Force alignment of number at risk AFTER adjustments
  n.hat[lower1[i+1]] <- n_risk1[i+1]
  if (!is.numeric(last)) stop("last is not numeric! It is: ", last)
  last.i[i+1] <- last
  cat("Final n.hat at time index", lower1[i+1], "forced to", n.hat[lower1[i+1]], "\n")
}

data.frame(
  time = digC$t[lower1],
  n_hat = n.hat[lower1],
  n_risk = n_risk1[1:(length(lower1))]
)

# 📘 7. Scale events to total events
#Example: total_events <- 57 (or whatever your value)
total_events <- nRisk$`Number of Events`[1]
total_events
total_assigned_events <- sum(d)
total_assigned_events
# Compute the scaling factor
scaling_factor <- total_events / total_assigned_events
print(paste("Scaling factor =", scaling_factor))

d_scaled <- d*scaling_factor
d_scaled
sum(d_scaled)
d_rounded <- floor(d_scaled)
d_rounded
sum(d_rounded)

remaining_events <- total_events - sum(d_rounded)
print(paste("Events before increment =", sum(d_rounded)))
print(paste("Remaining events to assign =", remaining_events))

#remainder_order <- seq_along(d_scaled)
# Find which time points had the largest leftover fractions
remainder_order <- order(d_scaled - d_rounded, decreasing = TRUE)
remainder_order

# Assign the remaining events to the top time points by leftover fractions
if (remaining_events > 0) {
  d_rounded[remainder_order[1:remaining_events]] <- d_rounded[remainder_order[1:remaining_events]] + 1
}

print(paste("Total events after final assignment =", sum(d_rounded)))

d <- d_rounded
d
sum(d)

# Step 8
# -------- Reconstruct Individual Patient Data (IPD) ----------

# Initialize IPD time vector (all default to max time)
t.IPD <- rep(digC$t[n.t], n_risk1[1])
t.IPD
# Initialize event indicator vector (0 = censored)
event.IPD <- rep(0, n_risk1[1])
event.IPD
# Tracker index
k <- 1

# Assign event times
for (j in 1:n.t) {
  if (d[j] != 0) {
    t.IPD[k:(k + d[j] - 1)] <- rep(digC$t[j], d[j])
    event.IPD[k:(k + d[j] - 1)] <- rep(1, d[j])
    k <- k + d[j]
  }
}

# Assign censoring times (midpoints between t_j and t_{j+1})
for (j in 1:(n.t - 1)) {
  if (cen[j] != 0) {
    t.IPD[k:(k + cen[j] - 1)] <- rep((digC$t[j] + digC$t[j + 1]) / 2, cen[j])
    event.IPD[k:(k + cen[j] - 1)] <- rep(0, cen[j])
    k <- k + cen[j]
  }
}

# Create the final IPD dataframe
IPD <- data.frame(t = t.IPD, ev = event.IPD)
IPD

# Ensure last time aligns with the survival curve max
if (IPD[nrow(IPD), "t"] < digC$t[length(digC$t)]) {
  IPD[nrow(IPD), "t"] <- digC$t[length(digC$t)]
}

# Print preview
head(IPD)

IPD <- IPD |> arrange(t)
IPD
sum(IPD$ev)


# ===========================================================
# 🟢 START HERE: Extract event/censor counts from IPD
# ===========================================================

# 🟢 Make sure IPD exists already from your prior code
stopifnot(exists("IPD"))
stopifnot(exists("digC"))
stopifnot(all(c("t", "ev") %in% names(IPD)))

# ------------------------------
# 📘 1. Define intervals
# ------------------------------
# e.g., using your actual time points for nRisk
# Define 6-month intervals
interval_bounds <- c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, Inf)
interval_labels <- paste0(
  interval_bounds[-length(interval_bounds)], "-",
  ifelse(interval_bounds[-1]==Inf, "+", interval_bounds[-1])
)

# Confirm counts
table(IPD$ev)

# Assign intervals to each observation
# Assign interval labels
IPD$interval <- cut(
  IPD$t,
  breaks = interval_bounds,
  right = FALSE,
  labels = paste0("Interval_", seq_len(n_intervals))
)
# ------------------------------
# 📘 2. Compute exits per interval
# ------------------------------
# We know total events and censors:
n_events_total <- sum(IPD$ev)
n_censor_total <- nrow(IPD) - n_events_total

# Summarize counts per interval
interval_counts <- IPD %>%
  group_by(interval) %>%
  summarize(
    n = n(),
    events = sum(ev),
    censors = n() - sum(ev)
  ) %>%
  arrange(interval) %>%
  mutate(interval_id = row_number())

print(interval_counts)

# ------------------------------
# 📘 3. Initialize allocation
# ------------------------------
# Split the interval counts into separate vectors
interval_n <- interval_counts$n
interval_events <- interval_counts$events
interval_censors <- interval_counts$censors
n_intervals <- length(interval_n)

# ------------------------------
# 📘 4. Prepare per-timepoint allocation
# ------------------------------
# Build initial vectors of d and cen matching digC$t
event_censor_counts <- IPD %>%
  group_by(t) %>%
  summarize(
    d = sum(ev == 1),
    cen = sum(ev == 0)
  ) |> 
  ungroup()
event_censor_counts

df_counts <- tibble(t = digC$t) %>%
  left_join(event_censor_counts, by = "t") %>%
  mutate(
    d = replace_na(d, 0),
    cen = replace_na(cen, 0)
  )

d <- df_counts$d
d
cen <- df_counts$cen
cen

# For each timepoint, assign interval
df_counts$interval <- cut(
  df_counts$t,
  breaks = interval_bounds,
  right = FALSE,
  labels = interval_labels
)

# Split into list of per-interval data.frames
df_list <- split(df_counts, df_counts$interval)

# ------------------------------
# 📘 5. Loss function
# ------------------------------
compute_survival_and_risk_at_targets <- function(d_vec, cen_vec, n.hat_start, digC, target_times) {
  KM.hat <- rep(1, length(d_vec))
  n.hat <- rep(n.hat_start, length(d_vec))
  last <- 1
  surv <- numeric(length(target_times))
  nrisk <- numeric(length(target_times))
  
  for (k in seq_along(d_vec)) {
    if (k > 1) {
      KM.hat[k] <- KM.hat[last] * (1 - d_vec[k] / n.hat[k])
    }
    if (d_vec[k] > 0) last <- k
    if (k < length(d_vec)) {
      n.hat[k + 1] <- n.hat[k] - d_vec[k] - cen_vec[k]
    }
  }
  
  for (i in seq_along(target_times)) {
    idx <- which.min(abs(digC$t - target_times[i]))
    surv[i] <- KM.hat[idx]
    nrisk[i] <- n.hat[idx]
  }
  
  list(survival = surv, nrisk = nrisk)
}

loss_fn <- function(d_vec, cen_vec) {
  res <- compute_survival_and_risk_at_targets(
    d_vec, cen_vec, n_risk_start + 1, digC, target_times
  )
  surv_error <- sum((res$survival - target_survivals)^2)
  nrisk_error <- sum(((res$nrisk - target_nrisk)/n_risk_start)^2)
  loss <- surv_error * 3 + nrisk_error
  return(loss)
}

# ------------------------------
# 📘 6. Optimizer
# ------------------------------
# Build starting d and cen vectors
d_start <- df_counts$d
cen_start <- df_counts$cen

# Initialize best
d_current <- d_start
cen_current <- cen_start
best_loss <- loss_fn(d_current, cen_current)
cat("Initial loss:", best_loss, "\n")

# Proposal function: within intervals only
propose_move <- function(d_vec, cen_vec) {
  d_new <- d_vec
  cen_new <- cen_vec
  
  # Randomly pick an interval
  interval_i <- sample(seq_along(df_list), 1)
  df_int <- df_list[[interval_i]]
  idxs <- which(df_counts$interval == names(df_list)[interval_i])
  
  # Get indices with events/censors
  idx_event <- idxs[which(d_new[idxs] > 0)]
  idx_censor <- idxs[which(cen_new[idxs] > 0)]
  
  move_type <- sample(c("move_event", "move_censor", "swap_e2c"), 1)
  
  if (move_type == "move_event" && length(idx_event) > 0) {
    i <- sample(idx_event, 1)
    j <- sample(idxs, 1)
    if (i != j) {
      d_new[i] <- d_new[i] - 1
      d_new[j] <- d_new[j] + 1
    }
  } else if (move_type == "move_censor" && length(idx_censor) > 0) {
    i <- sample(idx_censor, 1)
    j <- sample(idxs, 1)
    if (i != j) {
      cen_new[i] <- cen_new[i] - 1
      cen_new[j] <- cen_new[j] + 1
    }
  } else if (move_type == "swap_e2c" && length(idx_event) > 0 && length(idx_censor) > 0) {
    i <- sample(idx_event, 1)
    j <- sample(idx_censor, 1)
    d_new[i] <- d_new[i] - 1
    cen_new[i] <- cen_new[i] + 1
    cen_new[j] <- cen_new[j] - 1
    d_new[j] <- d_new[j] + 1
  }
  
  list(d = d_new, cen = cen_new)
}

# ------------------------------
# 📘 7. Optimization loop
# ------------------------------
set.seed(1)
max_iter <- 5000

for (iter in 1:max_iter) {
  prop <- propose_move(d_current, cen_current)
  
  # Enforce per-interval constraints
  # For each interval, count total exits
  df_tmp <- df_counts
  df_tmp$d <- prop$d
  df_tmp$cen <- prop$cen
  df_sum <- df_tmp %>%
    group_by(interval) %>%
    summarize(n_exit = sum(d) + sum(cen))
  
  # Check that intervals still have the same total exits
  interval_ok <- all(df_sum$n_exit == interval_n)
  
  # Check no negatives
  nonneg <- all(prop$d >= 0) && all(prop$cen >= 0)
  
  if (interval_ok && nonneg) {
    l <- loss_fn(prop$d, prop$cen)
    if (l < best_loss) {
      d_current <- prop$d
      cen_current <- prop$cen
      best_loss <- l
      cat("Iter", iter, "Loss improved to", l, "\n")
    }
  }
}

# ------------------------------
# 📘 8. Reconstruct IPD
# ------------------------------
IPD_final <- data.frame(
  t = digC$t,
  ev = d_current
) %>% tidyr::uncount(weights = ev) %>% mutate(event = 1)

censor_df <- data.frame(
  t = digC$t,
  ev = cen_current
) %>% tidyr::uncount(weights = ev) %>% mutate(event = 0)

IPD_optimized <- bind_rows(IPD_final, censor_df) %>% arrange(t)

cat("Final total rows:", nrow(IPD_optimized), "\n")
cat("Final total events:", sum(IPD_optimized$event), "\n")

# Done!

#-===============================

# 🟢 START HERE: Assuming your IPD is already prepared
# Should have 225 rows, columns t (time) and ev (0=censor, 1=event)
stopifnot(exists("IPD"))
stopifnot(nrow(IPD) == 225)
stopifnot(all(c("t", "ev") %in% names(IPD)))

# Confirm counts
table(IPD$ev)

# 📘 Define intervals: e.g., every 12 months
interval_bounds <- c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, Inf)
n_intervals <- length(interval_bounds) - 1

# Assign interval labels
IPD$interval <- cut(
  IPD$t,
  breaks = interval_bounds,
  right = FALSE,
  labels = paste0("Interval_", seq_len(n_intervals))
)

# Confirm distribution across intervals
table(IPD$interval)

# 📘 Target summaries
target_times <- c(6, 12, 18, 24, 30, 36, 42, 48, 54, 60)
target_survivals <- c(0.85, 0.785, 0.72, 0.686, 0.643, 0.632, 0.632, 0.632, 0.44, 0.44)
target_nrisk <- c(169, 124, 101, 80, 58, 43, 33, 16, 7, 1)

# 📘 Digitized KM curve (assumed loaded as digC)
stopifnot(exists("digC"))
if (!"p" %in% names(digC)) names(digC) <- c("t", "p")
if (sum(digC$t == 0) == 0) digC <- rbind(data.frame(t=0, p=1), digC)
digC <- digC[order(digC$t),]

# 📘 Function to compute survival + at-risk from patient-level data
compute_survival_and_risk <- function(IPD, target_times) {
  # Order
  df <- IPD[order(IPD$t), ]
  
  n_risk_start <- nrow(df)
  times <- unique(df$t)
  
  surv <- numeric(length(target_times))
  nrisk <- numeric(length(target_times))
  
  at_risk <- n_risk_start
  surv_prob <- 1
  k <- 1
  
  for (i in seq_along(target_times)) {
    t_cut <- target_times[i]
    events_before <- df[df$t <= t_cut & df$ev==1, ]
    censors_before <- df[df$t <= t_cut & df$ev==0, ]
    
    for (e in events_before$t) {
      surv_prob <- surv_prob * (1 - 1/at_risk)
      at_risk <- at_risk - 1
    }
    at_risk <- at_risk - nrow(censors_before)
    
    surv[i] <- surv_prob
    nrisk[i] <- at_risk
  }
  
  list(survival = surv, nrisk = nrisk)
}

# 📘 Loss function
loss_fn <- function(IPD) {
  res <- compute_survival_and_risk(IPD, target_times)
  surv_error <- sum((res$survival - target_survivals)^2)
  nrisk_error <- sum(((res$nrisk - target_nrisk)/nrow(IPD))^2)
  loss <- 3 * surv_error + nrisk_error
  return(loss)
}

# 📘 Perturbation function
perturb_IPD <- function(IPD) {
  new_IPD <- IPD
  # Random row to move
  i <- sample(seq_len(nrow(new_IPD)), 1)
  row <- new_IPD[i,]
  
  # Move time within its interval
  bnds <- as.numeric(strsplit(as.character(row$interval), "_")[[1]][2])
  t_low <- interval_bounds[bnds]
  t_high <- interval_bounds[bnds+1]
  new_t <- runif(1, t_low, t_high)
  
  # Possibly swap event ↔ censor
  if (runif(1) < 0.05) {
    new_ev <- 1 - row$ev
  } else {
    new_ev <- row$ev
  }
  
  new_IPD$t[i] <- new_t
  new_IPD$ev[i] <- new_ev
  return(new_IPD)
}

# 📘 Initialize
IPD_current <- IPD
best_loss <- loss_fn(IPD_current)
cat("Initial loss:", best_loss, "\n")

# 📘 Optimization loop
set.seed(123)
max_iter <- 10000

for (iter in 1:max_iter) {
  proposal <- perturb_IPD(IPD_current)
  l <- loss_fn(proposal)
  if (l < best_loss) {
    IPD_current <- proposal
    best_loss <- l
    cat("Iter", iter, "Loss improved to", round(l,5), "\n")
  }
}

# 📘 Final result
cat("Final loss:", best_loss, "\n")
table(IPD_current$ev)

# 📘 Inspect final IPD
head(IPD_current)


# ===============================================================
# Summarize your optimized IPD vs targets at the key timepoints
# ===============================================================
IPD_optimized <- IPD_current
# Ensure your IPD is sorted
IPD_optimized <- IPD_optimized %>% arrange(t)

# Compute the Kaplan-Meier estimator
fit <- survfit(Surv(t, ev) ~ 1, data = IPD_optimized)

# Create a function to compute the cumulative number of events and censors at any time
get_cumulative_counts <- function(time_points, IPD) {
  sapply(time_points, function(x) {
    c(
      events = sum(IPD$t <= x & IPD$event == 1),
      censored = sum(IPD$t <= x & IPD$event == 0)
    )
  }) %>% t() %>% as.data.frame()
}

# Compute survival estimates at target times
surv_at_targets <- summary(fit, times = target_times, extend = TRUE)

# Get cumulative counts
cum_counts <- get_cumulative_counts(target_times, IPD_optimized)

# Reconstruct number at risk at each target time
n_risk_reconstructed <- sapply(target_times, function(x) sum(IPD_optimized$t >= x))

# Combine everything into a table
df_comparison <- tibble(
  Time = target_times,
  Survival_Reconstructed = surv_at_targets$surv,
  Survival_Target = target_survivals,
  N_Risk_Reconstructed = n_risk_reconstructed,
  N_Risk_Target = target_nrisk,
  Cum_Events = cum_counts$events,
  Cum_Censored = cum_counts$censored
)

# Print table
print(df_comparison)

# Optional: pretty table with gt
df_comparison %>%
  gt::gt() %>%
  gt::fmt_percent(columns = c(Survival_Reconstructed, Survival_Target), decimals = 1) %>%
  gt::tab_header(
    title = "Comparison of Reconstructed IPD vs Target KM Estimates"
  )

# 📘 Assumes you have:
# - IPD_optimized: the reconstructed IPD with t and event columns
# - IPD_counts: your 225-row table with t, d, cen columns
# - target_times, target_survivals, target_nrisk

# Compute KM survival from your IPD
fit <- survfit(Surv(t, ev) ~ 1, data = IPD_optimized)

# Helper: cumulative counts at target times
get_cumulative_counts <- function(time_points, IPD) {
  sapply(time_points, function(x) {
    c(
      events = sum(IPD$t <= x & IPD$event == 1),
      censored = sum(IPD$t <= x & IPD$event == 0)
    )
  }) %>% t() %>% as.data.frame()
}

# Reconstructed survival and number at risk
surv_at_targets <- summary(fit, times = target_times, extend = TRUE)
cum_counts <- get_cumulative_counts(target_times, IPD_optimized)
n_risk_reconstructed <- sapply(target_times, function(x) sum(IPD_optimized$t >= x))

# Build milestone comparison
df_milestones <- tibble(
  t_milestone = target_times,
  Survival_Reconstructed = surv_at_targets$surv,
  Survival_Target = target_survivals,
  N_Risk_Reconstructed = n_risk_reconstructed,
  N_Risk_Target = target_nrisk,
  Cum_Events = cum_counts$events,
  Cum_Censored = cum_counts$censored
)

# 📘 Join milestone data onto your 225-row table
# (using fuzzy matching to carry forward the last known milestone)
IPD_counts_enriched <- IPD_current %>%
  arrange(t) %>%
  # For each t, get the most recent milestone time
  mutate(t_milestone = purrr::map_dbl(t, ~max(df_milestones$t_milestone[df_milestones$t_milestone <= .x], na.rm = TRUE))) %>%
  left_join(df_milestones, by = "t_milestone") %>%
  select(
    t,
    d,
    cen,
    Survival_Reconstructed,
    Survival_Target,
    N_Risk_Reconstructed,
    N_Risk_Target,
    Cum_Events,
    Cum_Censored
  )

# View it
print(IPD_counts_enriched, n = 50)

# Optionally: save as CSV
# write_csv(IPD_counts_enriched, "IPD_counts_enriched.csv")

