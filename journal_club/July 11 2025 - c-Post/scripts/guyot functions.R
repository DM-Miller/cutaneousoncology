Nrisk_low_up <- function(
    t.risk0,   # Vector of original time points from the number-at-risk table
    t.S1,      # Vector of time points from the digitized survival (KM) curve
    n.risk0    # Vector of number-at-risk counts at each t.risk0 time point
) {
  
  # Iterate over each number-at-risk interval (excluding first and last) 
  for (i in 2:(length(t.risk0) - 1)) {
    # Identify survival curve time points (t.S1) that fall within the current at-risk interval
    temp <- which((t.risk0[i] <= t.S1) & (t.S1 < t.risk0[i + 1]))
    
    # If no points fall within this interval, mark the at-risk time and count as invalid (-1)
    if (length(temp) == 0) {
      t.risk0[i] <- -1
      n.risk0[i] <- -1
      print(paste0("No events/points found between ", t.risk0[i], " and ", t.risk0[i + 1]))
    }
  }
  
  # Remove any invalid (-1) time points and counts
  t.risk1 <- t.risk0[t.risk0 != -1]
  n.risk1 <- n.risk0[n.risk0 != -1]
  
  # For each valid at-risk interval, find the corresponding indices (lower and upper) in t.S1
  lower1 <- sapply(1:(length(t.risk1) - 1), function(i) min(which((t.risk1[i] <= t.S1) & (t.S1 < t.risk1[i + 1]))))
  upper1 <- sapply(1:(length(t.risk1) - 1), function(i) max(which((t.risk1[i] <= t.S1) & (t.S1 < t.risk1[i + 1]))))
  
  # Handle the last interval separately to ensure coverage to the end of t.S1
  lower1 <- c(lower1, min(which(t.risk1[length(t.risk1)] <= t.S1)))
  upper1 <- c(upper1, max(which(t.risk1[length(t.risk1)] <= t.S1)))
  
  # Return a list of cleaned and matched at-risk times and their index mappings
  list(
    "lower1" = lower1,  # Lower index in t.S1 for each at-risk interval
    "upper1" = upper1,  # Upper index in t.S1 for each at-risk interval
    "t.risk1" = t.risk1,  # Cleaned vector of valid at-risk time points
    "n.risk1" = n.risk1   # Cleaned vector of valid number-at-risk counts
  )
}

# Main reconstruction function based on Guyot et al. algorithm
reconKMGuyot <- function(
    tot.events,    # Total number of events reported in the study
    t.S,           # Vector of time points from the digitized KM curve
    S,             # Vector of survival probabilities corresponding to t.S
    t.risk,        # Vector of time points where number-at-risk is reported
    lower,         # Vector of lower indices mapping number-at-risk intervals to t.S
    upper,         # Vector of upper indices mapping number-at-risk intervals to t.S
    n.risk,        # Vector of number-at-risk values at each t.risk time point
    tol            # Tolerance value (typically for convergence, not directly used here)
) {
  
  
  # Number of risk intervals
  n.int <- length(n.risk)
  # Total number of survival time points
  n.t <- upper[n.int]
  
  # Initialize vectors to store:
  # Estimated number of censorings per interval
  n.censor <- rep(0, n.int - 1)
  # Estimated number at risk at each time point
  n.hat <- rep(n.risk[1] + 1, n.t)
  # Number of censorings at each time point
  cen <- rep(0, n.t)
  # Number of events at each time point
  d <- rep(0, n.t)
  # Estimated Kaplan-Meier survival probability at each time point
  KM.hat <- rep(1, n.t)
  # Track the last time point with an event
  last.i <- rep(1, n.int)
  # Sum of events (for book-keeping, not used here)
  sumdL <- 0
  
  # Start looping through each risk interval to estimate events and censoring
  if (n.int > 1) {
    for (i in 1:(n.int - 1)) {
      
      # Estimate number of censored patients between interval i and i+1
      n.censor[i] <- round(n.risk[i] * S[lower[i + 1]] / S[lower[i]] - n.risk[i + 1])
      
      # Adjust estimated number of censorings iteratively until number at risk aligns
      while ((n.hat[lower[i + 1]] > n.risk[i + 1]) || 
             ((n.hat[lower[i + 1]] < n.risk[i + 1]) && (n.censor[i] > 0))) {
        
        if (n.censor[i] <= 0) {
          # If no censoring, set all censoring counts in this interval to zero
          cen[lower[i]:upper[i]] <- 0
          n.censor[i] <- 0
        } else {
          # Distribute censoring events uniformly across the interval
          cen.t <- t.S[lower[i]] + (1:n.censor[i]) * 
            (t.S[lower[i + 1]] - t.S[lower[i]]) / (n.censor[i] + 1)
          
          # Bin the censor times into the respective survival time intervals
          cen[lower[i]:upper[i]] <- hist(cen.t, 
                                         breaks = t.S[lower[i]:lower[i + 1]], 
                                         plot = FALSE)$counts
        }
        
        # Set starting number at risk for this interval
        n.hat[lower[i]] <- n.risk[i]
        # Initialize the last time point with event
        last <- last.i[i]
        
        # Iterate through each time point in this interval
        for (k in lower[i]:upper[i]) {
          # Estimate events at time k using conditional survival probability
          d[k] <- if (i == 1 && k == lower[i]) 0 else round(n.hat[k] * (1 - (S[k] / KM.hat[last])))
          
          # Update KM estimate based on new events
          KM.hat[k] <- KM.hat[last] * (1 - (d[k] / n.hat[k]))
          
          # Update number at risk for next time point
          n.hat[k + 1] <- n.hat[k] - d[k] - cen[k]
          
          # Update last event time if an event occurred at k
          if (d[k] != 0) last <- k
        }
        
        # Adjust censoring based on updated number at risk
        n.censor[i] <- n.censor[i] + (n.hat[lower[i + 1]] - n.risk[i + 1])
      }
      
      # Ensure that the estimated number at risk aligns with the known number
      if (n.hat[lower[i + 1]] < n.risk[i + 1]) n.risk[i + 1] <- n.hat[lower[i + 1]]
      
      # Store last event time point for next interval
      last.i[i + 1] <- last
    }
  }
  
  # -------- Reconstruct Individual Patient Data (IPD) ----------
  
  # Initialize IPD time vector (default all to last time point)
  t.IPD <- rep(t.S[n.t], n.risk[1])
  # Initialize event indicator vector (0 = censored by default)
  event.IPD <- rep(0, n.risk[1])
  # Tracker for indexing patients
  k <- 1
  
  # Assign event times to the patients
  for (j in 1:n.t) {
    if (d[j] != 0) {
      t.IPD[k:(k + d[j] - 1)] <- rep(t.S[j], d[j])  # Assign event time
      event.IPD[k:(k + d[j] - 1)] <- rep(1, d[j])   # Mark as event
      k <- k + d[j]  # Move the index forward
    }
  }
  
  # Assign censoring times (placed between survival times)
  for (j in 1:(n.t - 1)) {
    if (cen[j] != 0) {
      # Censoring time is placed mid-interval
      t.IPD[k:(k + cen[j] - 1)] <- rep((t.S[j] + t.S[j + 1]) / 2, cen[j])
      event.IPD[k:(k + cen[j] - 1)] <- rep(0, cen[j])  # Mark as censored
      k <- k + cen[j]
    }
  }
  
  # Create the final IPD dataframe
  IPD <- data.frame(t = t.IPD, ev = event.IPD)
  
  # Final adjustment to ensure the last time aligns with survival curve max
  if (IPD[nrow(IPD), "t"] < t.S[length(t.S)]) {
    IPD[nrow(IPD), "t"] <- t.S[length(t.S)]
  }
  
  # Return both the reconstructed event/censoring table and the IPD
  return(list(dat = cbind(t.S, S, n.hat[1:n.t], d, cen), ipd = IPD))
}


FUN_KM_RECON <- function(
    rawkmfile,      # Digitized Kaplan-Meier survival data (time and probability)
    rawnriskfile,   # Number-at-risk table with time and counts (can be NULL)
    totev,          # Total number of events reported for the study
    totp = 0        # Total number of patients (used if number-at-risk table is missing)
) {
  
  # Rename the digitized KM data columns for clarity: 't' for time and 'p' for survival probability
  colnames(rawkmfile) <- c("t", "p")
  
  # Ensure that time zero is included in the KM data (this anchors the curve at survival probability = 1)
  if (sum(rawkmfile$t == 0) == 0) {
    print("Time zero not present in KM data. Adding time = 0 and survival = 1.")
    new_row <- data.table(t = 0, p = 1)   # If missing, create a new row at time zero with survival = 1
    rawkmfile <- rbind(new_row, rawkmfile) # Add it to the top of the KM dataset
  } else {
    print("Time zero found in KM data. Setting survival at time zero to 1.")
    rawkmfile$p[which(rawkmfile$t == 0)] <- 1  # If time zero exists, make sure survival probability is set to 1
  }
  
  # Check if the number-at-risk table is provided
  if (!is.null(rawnriskfile)) {
    print("Number-at-risk data provided. Proceeding with alignment and full reconstruction.")
    
    # Align the number-at-risk data to the KM data using the helper function
    nrisk_bounds <- Nrisk_low_up(
      t.risk0 = rawnriskfile$Time,  # Times from the number-at-risk table
      t.S1 = rawkmfile$t,           # Times from the digitized KM curve
      n.risk0 = rawnriskfile$Nrisk  # Number-at-risk counts
    )
    
    # Perform the core reconstruction of patient-level data
    ipd_recon <- reconKMGuyot(
      tot.events = totev,                # Total events in the trial
      t.S = rawkmfile$t,                 # KM curve time points
      S = rawkmfile$p,                   # KM survival probabilities
      t.risk = nrisk_bounds$t.risk1,     # Cleaned number-at-risk times
      lower = nrisk_bounds$lower1,       # Lower bounds of survival time mapping
      upper = nrisk_bounds$upper1,       # Upper bounds of survival time mapping
      n.risk = nrisk_bounds$n.risk1,     # Cleaned number-at-risk counts
      tol = 0.01                         # Tolerance for convergence in reconstruction
    )
    print("Reconstruction using number-at-risk table completed.")
    # Return the reconstructed dataset as a list (including survival curve, at-risk table, and IPD)
    return(list(
      surv = rawkmfile,          # The processed KM data
      nrisk = rawnriskfile,      # Original number-at-risk table
      IPD = ipd_recon$ipd        # Reconstructed individual patient data (IPD)
    ))
    
  } else {
    # If no number-at-risk table is provided, reconstruct using only total population count
    print("Number-at-risk data NOT provided. Proceeding with total population count only.")
    
    ipd_recon <- reconKMGuyot(
      tot.events = totev,          # Total events
      t.S = rawkmfile$t,           # KM time points
      S = rawkmfile$p,             # KM survival probabilities
      t.risk = 0,                  # Placeholder since n.risk is not used here
      lower = 1,                   # Starting index for time mapping
      upper = length(rawkmfile$t), # Ending index
      n.risk = totp,               # Total number of patients (if no risk table)
      tol = 0.01                   # Tolerance for reconstruction
    )
    print("Reconstruction using total patient count completed.")
    # Return the reconstructed dataset (same structure)
    return(list(
      surv = rawkmfile,
      nrisk = rawnriskfile,
      IPD = ipd_recon$ipd
    ))
  }
}

process_KM_data <- function(
    digC,   # Digitized Kaplan-Meier curve data (data frame with time and survival probability)
    nRisk   # Number-at-risk table (data frame with time points, at-risk counts, and event count)
) {
  
  # Reconstruct individual patient data (IPD) using the wrapper function FUN_KM_RECON
  IPD_result <- FUN_KM_RECON(
    rawkmfile = digC,                 # Pass digitized KM data
    rawnriskfile = nRisk,             # Pass number-at-risk table
    totev = nRisk$`Number of Events`[1]  # Use the total event count from the first row of nRisk table
  )
  
  # Return the reconstructed IPD as a list (only the individual patient data portion)
  return(list(IPD = IPD_result$IPD))
}

