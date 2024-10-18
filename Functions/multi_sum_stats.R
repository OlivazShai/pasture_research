# Define a custom function to calculate many summary statistics
multi_sum_stats <- function(x, na.rm = TRUE) {
  # Filter out NA values to avoid issues
  x <- x[!is.na(x)]
  
  # If no data left after removing NA, return all NA values
  if (length(x) == 0) {
    return(c(
      mean = NA, median = NA, 
      q1_mean = NA, q2_mean = NA, 
      q3_mean = NA, q4_mean = NA, 
      sd = NA, iqr = NA
    ))
  }
  
  # Quartiles
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm)
  q1 <- q[1]
  q2 <- q[2] # Median
  q3 <- q[3]
  
  # Handle quartile means, but check if each subset has values
  # The equals to are important to deal with 0s 
  q1_vals <- x[x <= q1]
  q2_vals <- x[x >= q1 & x <= q2]
  q3_vals <- x[x >= q2 & x <= q3]
  q4_vals <- x[x >= q3]
  
  # Calculate means for each quartile, but return NA if no values
  q1_mean <- if (length(q1_vals) > 0) mean(q1_vals, na.rm = na.rm) else NA
  q2_mean <- if (length(q2_vals) > 0) mean(q2_vals, na.rm = na.rm) else NA
  q3_mean <- if (length(q3_vals) > 0) mean(q3_vals, na.rm = na.rm) else NA
  q4_mean <- if (length(q4_vals) > 0) mean(q4_vals, na.rm = na.rm) else NA
  
  # Return the summary statistics, handling edge cases
  c(
    mean = mean(x, na.rm = na.rm),
    median = median(x, na.rm = na.rm),
    q1_mean = q1_mean,
    q2_mean = q2_mean,
    q3_mean = q3_mean,
    q4_mean = q4_mean,
    sd = if (length(x) > 1) sd(x, na.rm = na.rm) else NA, # sd needs at least 2 values
    iqr = if (length(x) > 1) IQR(x, na.rm = na.rm) else NA # iqr also needs at least 2 values
  )
}

