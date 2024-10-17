# Define a custom function to calculate many summary statistics
multi_sum_stats <- function(x, na.rm = TRUE) {
  # This calculates multiple summary statiscs
  # Quartiles
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm)
  q1 <- q[1]
  q2 <- q[2] # This is the median
  q3 <- q[3]
  
  # Handle quartile means by dividing data into 4 groups
  q1_mean <- mean(x[x <= q1], na.rm = na.rm)
  q2_mean <- mean(x[x > q1 & x <= q2], na.rm = na.rm)
  q3_mean <- mean(x[x > q2 & x <= q3], na.rm = na.rm)
  q4_mean <- mean(x[x > q3], na.rm = na.rm)
  
  # Return a named vector
  c(
    mean = mean(x, na.rm = na.rm),
    median = median(x, na.rm = na.rm),
    q1_mean = q1_mean,
    q2_mean = q2_mean,
    q3_mean = q3_mean,
    q4_mean = q4_mean,
    sd = sd(x, na.rm = na.rm),
    iqr = IQR(x, na.rm = na.rm)
  )
}
