# Determine depth
# Determine number of letter values needed for n observations
# 
# @arguments number of observation to be shown in the LV boxplot
# @arguments number of letter value statistics used 
# @arguments if defined, depth k is calculated such that confidence intervals of an LV statistic do not extend into neighboring LV statistics
# @arguments if defined, depth k is adjusted such that \code{perc} percent outliers are shown
# @keyword internal 
determineDepth <- function(n, k, alpha,  perc) {
  if (!is.null(perc)) {
  	# we're aiming for perc percent of outlying points
  	k <- ceiling((log2(n))+1) - ceiling((log2(n*perc*0.01))+1)+1
  }
  if (is.null(k)) { 
  	# confidence intervals around an LV statistic 
  	# should not extend into surrounding LV statistics

  	k <- ceiling((log2(n))-log2(4*qnorm(alpha+(1-alpha)/2)^2))  
  }
  if (k < 1) k <- 1	
 
  return (k)
}
