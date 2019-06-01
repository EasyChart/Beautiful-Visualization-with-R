# LV summary table
# Create letter value summary table
# 
# @arguments numeric vector
# @arguments quantiles to compute
# @arguments number of letter statistics
# @arguments list of outliers
# @arguments depth of the corresponding LV statistic (i.e. how far from the outside do we have to go into the sorted data values?)
# @arguments significance level
# @value letter.val: letter value statistic, distinguishes between upper and lower LV statistic for all statistics but the median
# @value conf.int: confidence interval of corresponding letter value statistic
# @value out: list of defined outliers
# @keyword internal 
outputLVplot <- function(x,qu,k,out,depth,alpha) {
  low <- depth - floor(0.5 *sqrt(2*depth-1) * qnorm(alpha+(1-alpha)/2))
  high <- depth + ceiling(0.5 *sqrt(2*depth-1) * qnorm(alpha+(1-alpha)/2))
  n <- length(x)
  LV <- cbind(depth,lower=qu[k:1],upper=qu[k+1:k])
  y <- sort(x)
  conf <- cbind(c(y[rev(low[-1])],y[n-high]),c(y[rev(high[-1])],y[n-low]))
  colnames(conf) <- c(paste((1-alpha)/2*100,"%",sep=""),paste((alpha+(1-alpha)/2)*100,"%",sep=""))
  if (k > 1) {
    which <- (((k-1):1 + (6-k)) %% 26) + 1
    row.names(LV) <- c('M',toupper(letters[which]))
    row.names(conf) <- c(paste(toupper(letters[rev(which)]),"l",sep=""),'M',paste(toupper(letters[which]),"u",sep=""))
  } 
  if (k == 1) {
    row.names(LV) <- 'M'
    row.names(conf) <- 'M'
  }

  result <- list(letter.val = LV, conf.int= conf,outliers = x[out])
  return(result)
}

# Draw an LV plot 
# Draw a letter value boxplot
# 
# @arguments x positions
# @arguments y positions
# @arguments number of letter value statistics used
# @arguments out: outliers
# @arguments quantiles
# @arguments display horizontally (TRUE) or vertically (FALSE)
# @arguments vector of colours to use
# @keyword internal
drawLVplot <- function(x,y,k,out,qu,horizontal,col,...) {
  if (horizontal) { 
	points(x[out],rep(y,length(x[out])),pch=8)		
	# draw boxes:
	for (i in 1:k) 
		rect(qu[i], y+i/(2*k),qu[2*k-i+1], y-i/(2*k), col=col[i])
  } else { # draw vertical plot
	points(rep(y,length(x[out])),x[out],pch=8)						 
	# draw boxes:
	for (i in 1:k) 
		rect(y+i/(2*k),qu[i], y-i/(2*k), qu[2*k-i+1], col=col[i])
  }
}
