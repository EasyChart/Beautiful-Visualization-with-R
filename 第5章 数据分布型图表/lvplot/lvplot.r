# LV box plot
# An extension of standard boxplots which draws k letter statistics
# 
# This is a generic method so please see specific methods for details.
#
# @seealso \code{\link{LVboxplot.formula}}, \code{\link{LVboxplot.numeric}}
# @keyword internal
LVboxplot <- function(x, ...) UseMethod("LVboxplot",x)

# Side-by-side LV boxplots
# An extension of standard boxplots which draws k letter statistics
# 
# Conventional boxplots (Tukey 1977) are useful displays for conveying rough information
# about the central 50\% of the data and the extent of the data.
# 
# For moderate-sized data sets ($n < 1000$), detailed estimates of tail behavior beyond
# the quartiles may not be trustworthy, so the information provided by boxplots is
# appropriately somewhat vague beyond the quartiles, and the expected number of
# ``outliers'' and ``far-out'' values for a Gaussian sample of size $n$ is often less
# than 10 (Hoaglin, Iglewicz, and Tukey 1986). Large data sets ($n \approx
# 10,000-100,000$) afford more precise estimates of quantiles in the tails beyond the
# quartiles and also can be expected to present a large number of ``outliers'' (about 0.4
# + 0.007$n$).
# 
# The letter-value box plot addresses both these shortcomings: it conveys more detailed
# information in the tails using letter values, only out to the depths where the letter
# values are reliable estimates of their corresponding quantiles (corresponding to tail
# areas of roughly $2^{-i}$); ``outliers'' are defined as a function of the most extreme
# letter value shown. All aspects shown on the letter-value boxplot are actual
# observations, thus remaining faithful to the principles that governed Tukey's original
# boxplot.
# 
# @arguments the formula has to be of the form $y \tilde x$, where $x$ is a qualitative variable. The values of $y$ will be split into groups according to their values on $x$ and separate letter value box plots of $y$ are drawn side by side in the same display.
# @arguments significance level, if neither \code{k} nor \code{perc} is specified, \code{alpha} is used to determine how many letter values are to be used.
# @arguments percentage of data points to be shown individually (as outliers) outside the letter-value boxes. \code{perc} is only used, if \code{k} is not specified.  If used, $k$ is determined in such a way, that confidence intervals around each letter value statistics will not include neighboring letter value statistics at a significance level of \code{alpha}.
# @arguments number of letter statistics to compute and draw
# @arguments if defined, aim for \code{perc} percent outliers
# @arguments display horizontally (TRUE) or vertically (FALSE)
# @arguments specify base colour to use
# @arguments unused
# @keyword hplot
# @seealso \code{\link{LVboxplot.numeric}}
#X n <- 10
#X oldpar <- par()
#X par(mfrow=c(4,2), mar=c(3,3,3,3))
#X for (i in 1:4) {
#X 	x <- rexp(n*10^i)
#X 	boxplot(x,col="grey", horizontal=TRUE)
#X 	title(paste("Exponential, n=",length(x)))
#X 	LVboxplot(x,col="grey", xlab="")
#X }
LVboxplot.formula <- function(formula,alpha=0.95, k=NULL, perc=NULL,horizontal=TRUE,col="grey",...) {
    deparen <- function(expr) {
        while (is.language(expr) && !is.name(expr) && deparse(expr[[1]]) == 
            "(") expr <- expr[[2]]
        expr
    }
    bad.formula <- function() stop("invalid formula; use format y ~ x")
    bad.lengths <- function() stop("incompatible variable lengths")
    
    formula <- deparen(formula)
    if (!inherits(formula, "formula")) 
        bad.formula()
    z <- deparen(formula[[2]])
    x <- deparen(formula[[3]])
    rhs <- deparen(formula[[3]])
    if (is.language(rhs) && !is.name(rhs) && (deparse(rhs[[1]]) == 
        "*" || deparse(rhs[[1]]) == "+")) {
        bad.formula()
    }
    z.name <- deparse(z)
    z <- eval(z,  parent.frame())
    x.name <- deparse(x)
    x <- eval(x,  parent.frame())
	setx <- sort(unique(x))
    src.k <- k 
    src.col <- col 


    pt <- 1
	if (horizontal) {
	      
	  plot(z, rep(pt,length(z)),
	    ylim = c(0.5, length(setx) + .5), ylab = "",axes = FALSE,type = "n",...)
	  box()
	  axis(1)
	  axis(2,at=1:length(setx),labels=as.character(setx))
	} else {
	  plot(rep(pt,length(z)), z , xlim = c(0.5,length(setx)+0.5), xlab="", axes=FALSE, type="n", ...)
	  box()
	  axis(2)
	  axis(1,at=1:length(setx),labels=as.character(setx))
	}
	
	result <- list(length(setx))
    for (i in setx) {
       xx <- z[x==i]
	   n <- length(xx)
	   k <- determineDepth(n,src.k,alpha,perc) 

	   if (! is.na(src.col)) { 
	   		#col <- c(brewer.pal(k-1,"Blues"),"Black") # break dependency of ColorBrewer package
	   		
	   		colrgb <- col2rgb(src.col)
	   		colhsv <- rgb2hsv(colrgb)
			if (colhsv[2,1] == 0) {
	   			val <- seq(0.9,colhsv[3,1], length.out=k)
	   			colrgb <- col2rgb(hsv(colhsv[1,1], colhsv[2,1], val))
			} else {
	   			sat <- seq(0.1,colhsv[2,1], length.out=k)
	   			colrgb <- col2rgb(hsv(colhsv[1,1], sat, colhsv[3,1]))
			}
	   		col <- rgb(colrgb[1,],colrgb[2,],colrgb[3,], maxColorValue=255) 	
	   	}
	   else { col <- rep("grey",k) }
	   
	 # compute letter values based on depth  
	   depth    <- rep(0,k)
	   depth[1] <- (1 + n)/2
	   if (k > 1) {
		 for (j in 2:k) {depth[j] <- (1 + floor(depth[j-1]))/2 } 
	   }
	   
	   y <- sort(xx)
	   d <- c(rev(depth),n-depth+1)
	   qu <- (y[floor(d)] + y[ceiling(d)])/2 	
		 # floor and ceiling is the same for .0 values
		 # .5 values yield average of two neighbours
	   
	 # determine outliers
	   out <- (xx<qu[1]) | (xx>qu[2*k])            
		  
	   drawLVplot(xx,pt,k,out,qu,horizontal,col,...)
	   result[[pt]] <- outputLVplot(xx,qu,k,out,depth,alpha)      
	   pt <- pt+1
    }
    invisible(as.list(result))
}

# Single LV boxplot
# Produces a single lettervalue boxplot for the specified data.
# 
# @arguments alpha level for significance level: alpha 100\% confidence intervals do not touch neighboring LV statistics
# @arguments number of letter statistics to compute and draw
# @arguments if defined, aim for \code{perc} percent outliers
# @arguments display horizontally (TRUE) or vertically (FALSE)
# @arguments specify base colour to use
# @arguments unused
# @keyword hplot
# @seealso \code{\link{LVboxplot.formula}}
LVboxplot.numeric <- function(x,alpha=0.95, k=NULL, perc=NULL,horizontal=TRUE,col="grey",...) {
# extension of standard boxplots
# draws k letter statistics

  n <- length(x)
  k <- determineDepth(n,k,alpha,perc) 
  src.col <- col 

  if (! is.na(src.col)) { #col <- c(brewer.pal(k-1,"Blues"),"Black") 
 	   		
	   		colrgb <- col2rgb(src.col)
	   		colhsv <- rgb2hsv(colrgb)
			if (colhsv[2,1] == 0) {
	   			val <- seq(0.9,colhsv[3,1], length.out=k)
	   			colrgb <- col2rgb(hsv(colhsv[1,1], colhsv[2,1], val))
			} else {
	   			sat <- seq(0.1,colhsv[2,1], length.out=k)
	   			colrgb <- col2rgb(hsv(colhsv[1,1], sat, colhsv[3,1]))
			}
	   		col <- rgb(colrgb[1,],colrgb[2,],colrgb[3,], maxColorValue=255) 	
  }
  else { col <- rep("grey",k) }
  
# compute letter values based on depth  
  depth    <- rep(0,k)
  depth[1] <- (1 + n)/2
  if (k > 1) {
    for (j in 2:k) {depth[j] <- (1 + floor(depth[j-1]))/2 } 
  }
  
  y <- sort(x)
  d <- c(rev(depth),n-depth+1)
  qu <- (y[floor(d)] + y[ceiling(d)])/2 	
    # floor and ceiling is the same for .0 values
  	# .5 values yield average of two neighbours
  
# determine outliers
  out <- (x<qu[1]) | (x>qu[2*k])               
  if (k < 1) out <- x

  pt <- 0.5
  if (horizontal) {
    plot(x,rep(pt,length(x)),ylim=c(pt-0.5,pt+0.5),ylab="",axes=FALSE,type="n",...)
	box()
	axis(1)
  } else {
	plot(rep(pt,length(x)),x,xlim=c(pt-0.5,pt+0.5), xlab="", axes=FALSE, type="n", ...)
	box()
	axis(2)
  }
     
  drawLVplot(x,pt,k,out,qu,horizontal,col,...)

  result <- outputLVplot(x,qu,k,out,depth,alpha)
  invisible(result)
}
