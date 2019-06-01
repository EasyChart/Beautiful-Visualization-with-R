vase <- function(x, ..., names = NULL, bw = NULL) {


  all.x <- c(x, list(...))

  centers <- seq_along(all.x)
  n <- length(all.x)
  if (is.null(names)) {
    names <- names(all.x)
  }

  xmin <- 0.5
  xmax <- length(all.x) + 0.5

  ymin <- min(unlist(all.x), na.rm = TRUE)
  ymax <- max(unlist(all.x), na.rm = TRUE)

  #plot(c(xmin, xmax), c(0,0.5), type = "n", main = "",
   #       xlab = "Class", ylab = "Value", xaxt = "n", yaxt = "n")

 plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = "",
          xlab = "", ylab = "", xaxt = "n", yaxt = "n")


  for(i in 1:n){

    lower <- quantile(all.x[[i]], probs = .25)
    upper <- quantile(all.x[[i]], probs = .75)
    Uex <- quantile(all.x[[i]], probs = .95)
    Lex <- quantile(all.x[[i]], probs = 0.05)
    Hspread <- (upper - lower)[[1]]
    step <- 1.5*Hspread[[1]]
	median <- median(all.x[[i]])
	###tukey definition of whiskers
	TUex <- max(all.x[[i]][all.x[[i]] <= upper+step])
	TLex <- min(all.x[[i]][all.x[[i]] >= lower-step])

    Xs <- density(all.x[[i]], bw = bw[i])$x
    Ys <- density(all.x[[i]], bw = bw[i])$y

    in_box <- Xs < upper & Xs > lower
    Xs <- c(lower, Xs[in_box], upper)
    Ys <- c(0, Ys[in_box], 0)

    # Scale to 0.4
    Ys <- Ys / max(Ys) * 0.4

	outliers <- which( (all.x[[i]] > upper+step) |
     				   (all.x[[i]] < lower-step) )

    if (length(outliers) > 0) {
      segments(centers[i], upper+step, centers[i], upper, col = "black")
      segments(centers[i], lower-step, centers[i], lower, col = "black")

      points(rep(centers[i], length(outliers)), all.x[[i]][outliers], cex = 0.5)

    }

	###body
    lines(centers[i]+Ys, Xs)
    lines(centers[i]-Ys, Xs)

	###whiskers with Tukey definition
    segments(centers[[i]], TLex, centers[[i]], lower)
    segments(centers[[i]], TUex, centers[[i]], upper)
    #segments(centers[[i]], Lex, centers[[i]], lower)
    #segments(centers[[i]], Uex, centers[[i]], upper)

    ###plot a line for the median
    pos <- which.min(abs(Xs - median))[1]
    segments((centers[i]-Ys)[pos], Xs[pos], (centers[i]+Ys)[pos], Xs[pos],
      col = gray(0.25))

  }
  axis(1, at = centers, labels = names)
  axis(2)

  print(centers)
}
