#' Create plots in the style of Lord (1953)
#'
#' This package replicates plots produced by Lord in his 1953 article, 
#' "The relation of test score to the trait underlying the test." 
#' This function takes item response theory parameters (currently 2-
#' and 3-parameter logistic) as the primary inputs and uses them to 
#' create plots relating a theoretical normal distribution (representing 
#' the latent trait and plotted on the x-axis) to the "true score" 
#' distribution (representing a large number of possible observed test 
#' results and plotted on the y-axis). 
#'
#' @param a numeric; item discrimination parameter(s), a vector of length n, where n is the number of test items.
#' @param b numeric; item difficulty/location parameter(s), a vector of length n, where n is the number of test items.
#' @param c numeric; item guessing parameter(s), either a single value or a vector of length n, where n is the number of test items.
#' @param D numeric; constant used to generate results that are roughly equal to the probit scale.
#' @param xlim numeric vector of length 2, giving the range of the x coordinates.
#' @param xby numeric; gives the size of histogram breaks along the x-axis.
#' @param main character; plot title.
#' @param xcol the color of the x-axis histogram. The x-axis label also defaults to this color if not specified.
#' @param ycol the color of the y-axis histogram. The y-axis label also defaults to this color if not specified.
#' @param xlabcol the color of the x-axis label (defaults to xcol).
#' @param ylabcol the color of the y-axis label (defaults to ycol).
#' @keywords item response theory
#' @export
#' @author Brandon E. Gavett, \email{bgavett@@uccs.edu}
#' @author Richard Jones, \email{richard_jones@@brown.edu}
#' @references 
#'     Lord, F. M. (1953). The relation of test score to the trait underlying the test. 
#'     Educational and Psychological Measurement, 4, 517-549.
#' @examples
#' ## An example using data and results from the ltm package.
#' library(ltm)
#' data(LSAT)
#' LSAT2PL <- coef(ltm(LSAT ~ z1))
#' lordplot(a = LSAT2PL[, 2], b = LSAT2PL[, 1])

lordplot <- function(a = c(.2,.4,.6), b = c(-2,-1,0), c = 0, D = 1.7, xlim = c(-4, 4), xby = .5, main = NULL, 
                     xcol = "lightgrey", ycol = "lightblue", xlabcol = xcol, ylabcol = ycol){
  
  if(abs(xlim[1]) == xlim[2]){
    if(xlim[1] < xlim[2]){
      if(length(a) == length(b)){
        if(length(c) == 1 | length(c) == length(a)){
          nitems <- length(a)
          
          xfrom <- xlim[1] #User inputs the lower value of the x axis (default -4)
          xto <- xlim[2] #User inputs the upper value of the x axis (default 4)
          rm(xlim) #Removes the xlim object
          
          xbreaks <- seq(xfrom, xto, xby) # set user-specified (default .5) sd breaks on the normal curve
          nbreaks <- length(xbreaks) # count how many breaks we have
          
          #create the bin widths for the histogram
          binsizes <- rep(0, nbreaks-1) # create an empty array of zeroes to contain bin sizes
          nprob <- pnorm(xbreaks) # obtain probabilities for each bin
          binsizes[1] <- nprob[1] # generate the first bin size, from -inf to -4
          for(i in 1:(nbreaks-1)){ #generate the next 11 bin sizes
            binsizes[i] <- nprob[i+1] - nprob[i]
          } 
          
          # create a data frame with coordinates for lower histogram. 
          # xs are the left and right boundaries of each bin
          lower_hist <- data.frame(x1 = xbreaks[1:(nbreaks - 1)],
                                   x2 = xbreaks[2:nbreaks])
          lower_hist$y1 <- 0 # all bins start at y = 0
          lower_hist$y2 <- -binsizes/max(binsizes) # scale so that the peak is a value of 1, to fit in window
          lower_hist$x1 <- seq(0, 2, length = nbreaks)[1:(nbreaks - 1)] # scale to fit
          lower_hist$x2 <- seq(0, 2, length = nbreaks)[2:nbreaks] # scale to fit
          
          par(mar = c(0, 0, 0, -1) + 2) #set up the plotting area
          plot(0, 0, xlim = c(-1, 2), ylim = c(-1, 2), col = "white", axes = F, main = main, xlab = "Latent trait") #plot a blank window
          rect(lower_hist$x1, lower_hist$y1, lower_hist$x2, lower_hist$y2, col = xcol) # plot the lower histogram
          theta <- seq(xfrom, xto, .001) # x-axis
          den_y <- dnorm(theta)
          mids <- seq(mean(xbreaks[1:2]), mean(tail(xbreaks, 2)), length = length(theta))
          den_y_s <- den_y*diff(mids[1:2])*length(theta)
          den_y_s <- -den_y_s/max(den_y_s)
          lines(seq(0, 2, length = length(den_y_s)), den_y_s, type = "l", lty = 2)
          
          
          #Set up the TCC and do some modifications so it fits in the window correctly
          
          irtfunction <- function(THETA = theta){
            p <- matrix(nrow = nitems, ncol = length(THETA))
            if(length(c) == 1){
              for(i in 1:nitems){
                p[i,] <- c + ((1 - c)/(1 + exp(-D * a[i] * (THETA - b[i]))))
              }
            }
            if(length(c) > 1){
              for(i in 1:nitems){
                p[i,] <- c[i] + ((1 - c[i])/(1 + exp(-D * a[i] * (THETA - b[i]))))
              }
            }
            return(p)
          }
          simndat <- rnorm(length(theta))
          escore <- colSums(irtfunction(THETA = simndat))
          escore <- escore[order(escore)]
          prob <- colSums(irtfunction(THETA = theta))/nitems
          ogive <- data.frame(x = theta, y = prob) # x and y
          squished <- ogive # store in new variable
          squished$x <- 1 + ogive$x/max(theta) #rescale to fit
          squished$x <- squished$x - min(squished$x) # shift to origin of 0
          squished$y <- squished$y*2 # rescale to fit
          
          # Draw the lines
          lines(squished)
          
          
          # Set up & draw the dotted lines
          origin <- 2*(colSums(irtfunction(THETA = xbreaks))/nitems)
          vlines <- data.frame(x1 = seq(0, 2, length = length(origin)), x2 = seq(0, 2, length = length(origin)), 
                               y1 = 0, y2 = origin)
          segments(vlines$x1, vlines$y1, vlines$x2, vlines$y2, lty = 2)
          insertion <- data.frame(y1 = origin, y2 = origin, 
                                  x1 = 0, x2 = seq(0, 2, length = length(origin)))
          segments(insertion$x1, insertion$y1, insertion$x2, insertion$y2, lty = 2)
          
          
          # Set up & draw the left histogram - this is very rough. 
          # The _x suffix on object names and the while() function
          # were used to deal with extreme cases (like a = 5, b = 0)
          # in order to reduce the number of bins on the true score axis
          # to properly visualize the u-shape. 
          z_obs <- 2*escore/nitems
          side_breaks <- c(0, origin, 2)
          side_breaks <- side_breaks[order(side_breaks)]
          nside_breaks <- length(side_breaks)
          side_break_width <- c(1, diff(side_breaks, lag = 1))
          side_breaks_x <- side_breaks
          side_break_x_width <- side_break_width
          while(min(side_break_x_width) < .01){
            side_breaks_x <- side_breaks_x[side_break_x_width != min(side_break_x_width)]
            side_break_x_width <- c(1, diff(side_breaks_x, lag = 1))
          }
          
          nside_breaks_x <- length(side_breaks_x)
          side_hist <- data.frame(y1 = side_breaks_x[1:(nside_breaks_x-1)],
                                  y2 = side_breaks_x[2:nside_breaks_x])
          side_hist$y2[nside_breaks_x-1] <- 2
          side_hist$x1 <- 0
          side_hist$x2 <- 0
          side_hist$x2[1] <- sum(z_obs <= side_hist$y2[1])
          side_hist$x2[nside_breaks_x-1] <- sum(z_obs > side_hist$y2[nside_breaks_x-1])
          for(i in 2:(nside_breaks_x-1)){
            side_hist$x2[i] <- sum(z_obs <= side_hist$y2[i] & z_obs > side_hist$y1[i])
          }
          side_hist$x2 <- -side_hist$x2/max(side_hist$x2)
          rect(side_hist$x1, side_hist$y1, side_hist$x2, side_hist$y2, col = ycol)
          
          xticks <- seq(0, 2, length = (nbreaks+1)/2)
          xlabs <- xbreaks[c(TRUE,FALSE)]
          text(xticks, 0, xlabs, pos = 1, cex = .75)
          yticks <- seq(0, 2, .5)
          ylabs <- seq(0, 1, .25)
          text(0, yticks, ylabs, pos = 2, cex = .75)
          lines(c(0,0), c(0,2))
          text(-1, 2, "True Score", pos = 4, col = ylabcol)
          text(2, -.5, "Ability", pos = 2, col = xlabcol)
        } else stop("c parameter should be a single value or equal in length to the a and b parameters.")
      } else stop("a and b parameters are unequal in length.")
    } else stop("Cannot reverse x-axis.")
  } else stop("Only symmetrical x-axis limits currently supported.")
}