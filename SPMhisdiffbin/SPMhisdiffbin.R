# ------------------------------------------------------------------------------
# Book:        SPM - Nonparametric and Semiparametric Modelling
# ------------------------------------------------------------------------------
# Quantlet:    SPMhisdiffbin
# ------------------------------------------------------------------------------
# Description: SPMhisdiffbin computes histograms with different binwidths           
# ------------------------------------------------------------------------------
# Usage:       -
# ------------------------------------------------------------------------------
# Inputs:      x0 - origin
#              h  - binwidth
# ------------------------------------------------------------------------------
# Output:      Plot four histograms for the stock returns data with different 
#              bindwidths.
# ------------------------------------------------------------------------------
# Keywords:    histogram, binwidth,
# ------------------------------------------------------------------------------
# See also:    SPMhistogram, SPMhistmse, SPMhisdiffori, SPMashstock, 
#              SPMhiststock
# ------------------------------------------------------------------------------
# Example:     -
# ------------------------------------------------------------------------------
# Author:      Marlene Mueller, 2008/11/21
# ------------------------------------------------------------------------------

# Close windows and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# Read dataset
x = read.csv("C:/.../stockres.txt")  #Change directory
x = unlist(x)

# Input
h  = c(0.007, 0.02, 0.05, 0.1)  #binwidth, 1x4 vector
x0 = 0  #origin

# Select bins
breaks = function(x, x0, h) {
    b = floor((min(x) - x0)/h):ceiling((max(x) - x0)/h)
    b = b * h + x0
    return(b)
}

# Plot histograms
par(mfrow = c(2, 2))
hist(x, freq = FALSE, breaks = breaks(x, x0 = 0, h[1]), main = c(paste("x0 =", 
    x0), paste("h =", h[1])), xlab = "x", ylab = "fh", ylim = c(0, 17))
hist(x, freq = FALSE, breaks = breaks(x, x0, h[2]), main = c(paste("x0 =", 
    x0), paste("h =", h[2])), xlab = "x", ylab = "fh", ylim = c(0, 17))
hist(x, freq = FALSE, breaks = breaks(x, x0, h[3]), main = c(paste("x0 =", 
    x0), paste("h =", h[3])), xlab = "x", ylab = "fh", ylim = c(0, 17))
hist(x, freq = FALSE, breaks = breaks(x, x0, h[4]), main = c(paste("x0 =", 
    x0), paste("h =", h[4])), xlab = "x", ylab = "fh", ylim = c(0, 17)) 
