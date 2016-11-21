# ------------------------------------------------------------------------------
# Book: SPM - Nonparametric and Semiparametric Modelling
# ------------------------------------------------------------------------------
# Quantlet: SPMhisdiffori
# ------------------------------------------------------------------------------
# Description: SPMhisdiffori computes histograms with different origins.
# ------------------------------------------------------------------------------
# Usage: -
# ------------------------------------------------------------------------------
# Inputs: x0 - origin, 1x4 vector h - binwidth
# ------------------------------------------------------------------------------
# Output: Four histograms for the stock returns data corresponding to different
# origins.
# ------------------------------------------------------------------------------
# Keywords: histogram, binwidth,
# ------------------------------------------------------------------------------
# See also: SPMhistogram, SPMhistmse, SPMhisdiffbin, SPMashstock, SPMhiststock
# ------------------------------------------------------------------------------
# Example: An example is produced for x0=c(0,0.01,0.02,0.03) and h=0.04.
# ------------------------------------------------------------------------------
# Author: Marlene Mueller, 2008/11/21
# ------------------------------------------------------------------------------

# Close windows and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# Read dataset
x = read.csv("C:/.../stockres.txt")  #Change directory
x = unlist(x)

# Input
h  = 0.04  #binwidth
x0 = c(0, 0.01, 0.02, 0.03)  #origin, 1x4 vector

# Select bins
breaks = function(x, x0, h) {
    b = floor((min(x) - x0)/h):ceiling((max(x) - x0)/h)
    b = b * h + x0
    return(b)
}

# Plot histograms
par(mfrow = c(2, 2))
hist(x, freq = FALSE, breaks = breaks(x, x0[1], h = 0.04), main = c(paste("x0 =", 
    x0[1]), paste("h =", h)), xlab = "x", ylab = "fh", ylim = c(0, 17))
hist(x, freq = FALSE, breaks = breaks(x, x0[2], h = 0.04), main = c(paste("x0 =", 
    x0[2]), paste("h =", h)), xlab = "x", ylab = "fh", ylim = c(0, 17))
hist(x, freq = FALSE, breaks = breaks(x, x0[3], h = 0.04), main = c(paste("x0 =", 
    x0[3]), paste("h =", h)), xlab = "x", ylab = "fh", ylim = c(0, 17))
hist(x, freq = FALSE, breaks = breaks(x, x0[4], h = 0.04), main = c(paste("x0 =", 
    x0[4]), paste("h =", h)), ylab = "fh", ylim = c(0, 17)) 