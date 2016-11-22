# ----------------------------------------------------------------------------
# Book:         SPM
# ----------------------------------------------------------------------------
# Usage:        -
# ----------------------------------------------------------------------------
# See also:     -
# ----------------------------------------------------------------------------
# Quantlet:     SPMwavereg
# ----------------------------------------------------------------------------
# Description:  computes the wavelet regression using Daubechies basis for a 
#               simulated data set.
# ----------------------------------------------------------------------------
# Author:       Awdesch Melzer 20130319
# ----------------------------------------------------------------------------

rm(list = ls(all = TRUE))
graphics.off()

# load library
install.packages("waveslim")
library(waveslim)

# set pseudo random numbers
set.seed(117117)


n     = 256  # number of observations
x     = seq(0, by = 2 * pi/(n - 1), length = n)  # grid
m     = (x > pi) + sin(x^2) * (x <= pi)  # true funtion
y     = m + rnorm(n)/4  # add noise
y.dwt = dwt(y, wf = "d8", 4)  # discrete wavelet transform
# inverse dwt, with manual threshold 1.9 for smoothness:
mhat  = idwt(manual.thresh(y.dwt, 5, 1.9, hard = F))[1:n]  

# plot 
# To save the plot uncomment pdf(...) and dev.off()
# pdf('SPMwavereg.pdf', height = 6, width = 8)

plot(x, y, type = "n", ylab = "Y", xlab = "X")
title("Wavelet Regression")
points(x, y, pch = 19, col = "blue3", cex = 0.5)
lines(x, mhat, col = "black", lwd = 2.3)
lines(x, m, col = "blue3", lwd = 1.5) 
