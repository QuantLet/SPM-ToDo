# ----------------------------------------------------------------------------
# Book: SPM
# ----------------------------------------------------------------------------
# See_also: 
# ----------------------------------------------------------------------------
# Quantlet: SPMhistmse
# ----------------------------------------------------------------------------
# Description: SPMhistmse calculates MSE, Bias^2 and Variance 
# (asymptotic formulas) of a histogram at a fixed point x0. 
# The origin of the histogram is set to 0. 
# The true density is f(x)=2/3 *{(1+x/2)*I[-2,0)+(1-x)*[0,1)}.  
# ----------------------------------------------------------------------------
# Author: Awdesch Melzer 20121029
# ----------------------------------------------------------------------------

rm(list = ls(all = TRUE))
graphics.off()

x0  = 0.5  # point in which we calculate the histogram

h   = seq(0.02, 0.219, 0.001)  # grid of binwidths
n   = 1000  # number of observations

j   = ceiling(x0/h)  # histogram intervals
mj  = (j - 1/2) * h  # centers of intervals

ff  = (2/3) * ((x0/2 + 1) * as.numeric(-2 <= x0) * as.numeric(x0 < 0) + (1 - x0) * 
    as.numeric(0 <= x0) * as.numeric(x0 < 1))  # true density

fj  = (2/3) * (0.5 * as.numeric(-2 <= mj) * as.numeric(mj < 0) - 1 * as.numeric(0 <= 
    mj) * as.numeric(mj < 1))  # its derivative in center of intervals
b   = fj * (mj - x0)  # bias formula: 
b2  = b^2
v   = ff * 1/(n * h)  # variance formula
mse = b2 + v  # MSE formula

# plotting

plot(h, v, type = "l", lty = "dashed", col = "red3", 
    lwd = 2.5, ylab = "Bias^2, Variance and MSE", 
    xlab = "Bandwidth h", ylim = c(0, 0.017))
title("Bias^2, Variance and MSE")
lines(h, b2, col = "blue3", lwd = 2.5)
lines(h, mse, lwd = 2.5)