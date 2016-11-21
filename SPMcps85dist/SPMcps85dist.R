## ----------------------------------------------------------------------------
## R scripts for Haerdle/Mueller/Sperlich/Werwatz: 'Nonparametric
## and Semiparametric Modelling', Springer Series in Statistics,
## 2004
## ----------------------------------------------------------------------------
## Name of QuantLet: SPMcps85dist
## ----------------------------------------------------------------------------
## Description: estimates the distribution of log(wages) from the
## 1985 CPS (see Berndt, 1991). Confidence bands and intervals for
## the kernel density estimate can be compared.
## ----------------------------------------------------------------------------
## Author: Marlene Mueller, 2010/04/29
## ----------------------------------------------------------------------------

graphics.off()
rm(list = ls(all = TRUE))

# set (or change) your working directory setwd('C:/...')

install.packages("KernSmooth")
library(KernSmooth)

x  = read.table("cps85.dat")
x  = x$V11  # log wages
xe = exp(x)

# choose parameters
h      = 5
alpha  = 0.05
kernel = "biweight"

# cK is the squared L2 norm for K (||K||_2^2) mK is the second
# moment (mu2(K)) c2 is the ratio ||K'||_2/||K||_2

canonical = data.frame(kernel = c("box", "trian", "epanech", "biweight", 
    "triweight", "normal", "cosi"), cK = c((0.5), (2/3), (0.6), (5/7), 
    (350/429), (1/(2 * sqrt(pi))), (pi^2/16)), mK = c((1/3), (1/6), 
    (0.2), (1/7), (1/9), (1), (1 - 8 * pi^2)), c2 = c(NaN, NaN, NaN, 
    1.5, NaN, NaN, NaN))

fh   = data.frame(bkde(xe, bandwidth = h, kernel = kernel, range.x = range(xe)))
fh$y = replace(fh$y, fh$y < 0, 0)

fl   = data.frame(x = fh$x, y = dlnorm(fh$x, mean(x), sd(x)))

cK   = canonical$cK[canonical$kernel == kernel]
c2   = canonical$c2[canonical$kernel == kernel]

calpha = qnorm(1 - alpha/2)
dev    = calpha * sqrt(cK * fh$y/(length(x) * h))
# confidence intervals
fh.ci  = data.frame(x = fh$x, up = fh$y + dev, low = fh$y - dev)

mx     = min(xe)
rx     = diff(range(xe))
fr     = data.frame(bkde((xe - mx)/rx, bandwidth = h/rx, kernel = kernel, 
                    range.x = c(0, 1)))
fr$y   = replace(fr$y, fr$y < 0, 0)
r      = sqrt(-2 * log(h/rx))
dn     = r + 0.5 * log(c2/(2 * (pi^2)))/r

calpha = -log(-log(1 - alpha)/2)
dev    = (calpha/r + dn) * sqrt(cK * fr$y/(length(xe) * h))
# confidence bands
fh.cb  = data.frame(x = rx * fr$x + mx, up = (fr$y + dev)/rx, low = (fr$y - dev)/rx) 

ylim   = range(c(fh$y, fl$y, fh.ci$up, fh.cb$up))

par(mfrow = c(2, 2))
plot(fh, type = "l", lwd = 3, ylim = ylim, main = "Lognormal and Kernel Density", 
    xlab = "Wages", ylab = "Density")
lines(fl, col = "red", lty = 2)

rug(xe)
plot(fh, type = "l", lwd = 3, ylim = ylim, main = "Lognormal, KDE and Confidence Intervals", 
    xlab = "Wages", ylab = "Density")
lines(fl, col = "red", lty = 2)
lines(fh.ci$x, fh.ci$up, col = "blue", lty = 2)
lines(fh.ci$x, fh.ci$low, col = "blue", lty = 2)

rug(xe)
plot(fh, type = "l", lwd = 3, ylim = ylim, main = "Lognormal, KDE and Confidence Bands", 
    xlab = "Wages", ylab = "Density")
lines(fl, col = "red", lty = 2)
lines(fh.cb$x, fh.cb$up, col = "green")
lines(fh.cb$x, fh.cb$low, col = "green")

rug(xe)
plot(fh, type = "l", lwd = 3, ylim = ylim, main = "Confidence Intervals vs. Bands", 
    xlab = "Wages", ylab = "Density")
lines(fh.ci$x, fh.ci$up, col = "blue", lty = 2)
lines(fh.ci$x, fh.ci$low, col = "blue", lty = 2)
lines(fh.cb$x, fh.cb$up, col = "green")
lines(fh.cb$x, fh.cb$low, col = "green")

rug(xe)
par(mfrow = c(1, 1))