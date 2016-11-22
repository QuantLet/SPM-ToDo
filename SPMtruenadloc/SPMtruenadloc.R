# ----------------------------------------------------------------------------
# Book: SPM
# ----------------------------------------------------------------------------
# Usage: -
# ----------------------------------------------------------------------------
# See also: -
# ----------------------------------------------------------------------------
# Quantlet: SPMtruenadloc
# ----------------------------------------------------------------------------
# Description: SPMtruenadloc computes the true function, the Nadaraya-Watson
# estimate and the local linear estimate.
# ----------------------------------------------------------------------------
# Author: Eunah Whang, Awdesch Melzer 03042013
# ----------------------------------------------------------------------------

graphics.off()
rm(list = ls(all = TRUE))

set.seed(100)  # set pseudo random numbers
# Number of observations
N    = 900  
grid = seq(0, 1, length = 30)  # grid
x12  = expand.grid(grid, grid)  # expand the grid
x1   = x12[[1]]
x2   = x12[[2]]
true = function(x1, x2) {
    sin(2 * pi * x1) + x2
}  # true function

z = true(x1, x2)  # true function for inputs
y = true(x1, x2) + rnorm(N, mean = 0, sd = 0.5)  # add error to true function

# The local linear smoother
mhat.ll = loess(y ~ x1 + x2, span = 0.1, degree = 1)  # loc

# The Nadaraya-Watson smoother
mhat.nw = loess(y ~ x1 + x2, span = 0.1, degree = 0)


data.tr = data.frame(z, grid, grid)
data.ll = data.frame(mhat.ll$fitted, grid, grid)
data.nw = data.frame(mhat.nw$fitted, grid, grid)



require(lattice)
# To save plot uncomment following line and 'dev.off()', please, comment-out
# 'dev.new()' pdf('SPMtruenadloc%02d.pdf', height=5, width=15, onefile=F)
par.set = list(axis.line = list(col = "transparent"), clip = list(panel = "off"))

# true function plor
p1 = wireframe(z ~ grid * grid, data = data.tr, drape = TRUE, colorkey = F, ticktype = "detailed", 
    main = expression(paste("True Function")), screen = list(z = 30, x = -60), 
    scales = list(arrows = FALSE, col = "black", distance = 1, tick.number = 8, 
        cex = 0.7, x = list(labels = round(seq(0, 1, length = 11), 1)), y = list(labels = round(seq(0, 
            1, length = 11), 1)), z = list(labels = round(seq(min(z), max(z), length = 11), 
            2))), xlab = list("", rot = 30, cex = 1.2), ylab = list("", rot = -40, 
        cex = 1.2), zlab = list("", rot = 95, cex = 1.1), par.settings = par.set)

# local linear plot

p2 = wireframe(mhat.ll$fitted ~ grid * grid, data = data.ll, drape = TRUE, colorkey = F, 
    ticktype = "detailed", main = expression(paste("Local Linear")), screen = list(z = 30, 
        x = -60), scales = list(arrows = FALSE, col = "black", distance = 1, tick.number = 8, 
        cex = 0.7, x = list(labels = round(seq(0, 1, length = 11), 1)), y = list(labels = round(seq(0, 
            1, length = 11), 1)), z = list(labels = round(seq(min(mhat.ll$fitted), 
            max(mhat.ll$fitted), length = 11), 1))), xlab = list("", rot = 30, 
        cex = 1.2), ylab = list("", rot = -40, cex = 1.2), zlab = list("", rot = 95, 
        cex = 1.1), par.settings = par.set)

# nadaraya watson plot

p3 = wireframe(mhat.nw$fitted ~ grid * grid, data = data.nw, drape = TRUE, colorkey = F, 
    ticktype = "detailed", main = expression(paste("Nadaraya Watson")), screen = list(z = 30, 
        x = -60), scales = list(arrows = FALSE, col = "black", distance = 1, tick.number = 8, 
        cex = 0.7, x = list(labels = round(seq(0, 1, length = 11), 1)), y = list(labels = round(seq(0, 
            1, length = 11), 1)), z = list(labels = round(seq(min(mhat.nw$fitted), 
            max(mhat.nw$fitted), length = 11), 1))), xlab = list("", rot = 30, 
        cex = 1.2), ylab = list("", rot = -40, cex = 1.2), zlab = list("", rot = 95, 
        cex = 1.1), par.settings = par.set)

print(p1, split = c(1, 1, 3, 1), more = TRUE)
print(p2, split = c(2, 1, 3, 1), more = TRUE)
print(p3, split = c(3, 1, 3, 1))