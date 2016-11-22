# ----------------------------------------------------------------------------
# Book: SPM
# ----------------------------------------------------------------------------
# See also: -
# ----------------------------------------------------------------------------
# Quantlet: SPMslices3D
# ----------------------------------------------------------------------------
# Description: SPMslices3D computes a kernel density estimate of variables
# Duration of credit, Income and Age of credit data. The density is visualized
# by fixing one variable in each direction.  The bandwidth parameters are
# chosen by Scott's rule of thumb.
# ----------------------------------------------------------------------------
# Author: Yafei Xu, Awdesch Melzer 20130404
# ----------------------------------------------------------------------------

rm(list = ls(all = TRUE))
graphics.off()

require(lattice)
x = read.table("kredit.dat")
x = x[, c(3, 6, 14)]


########### biweight kernel density estimation

ng   = 30  # number of grid
nobs = nrow(x)  # number of obs
h    = 2.5964 * apply(x, 2, sd) * nobs^(-1/7)  # rule-of-thumb bandwidth's

# Duration fix at 38
fix = 38

a   = apply(x, 2, min)
b   = apply(x, 2, max)

for (i in 1:3) {
    assign(paste("grid", i, sep = ""), seq(a[i], b[i], length = ng))
}

X1 = x[, 1]
X2 = x[, 2]
X3 = x[, 3]

fhat = function(x, y) {
    v = matrix(0, ng^2, 1)
    for (i in 1:ng^2) {
        c1 = matrix(fix, nobs, 1)
        c2 = matrix(x[i], nobs, 1)
        c3 = matrix(y[i], nobs, 1)
        v[i] = (1/(nobs * h[1] * h[2] * h[3])) * sum(((15/16) * (1 - ((c1 - X1)/h[1])^2)^2) * 
            as.numeric(abs((c1 - X1)/h[1]) <= 1) * ((15/16) * (1 - ((c2 - X2)/h[2])^2)^2) * 
            as.numeric(abs((c2 - X2)/h[2]) <= 1) * ((15/16) * (1 - ((c3 - X3)/h[3])^2)^2) * 
            as.numeric(abs((c3 - X3)/h[3]) <= 1))
    }
    return(v)
}
### 
outer931 = outer(grid2, grid3, fhat)

par.set = list(axis.line = list(col = "transparent"), clip = list(panel = "off"))
# true function plor
p1 = wireframe(outer931, drape = TRUE, colorkey = F, ticktype = "detailed", main = paste("Duration fixed at", 
    fix), screen = list(z = 30, x = -60), scales = list(arrows = FALSE, col = "black", 
    distance = 1, tick.number = 8, cex = 0.7, x = list(labels = round(seq(a[2], 
        b[2], length = 11), 1)), y = list(labels = round(seq(a[3], b[3], length = 11), 
        1)), z = list(labels = rep(" ", 11))), xlab = list("income", rot = 25, 
    cex = 0.7), ylab = list("age", rot = -50, cex = 0.7), zlab = list("density", 
    rot = 95, cex = 0.7), par.settings = par.set)

# Income fixed at 9337
fix = 9337
fhat = function(x, y) {
    v = matrix(0, ng^2, 1)
    for (i in 1:ng^2) {
        c1 = matrix(x[i], nobs, 1)
        c2 = matrix(fix, nobs, 1)
        c3 = matrix(y[i], nobs, 1)
        v[i] = (1/(nobs * h[1] * h[2] * h[3])) * sum(((15/16) * (1 - ((c1 - X1)/h[1])^2)^2) * 
            as.numeric(abs((c1 - X1)/h[1]) <= 1) * ((15/16) * (1 - ((c2 - X2)/h[2])^2)^2) * 
            as.numeric(abs((c2 - X2)/h[2]) <= 1) * ((15/16) * (1 - ((c3 - X3)/h[3])^2)^2) * 
            as.numeric(abs((c3 - X3)/h[3]) <= 1))
    }
    return(v)
}

outer932 = outer(grid1, grid3, fhat)

# plot 2
p2 = wireframe(outer932, drape = TRUE, colorkey = F, ticktype = "detailed", main = paste("Income fixed at", 
    fix), screen = list(z = 30, x = -60), scales = list(arrows = FALSE, col = "black", 
    distance = 1, tick.number = 8, cex = 0.7, x = list(labels = round(seq(a[1], 
        b[1], length = 11), 1)), y = list(labels = round(seq(a[3], b[3], length = 11), 
        1)), z = list(labels = rep(" ", 11))), xlab = list("duration", rot = 25, 
    cex = 0.7), ylab = list("age", rot = -50, cex = 0.7), zlab = list("density", 
    rot = 95, cex = 0.7), par.settings = par.set)

# age fix at 47
fix  = 47
fhat = function(x, y) {
    v = matrix(0, ng^2, 1)
    for (i in 1:ng^2) {
        c1 = matrix(x[i], nobs, 1)
        c2 = matrix(y[i], nobs, 1)
        c3 = matrix(fix, nobs, 1)
        v[i] = (1/(nobs * h[1] * h[2] * h[3])) * sum(((15/16) * (1 - ((c1 - X1)/h[1])^2)^2) * 
            as.numeric(abs((c1 - X1)/h[1]) <= 1) * ((15/16) * (1 - ((c2 - X2)/h[2])^2)^2) * 
            as.numeric(abs((c2 - X2)/h[2]) <= 1) * ((15/16) * (1 - ((c3 - X3)/h[3])^2)^2) * 
            as.numeric(abs((c3 - X3)/h[3]) <= 1))
    }
    return(v)
}

outer933 = outer(grid1, grid2, fhat)

# plot 3
p3 = wireframe(outer933, drape = TRUE, colorkey = F, ticktype = "detailed", main = paste("Age fixed at", 
    fix), screen = list(z = 30, x = -60), scales = list(arrows = FALSE, col = "black", 
    distance = 1, tick.number = 8, cex = 0.7, x = list(labels = round(seq(a[1], 
        b[1], length = 11), 1)), y = list(labels = round(seq(a[2], b[2], length = 11), 
        1)), z = list(labels = rep(" ", 11))), xlab = list("duration", rot = 25, 
    cex = 0.7), ylab = list("income", rot = -50, cex = 0.7), zlab = list("density", 
    rot = 95, cex = 0.7), par.settings = par.set)

# pdf('SPMsliced3D%02d.pdf', height=5, width=15, onefile=F)
print(p1, split = c(1, 1, 3, 1), more = TRUE)
print(p2, split = c(2, 1, 3, 1), more = TRUE)
print(p3, split = c(3, 1, 3, 1))
# dev.off()