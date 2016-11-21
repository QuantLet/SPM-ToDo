# ------------------------------------------------------------------------------
# Book:        SPM - Nonparametric and Semiparametric Modelling
# ------------------------------------------------------------------------------
# Quantlet:    SPMcps85lin
# ------------------------------------------------------------------------------
# Description: SPMcps85lin computes a linear regression of log wages on 
#              schooling, experience and experience squared for observations 
#              from the 1985 CPS (see Berndt, 1991). The marginal wage-schooling
#              and wage-experience profiles and the 3d wage-schooling/experience
#              surface are displayed.              
# ------------------------------------------------------------------------------
# Usage:       -
# ------------------------------------------------------------------------------
# Inputs:      None
# ------------------------------------------------------------------------------
# Output:      Plots for the marginal wage-schooling and the wage-experience 
#              profiles are displayed. The 3d wage-schooling/experience
#              surface is plotted. 
# ------------------------------------------------------------------------------
# Keywords:    linear, regression, 3D, parametric, nonparametric
# ------------------------------------------------------------------------------
# See also:     
# ------------------------------------------------------------------------------
# Example:     -
# ------------------------------------------------------------------------------
# Author:      Marlene Mueller, 2008/11/19
# ------------------------------------------------------------------------------


# Close windows and clear variables
graphics.off()
rm(list = ls(all = TRUE))

setwd("C:/...")  #Set working directory

# read the data, then choose variables
x  = read.table("cps85.dat")
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages("AER")
library(AER)

# define the variables
lwage       = x$V11
education   = x$V1
experience  = x$V8

# Fit a linear model of wages to education, experience and
# experience^2.
l  = lm(lwage ~ education + experience + I(experience^2))
summary(l)

# Extract coefficients of the linear model
b  = coef(l)

# Determine the wage-schooling profile
m.school  = data.frame(education, education * b["education"])
m.school  = m.school[order(education), ]  # wage-schooling profile

# Determine the wage-experience profile
m.experi  = data.frame(experience, experience * b["experience"] + experience^2 * 
                       b["I(experience^2)"])
m.experi  = m.experi[order(experience), ]  # wage-experience profile

# Plot the wage-schooling and the wage-experience profiles
par(mfrow = c(1, 2))
plot(m.school, col = "blue", pch = 19, main = "Wage <-- Schooling", 
    xlab = "education", ylab = "")
lines(m.school, col = "blue")
plot(m.experi, col = "magenta", pch = 19, main = "Wage <-- Experience", 
    xlab = "experience", ylab = "")
lines(m.experi, col = "magenta")
par(mfrow = c(1, 1))

# Set the color schemes for the wage-schooling/experience surfaces
col  = rep("green", nrow(x))
col[education == 12]  = "red"
pch  = rep(1, nrow(x))
pch[education == 12]  = 8

# Plot the 3d surface
dev.new()
scatterplot3d(education, experience, fitted(l), xlab = "education", 
    ylab = "experience", zlab = "", main = "Wage <-- Schooling, Experience", 
    color = col, pch = pch, angle = 130)


# Now we use a more complicated linear model.
l1 = lm(lwage ~ education + I(education^2) + experience + I(experience^2) + 
    I(experience^3))
summary(l1)

col = rep("green", nrow(x))
col[education == 12] = "red" 

dev.new()
scatterplot3d(education,experience, fitted(l1),
              xlab = "education",ylab = "experience",zlab = "",
              main = "Wage <-- Schooling, Experience",color = col,pch = pch,angle = 130)