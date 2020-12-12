# File:    R01_6_3_Regression.R
# Course:  R01: R: An introduction
# Chapter: 6: Modeling data
# Section: 3: Regression

# INSTALL AND LOAD PACKAGES ################################

library(datasets)  # Load base packages manually

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, caret, lars, tidyverse)

# LOAD DATA ################################################

# Define variable groups

x <- c(5.1, 5.5, 5.8, 6.1, 6.4, 6.7, 6.4, 6.1, 5.10, 5.7)
y <- c(63, 66, 69, 72, 75, 78, 75, 72, 69, 66)

# REGRESSION WITH SIMULTANEOUS ENTRY #######################

# Using variable groups
reg1 <- lm(y ~ x)

summary(reg1)  # Inferential tests
plot(reg1)

#Find weight of a person of 6.3 inch
a <- data.frame(x=6.3)
result <- predict(reg1,a)
print(result)

# MORE SUMMARIES ###########################################

anova(reg1)            # Coefficients w/inferential tests
coef(reg1)             # Coefficients (same as reg1)
confint(reg1)          # CI for coefficients
resid(reg1)            # Residuals case-by-case
hist(residuals(reg1))  # Histogram of residuals

# ADDITIONAL MODELS ########################################

# Conventional stepwise regression
stepwise <- lars(x, y, type = "stepwise")

# Stagewise: Like stepwise but with better generalizability
forward <- lars(x, y, type = "forward.stagewise")

# LAR: Least Angle Regression
lar <- lars(x, y, type = "lar")

# LASSO: Least Absolute Shrinkage and Selection Operator
lasso <- lars(x, y, type = "lasso")

# Comparison of R^2 for new models
r2comp <- c(stepwise$R2[6], forward$R2[6], 
            lar$R2[6], lasso$R2[6]) %>% 
            round(2)
names(r2comp) <- c("stepwise", "forward", "lar", "lasso") 
r2comp  # Show values of R^2

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
