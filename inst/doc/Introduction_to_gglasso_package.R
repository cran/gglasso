## ----eval=FALSE---------------------------------------------------------------
#  # on CRAN
#  install.packages("gglasso")
#  
#  # dev version on GitHub
#  pacman::p_load_gh('emeryyi/gglasso')

## -----------------------------------------------------------------------------
library(gglasso)

# load bardet data set
data(bardet)

group1 <- rep(1:20, each = 5)

fit_ls <- gglasso(x = bardet$x, y = bardet$y, group = group1, loss = "ls")

plot(fit_ls)

coef(fit_ls)[1:5,90:100]

## -----------------------------------------------------------------------------
cvfit_ls <- cv.gglasso(x = bardet$x, y = bardet$y, group = group1, loss = "ls")

plot(cvfit_ls)
coef(cvfit_ls, s = "lambda.min")

## -----------------------------------------------------------------------------
# generate weight matrix
times <- seq_along(bardet$y)
rho <- 0.5
sigma <- 1
H <- abs(outer(times, times, "-"))
V <- sigma * rho^H
p <- nrow(V)
V[cbind(1:p, 1:p)] <- V[cbind(1:p, 1:p)] * sigma

# reduce eps to speed up convergence for vignette build
fit_wls <- gglasso(x = bardet$x, y = bardet$y, group = group1, loss = "wls", 
                   weight = V, eps = 1e-4)

plot(fit_wls)

coef(fit_wls)[1:5,90:100]

