library(tidyverse)
library(modelr)
library(mgcv)

mcycle <- MASS::mcycle

ggplot(mcycle, aes(x = times, y = accel)) + geom_point()

M9 <- gam(accel ~ times, data = mcycle)
summary(lm(accel ~ times, data = mcycle))$coefficients

M10 <- gam(accel ~ s(times), data = mcycle)
plot(M10)
plot(M10, residuals = T)

summary(M10)

M11 <- gam(accel ~ s(times, k = 5), data = mcycle)
plot(M11, residuals = TRUE)
plot(M10, residuals = TRUE)


k_seq <- seq(3, 20)
M_k_seq <- map(k_seq, ~gam(accel ~ s(times, k = .), data = mcycle))

# lapply <-> purrr::map
# sapply/vapply <-> purrr:map_dbl

M_k_aic <- map_dbl(M_k_seq, AIC)


gam.check(M10)
gam.check(M11)

k.check(M10)
k.check(M11)
