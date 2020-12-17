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



# penalty terms -----------------------------------------------------------

M11$sp


M12_1.0 <- gam(accel ~ s(times, sp = 1.0), data = mcycle)
M12_10.0 <- gam(accel ~ s(times, sp = 10.0), data = mcycle)
M12_100.0 <- gam(accel ~ s(times, sp = 100.0), data = mcycle)
M12_1000.0 <- gam(accel ~ s(times, sp = 1000.0), data = mcycle)
M12_10000.0 <- gam(accel ~ s(times, sp = 10000.0), data = mcycle)
M12_0.1 <- gam(accel ~ s(times, sp = 0.1), data = mcycle)
M12_0.01 <- gam(accel ~ s(times, sp = 0.01), data = mcycle)
M12_0.001 <- gam(accel ~ s(times, sp = 0.001), data = mcycle)
M12_0.0001 <- gam(accel ~ s(times, sp = 0.0001), data = mcycle)

plot(M10, residuals = T)
plot(M12_1.0, residuals = T)
plot(M12_100.0, residuals = T)
plot(M12_10000.0, residuals = T)
plot(M12_0.1, residuals = T)
plot(M12_0.01, residuals = T)
plot(M12_0.001, residuals = T)

AIC(M10)
AIC(M12_1.0)
AIC(M12_100.0)
AIC(M12_0.1)
AIC(M12_0.01)
AIC(M12_0.001)
AIC(M12_0.0001)


# eye fix data ------------------------------------------------------------


eyefix_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/funct_theme_pts.csv")

eyefix_df_avg <- group_by(eyefix_df, Time, Object) %>%
  summarize(mean_fix = mean(meanFix), .groups = 'drop')

eyefix_df_avg_targ <- filter(eyefix_df_avg, Object == 'Target')


M13 <- gam(mean_fix ~ s(Time), data = eyefix_df_avg_targ)
plot(M13, residuals = T)

M14 <- gam(mean_fix ~ s(Time) + Object, data = eyefix_df_avg)

eyefix_df_avg %>% 
  add_predictions(M14) %>% 
  ggplot(aes(x = Time, y = pred, colour = Object)) + geom_line()

eyefix_df_avg <- mutate(eyefix_df_avg, Object = factor(Object))

M15 <- gam(mean_fix ~ s(Time, by = Object), data = eyefix_df_avg)

eyefix_df_avg %>% 
  add_predictions(M15) %>% 
  ggplot(aes(x = Time, y = pred, colour = Object)) + geom_line()

M16 <- gam(mean_fix ~ s(Time, Object, bs = 'fs'), 
           data = eyefix_df_avg)

eyefix_df_avg %>% 
  add_predictions(M16) %>% 
  ggplot(aes(x = Time, y = pred, colour = Object)) + geom_line()

vis.gam(M16, theta = -200)


# generalized gams --------------------------------------------------------

golf_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/golf_putts.csv")

# generalized LINEAR model
M17 <- glm(cbind(success, attempts - success) ~ distance,  
           family = binomial, 
           data = golf_df)

golf_df %>% 
  add_predictions(M17) %>% 
  ggplot(aes(x = distance, y = pred)) + geom_line() + geom_point()

golf_df %>% 
  add_predictions(M17, type = 'response') %>% 
  ggplot(aes(x = distance, y = pred)) + geom_line() + geom_point() +
  geom_point(aes(y = success/attempts), colour = 'red')


# GAM version of a generalized LINEAR model

M18 <- gam(cbind(success, attempts - success) ~ s(distance),  
           family = binomial, 
           data = golf_df)

golf_df %>% 
  add_predictions(M18, type = 'response') %>% 
  ggplot(aes(x = distance, y = pred)) + geom_line() + geom_point() +
  geom_point(aes(y = success/attempts), colour = 'red')


M19 <- gam(cbind(success, attempts - success) ~ s(distance, sp = 0.5),  
           family = binomial, 
           data = golf_df)

golf_df %>% 
  add_predictions(M19, type = 'response') %>% 
  ggplot(aes(x = distance, y = pred)) + geom_line() + geom_point() +
  geom_point(aes(y = success/attempts), colour = 'red')


M20 <- gam(cbind(success, attempts - success) ~ s(distance, sp = 10.0),  
           family = binomial, 
           data = golf_df)

golf_df %>% 
  add_predictions(M20, type = 'response') %>% 
  ggplot(aes(x = distance, y = pred)) + geom_line() + geom_point() +
  geom_point(aes(y = success/attempts), colour = 'red')




# Spatial models ----------------------------------------------------------

meuse_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/meuse.csv")


M21 <- gam(copper ~ s(x, y), data = meuse_df)

plot(M21)
plot(M21, se = F)
plot(M21, scheme = 1)
plot(M21, scheme = 2)

vis.gam(M21, view = c('x', 'y'), plot.type = 'persp', theta = 10)
vis.gam(M21, view = c('x', 'y'), plot.type = 'contour')
vis.gam(M21, view = c('x', 'y'), plot.type = 'contour', too.far = 0.1)
vis.gam(M21, view = c('x', 'y'), plot.type = 'contour', nlevels = 3, too.far = 0.1)


add_predictions(meuse_df, M21)

tibble(x = 179000, y = 333000) %>% 
  add_predictions(M21)


M22 <- gam(copper ~ te(x, y, elev), data = meuse_df)
plot(M22)

M23 <- gam(copper ~ s(x, y) + s(elev) + ti(x, y, elev), 
           data = meuse_df)
plot(M23)


# multilevel/mixed-effect gams --------------------------------------------

library(lme4)

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + stat_smooth(method = 'lm', se = F) +
  facet_wrap(~Subject)

M24 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

# nonlinear multilevel data set
data_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/raneftanh.csv")

ggplot(data_df, aes(x = x, y = y, colour = v)) + geom_point() + facet_wrap(~v)

add_basis_functions <- function(x, k = 5){
  bs(x, degree = k) %>%
    as_tibble() %>% 
    rename_all(~paste0('phi_', .)) %>%
    mutate_all(as.numeric)
}
bs_df <- data_df %>% pull(y) %>% add_basis_functions(k = 5)
data_df <- bind_cols(data_df, bs_df)

# look at the basis functions
data_df %>% 
  dplyr::select(y, starts_with('phi')) %>% 
  pivot_longer(cols = -y,
               names_to = 'phi',
               values_to = 'f') %>% 
  ggplot(aes(x = y, y = f, colour = phi)) + geom_line()

data_df

lm(y ~ bs(x))

data_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/raneftanh.csv")
bs_df <- data_df %>% pull(x) %>% add_basis_functions(k = 5)
data_df <- bind_cols(data_df, bs_df)

M25 <- lmer(y ~ phi_1 + phi_2 + phi_3 + phi_4 + phi_5 + (phi_1 + phi_2 + phi_3 + phi_4 + phi_5||v), 
            data = data_df)



# Bayesian gams -----------------------------------------------------------

set.seed(10101) # Omit or change this if you like

N <- 25

x_1 <- rnorm(N)
x_2 <- rnorm(N)

beta_0 <- 1.25
beta_1 <- 1.75
beta_2 <- 2.25

mu <- beta_0 + beta_1 * x_1 + beta_2 * x_2

y <- mu + rnorm(N, mean=0, sd=1.75)

Df <- tibble(x_1, x_2, y)

M26 <- lm(y ~ x_1 + x_2, data = Df)

library(brms)

M27 <- brm(y ~ x_1 + x_2, data = Df)


M28 <- gam(accel ~ s(times), data = mcycle)
M29 <- brm(accel ~ s(times), data = mcycle)
