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
