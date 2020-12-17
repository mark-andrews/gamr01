library(tidyverse)
library(modelr)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/weight.csv")

M <- lm(weight ~ height, data = weight_df)
M1 <- lm(weight ~ height + gender, data = weight_df)

weight_df <- mutate(weight_df, heavy = weight > 90)
weight_df$heavy <- weight_df$weight > 90

# polynomial regression ---------------------------------------------------

eyefix_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/funct_theme_pts.csv")

eyefix_df_avg <- group_by(eyefix_df, Time, Object) %>%
  summarize(mean_fix = mean(meanFix), .groups = 'drop')
  
eyefix_df_avg_targ <- filter(eyefix_df_avg, Object == 'Target')

ggplot(eyefix_df_avg,
       aes(x = Time, y = mean_fix, colour = Object, group = Object)) + geom_point()

ggplot(eyefix_df_avg_targ,
       aes(x = Time, y = mean_fix)) + geom_point()


# Degree 9 polynomial -----------------------------------------------------

M2 <- lm(mean_fix ~ poly(Time, 9), data = eyefix_df_avg_targ)

# lm(mean_fix ~ Time + I(Time^2) + Time^3 + Time^4 + Time^5  + ... Time^9, ...)

eyefix_df_avg_targ %>% 
  add_predictions(M2) %>% 
  ggplot(aes(x = Time, y = pred)) + geom_line() +
  geom_point(aes(y = mean_fix))


M3 <- lm(mean_fix ~ poly(Time, 5), data = eyefix_df_avg_targ)

eyefix_df_avg_targ %>% 
  add_predictions(M3) %>% 
  ggplot(aes(x = Time, y = pred)) + geom_line() +
  geom_point(aes(y = mean_fix))

summary(M3)$r.sq # R^2 for degree 5 polynomial
summary(M2)$r.sq # R^2 for degree 9 polynomial

summary(M3)$adj.r.sq
summary(M2)$adj.r.sq

# null hypothesis model comparison
anova(M3, M2)

# AIC for models M2 and M3
AIC(M2)
AIC(M3)


# Generate random data ----------------------------------------------------

set.seed(101)
Df <- tibble(x = seq(-2, 2, length.out = 20),
             y = 0.5 + 1.5 * x + rnorm(length(x))
) 
ggplot(Df, aes(x,y)) + geom_point()

M_fits <- map(seq(9), 
              ~lm(y ~ poly(x, degree = .), data = Df))

map_dbl(M_fits, AIC)

M_aics <- map_dbl(M_fits, AIC)
M_aics - min(M_aics)

Df_new <- tibble(x = seq(-2, 2, length.out = 100))

Df %>% add_predictions(M_fits[[1]]) %>%
  ggplot(aes(x, y = pred)) + 
  geom_line() + 
  geom_point(aes(y = y))

Df %>% add_predictions(M_fits[[2]]) %>%
  ggplot(aes(x, y = pred)) + 
  geom_line() + 
  geom_point(aes(y = y))

Df %>% add_predictions(M_fits[[3]]) %>%
  ggplot(aes(x, y = pred)) + 
  geom_line() + 
  geom_point(aes(y = y))

Df %>% add_predictions(M_fits[[9]]) %>%
  ggplot(aes(x, y = pred)) + 
  geom_line() + 
  geom_point(aes(y = y))

aic_c <- function(model){
  K <- length(coef(model)) + 1
  N <- nrow(model$model)
  AIC(model) + (2*K*(K+1))/(N-K-1)
}

M_aiccs <- map_dbl(M_fits, aic_c)
M_aiccs - min(M_aiccs)

map_dbl(M_fits, ~summary(.)$r.sq)

# model with categorical predictor
M4 <- lm(mean_fix ~ poly(Time, degree = 9) * Object, data = eyefix_df_avg)

eyefix_df_avg %>% 
  add_predictions(M4) %>% 
  ggplot(aes(x = Time, y = pred, colour = Object, group = Object)) + geom_line() +
  geom_point(aes(y = mean_fix))



# B splines ---------------------------------------------------------------
knots <- seq(-500, 2500, by = 500)
M5 <- lm(mean_fix ~ bs(Time, knots = knots), 
         data = eyefix_df_avg_targ)

eyefix_df_avg_targ %>% 
  add_predictions(M5) %>% 
  ggplot(aes(x = Time, y = pred)) + geom_line(colour = 'red') +
  geom_point(aes(y = mean_fix))

M6 <- lm(mean_fix ~ bs(Time, knots = knots) * Object, 
         data = eyefix_df_avg)

eyefix_df_avg %>% 
  add_predictions(M6) %>% 
  ggplot(aes(x = Time, y = pred, colour = Object, group = Object)) + 
  geom_line() +
  geom_point(aes(y = mean_fix))


M7 <- lm(mean_fix ~ bs(Time, df = 10) * Object, 
         data = eyefix_df_avg)

eyefix_df_avg %>% 
  add_predictions(M7) %>% 
  ggplot(aes(x = Time, y = pred, colour = Object, group = Object)) + 
  geom_line() +
  geom_point(aes(y = mean_fix))

with(eyefix_df_avg, attr(bs(Time, df = 10), 'knots'))



# Model evaluation in spline models ---------------------------------------

gssvocab_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/gssvocab.csv")

ggplot(gssvocab_df, aes(x = age, y = vocab)) + geom_point()

M8 <- lm(vocab ~ ns(age, df = 1), data = gssvocab_df)

df_seq <- seq(30)

M_gss <- map(df_seq, ~lm(vocab ~ ns(age, df = .), 
                         data = gssvocab_df))

M_gss_aic <- map_dbl(M_gss, aic_c)

gssvocab_df %>% 
  add_predictions(M_gss[[5]]) %>% 
  ggplot(aes(x = age, y = pred)) +
  geom_line(colour = 'red') +
  geom_point(aes(y = vocab))


gssvocab_df %>% 
  add_predictions(M_gss[[1]]) %>% 
  ggplot(aes(x = age, y = pred)) +
  geom_line(colour = 'red') +
  geom_point(aes(y = vocab))


gssvocab_df %>% 
  add_predictions(M_gss[[15]]) %>% 
  ggplot(aes(x = age, y = pred)) +
  geom_line(colour = 'red') +
  geom_point(aes(y = vocab))
