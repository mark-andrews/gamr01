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
