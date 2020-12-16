library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/gamr01/master/data/weight.csv")

M <- lm(weight ~ height, data = weight_df)
M1 <- lm(weight ~ height + gender, data = weight_df)

weight_df <- mutate(weight_df, heavy = weight > 90)
weight_df$heavy <- weight_df$weight > 90
