library(tidyverse)
library(splines)
library(modelr)
library(magrittr)

# Looking at splines ---------------------------------------------------

b_spline <- function(x, knots, show_piece = F){
  
  stopifnot(length(knots) == 5)
  
  .b_spline <- function(x){
    if (x >= knots[1] & x < knots[2]) {
      piece <- 1
      u <- (x-knots[1])/(knots[2] - knots[1])
      y <- 1/6 * u^3 
      
    } else if (x >= knots[2] & x < knots[3]) {
      piece <- 2
      u <- (x-knots[2])/(knots[3] - knots[2])
      y <- 1/6 * (1 + 3*u + 3*u^2 - 3*u^3)
      
    } else if (x >= knots[3] & x < knots[4]) {
      piece <- 3
      u <- (x-knots[3])/(knots[4] - knots[3])
      y <- 1/6 * (4 - 6*u^2 + 3*u^3)
      
    } else if (x >= knots[4] & x <= knots[5]) {
      piece <- 4
      u <- (x-knots[4])/(knots[5] - knots[4])
      y <- 1/6 * (1 - 3*u + 3*u^2 - u^3)
    }
    else {
      piece <- 0
      y <- 0 
    } 
    
    if (!show_piece) return(y)
    
    c(y, piece)
    
  }
  
  if (!show_piece){
    tibble(x = x, 
           y = map_dbl(x, .b_spline)
    )
  } else {
    map(x, .b_spline) %>% 
      do.call(rbind, .)%>%
      set_colnames(c('y', 'segment')) %>% 
      as_tibble() %>% 
      mutate(x = x) %>% 
      mutate_at(vars(segment), as.factor) %>% 
      select(x, everything())
  }
  
}

x <- seq(-1, 1, length.out = 1000)
knots <- seq(-0.5, 0.5, length.out = 5)
b_spline(x, knots = knots, show_piece = T) %>%
  ggplot(mapping = aes(x = x, 
                       y = y, 
                       colour = segment)) +
  geom_point(size = 0.5) +
  theme(legend.position="none")

# a range of splines
x <- seq(-2, 2, length.out = 1000)
knots <- seq(-2, 2, by = 0.05)

imap_dfr(seq(1,length(knots)-4),
         ~b_spline(x, knots = knots[.x:(.x+4)]) %>% 
           mutate(k = .y)
) %>% ggplot(mapping = aes(x = x, y = y, group = k)) + geom_line()

# Using splines::bs
bs(x, knots  = knots) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(x=x) %>% 
  gather(k, y, -x) %>% ggplot(mapping = aes(x = x, y = y, group = k)) + geom_line()

# a random spline functions
rbspline <- function(x, knots){
  tibble(x = x,
         y = bs(x, knots = knots) %*% rnorm(length(knots) + 3) %>% 
           as.vector()
  )
  
}

rbspline_examples <- function(i){
  set.seed(i)
  Df <- imap(rerun(5, rbspline(x, knots)), 
             ~mutate(., example = .y)) %>% 
    bind_rows() %>% 
    mutate_at(vars(example), as.character)
  
  p <- Df %>% ggplot(mapping = aes(x = x, y = y, colour = example)) + 
    geom_line() +
    theme(legend.position="none")
  
  p
}

# rbf ---------------------------------------------------------------------
rbf <- function(x, centres, sigma = 1.0){
  map_dfc(centres, 
          ~exp(-(x-.)^2/(2*sigma^2))
  ) %>% as.matrix()
}

random_rbf <- function(x, centres, sigma = 1.0){
  rbf(x, centres, sigma) %>% 
    multiply_by_matrix(rnorm(length(centres))) %>% 
    as.data.frame() %>% 
    set_names('y') %>% 
    mutate(x = x)
}


random_rbf_examples <- function(i, sigma){
  set.seed(i)
  Df <- imap(rerun(5, random_rbf(x, centres = centres, sigma = sigma)), 
             ~mutate(.x, example = .y)) %>% 
    bind_rows() %>% 
    mutate_at(vars(example), as.character)
  
  p <- Df %>% ggplot(mapping = aes(x = x, y = y, colour = example)) + 
    geom_line() +
    theme(legend.position="none")
  
  p
}

# RBF regression ----------------------------------------------------------
rbf <- function(x, centres, sigma = 1.0){
  map(centres,
      ~exp(-(x-.)^2/(2*sigma^2))
  ) %>% do.call(cbind, .)
}



