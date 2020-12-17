
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
  select(y, starts_with('phi')) %>% 
  pivot_longer(cols = -y,
               names_to = 'phi',
               values_to = 'f') %>% 
  ggplot(aes(x = y, y = f, colour = phi)) + geom_line()