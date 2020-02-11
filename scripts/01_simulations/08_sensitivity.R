# Load packages
library(here)
library(cowplot)
library(furrr)
library(tidyverse)
library(scales)

# Source functions
source(here("scripts", "00_helpers.R"))
source(here("scripts", "01_simulations", "01_model.R"))
source(here("scripts", "01_simulations", "02_wrapper.R"))
source(here("scripts", "01_simulations", "03_default_parameters.R"))

optimize <- function(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, years, b = 0){
  
  pars <- list(r = r,
               K = K,
               X0 = X0,
               D = D,
               p = p,
               q = q,
               c = c,
               beta = beta,
               L = L,                               # The ith value of L gets passed to the list
               alpha = alpha,
               mu = mu,
               w = w,
               years = years,
               b = b)
  
  # Call optim
  opt_results <- optim(par = 0,                      # Guess that we know works (OA equilibrium)
                       fn = get_chi,                 # Function to call
                       pars = pars,                  # All other parametrs passed as a list, and extracted in wraper
                       control = list(fnscale = -1), # Indicate that we are maximizing
                       method = "L-BFGS-B",          # Use a method that allows for min and max
                       lower = 0,                    # Minimum charge is 0
                       upper = 1e6)                  # Maximum fee is 1 million
  
  opt_par <- opt_results$par                      # Save best chi
  opt_val <- opt_results$value                    # Save corresponding biomass value
  
  results <- tibble(chi = opt_par, X = opt_val)
  
  return(results)
}

# Different dispersal scenarios
make_D <- function(self_rec){
  
  exports <- 1 - self_rec
  
  D <- matrix(c(self_rec, exports,
                exports, self_rec),
              nrow = 2,
              byrow = T)
  
  return(D)
}


# best_results <- read.csv(here("results", "best_combination_of_L_and_Chi.csv"),
#                          stringsAsFactors = F) %>% 
#   select(index, L_try = L, chi_try = chi)


plan(multiprocess)

a <- expand_grid(L_try = seq(0, 1, by = 0.2),
                 w_try = c(0.1 * w, 0.5 * w, w, 10 * w),
                 alpha_try = c(0.5 * alpha, alpha, alpha * 2),
                 c_try = c(2500, 3000, 3500),
                 self_rec = c(0.3, 0.5, 0.7)) %>%
  mutate(D_try = map(self_rec, make_D)) %>% 
  # left_join(best_results, by = c("index")) %>%
  # select(-c(index)) %>%
  mutate(results = future_pmap(.l = list(L = L_try,
                                         w = w_try,
                                         alpha = alpha_try,
                                         c = c_try,
                                         D = D_try),
                               .f = optimize,
                               r = r,
                               K = K,
                               X0 = X0,
                               p = p,
                               q = q,
                               beta = beta,
                               mu = mu,
                               years = years)) %>% 
  unnest(cols = results)

plan(sequential)


p1 <- a %>% group_by(w_try, alpha_try, c_try, self_rec) %>% 
  mutate(max_X = max(X, na.rm = T)) %>% 
  ungroup() %>% 
  filter(X == max_X) %>% 
  ggplot(aes(x = L_try)) +
  geom_density(fill = "steelblue") +
  plot_theme() +
  scale_x_continuous(limits = c(0, 1))

lazy_ggsave(p1, filename = "fake_montecarlo", width = 9, height = 6)





