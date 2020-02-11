
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

# Plot legends
l_legend <- "Proportion as lease area (L)"
b_legend <- "External budget (b)"

# Define parameters
Ls <- seq(0, 1, by = 0.05)                           # Define a vector of L values

bs_heatmap <- 10 ^ seq(1, 10, by = 0.5)

bs_ridgeline <- c(10,
                  100, 
                  1000,
                  0.5 * alpha,
                  alpha / 10,
                  alpha,
                  2 * alpha,
                  5 * alpha,
                  10 * alpha,
                  20 * alpha,
                  50 * alpha,
                  60 * alpha,
                  80 * alpha,
                  90 * alpha,
                  100 * alpha,
                  110 * alpha,
                  111 * alpha,
                  120 * alpha,
                  200 * alpha,
                  500 * alpha,
                  1000 * alpha,
                  1e4 * alpha,
                  1e5 * alpha)

# Start simulations
ridgeline_data <- expand_grid(b_try = bs_ridgeline,
                              L_try = Ls) %>% 
  mutate(results = pmap(.l = list(b = b_try,
                                  L = L_try),
                        .f = wrapper,
                        chi = chi,
                        K = K,
                        r = r,
                        X0 = X0,
                        D = D,
                        p = p,
                        q = q,
                        c = c,
                        beta = beta,
                        alpha = alpha, 
                        mu = mu,
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(results) %>% 
  group_by(b_try) %>% 
  mutate(X_max = max(X_vec, na.rm = T)) %>% 
  ungroup() %>% 
  filter(X_vec == X_max)

L_and_b_ridgeline <- ggplot(data = ridgeline_data, aes(x = L_try,y = b_try)) +
  geom_point(size = 2) +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend) +
  scale_y_continuous(trans = "log10")

heatmap_data <- expand_grid(b_try = bs_heatmap,
            L_try = Ls) %>% 
  mutate(results = pmap(.l = list(b = b_try,
                                  L = L_try),
                        .f = wrapper,
                        chi = chi,
                        K = K,
                        r = r,
                        X0 = X0,
                        D = D,
                        p = p,
                        q = q,
                        c = c,
                        beta = beta,
                        alpha = alpha, 
                        mu = mu,
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(results) %>% 
  mutate(budget = E_e_vec * alpha,
         budget_from_fees = budget - b_try,
         budget_prop = budget_from_fees / budget,
         prop_illegal_harvests = H_i_vec / (H_i_vec + H_l_vec)) %>% 
  replace_na(replace = list(prop_illegal_harvests = 0))

# Heatmaps

biomass_heatmap <- 
  ggplot(data = heatmap_data, aes(x = L_try, y = b_try)) + 
  geom_tile(aes(fill = X_vec / K)) +
  geom_point(data = ridgeline_data) +
  scale_fill_viridis_c() +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend) +
  scale_y_continuous(trans = "log10", expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

budget_heatmap <- 
  ggplot(data = heatmap_data,aes(x = L_try, y = b_try, fill = budget_prop)) + 
  geom_tile() +
  scale_fill_viridis_c(name = "Percent budget\nfrom access fees",
                       labels = scales::percent) +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend) +
  scale_y_continuous(trans = "log10", expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

effort_heatmap <- 
  ggplot(data = heatmap_data, aes(x = L_try, y = b_try, fill = prop_illegal_harvests)) + 
  geom_tile() +
  scale_fill_viridis_c(name = "Percent harvests\nillegal",
                       labels = scales::percent)  +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend) +
  scale_y_continuous(trans = "log10", expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

plot_grid(biomass_heatmap,
          budget_heatmap,
          effort_heatmap, ncol = 1) 

################################ OPTIMIZED VERSIONS ################################

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
  
  results <- wrapper(chi = opt_results$par,              # Call the simulation to calculate B_r / B_f
                     r = r,
                     K = K,
                     X0 = X0,
                     D = D,
                     p = p,
                     q = q,
                     c = c,
                     beta = beta,
                     L = L,
                     alpha = alpha,
                     mu = mu,
                     w = w,
                     years = years,
                     want = "All",
                     b = b)
  
  return(results)
}

# Run ridgeline
ridgeline_data_optimizing <- expand_grid(b_try = bs_heatmap,
                                         L_try = Ls) %>% 
  mutate(results = future_pmap(.l = list(b = b_try,
                                         L = L_try),
                               .f = optimize,
                               K = K,
                               r = r,
                               X0 = X0,
                               D = D,
                               p = p,
                               q = q,
                               c = c,
                               beta = beta,
                               alpha = alpha, 
                               mu = mu,
                               w = w,
                               years = years)) %>% 
  unnest(results) %>% 
  group_by(b_try) %>% 
  mutate(X_max = max(X_vec, na.rm = T)) %>% 
  ungroup() %>% 
  filter(X_vec == X_max)

ggplot(data = ridgeline_data_optimizing, aes(x = L_try,y = b_try)) +
  geom_point(size = 3) +
  plot_theme() +
  labs(x = "Proportion as lease area (L)",
       y = "External budget (b)") +
  scale_y_continuous(trans = "log10")

plan(multiprocess)

# Run heatmap data
heatmap_data_optimizing <- expand_grid(b_try = bs_heatmap,
                                       L_try = Ls) %>% 
  mutate(results = future_pmap(.l = list(b = b_try,
                                  L = L_try),
                        .f = optimize,
                        K = K,
                        r = r,
                        X0 = X0,
                        D = D,
                        p = p,
                        q = q,
                        c = c,
                        beta = beta,
                        alpha = alpha, 
                        mu = mu,
                        w = w,
                        years = years)) %>% 
  unnest(results) %>% 
  mutate(budget = E_e_vec * alpha,
         budget_from_fees = budget - b_try,
         budget_prop = budget_from_fees / budget,
         prop_illegal_harvests = H_i_vec / (H_i_vec + H_l_vec))


biomass_heatmap_optimizing <- 
  ggplot(heatmap_data_optimizing, aes(x = L_try, y = b_try)) + 
  geom_tile(aes(fill = X_vec / K)) +
  geom_point(data = ridgeline_data_optimizing) +
  scale_fill_viridis_c() +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend) +
  scale_y_continuous(trans = "log10", expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

budget_heatmap_optimizing  <- 
  ggplot(data = heatmap_data_optimizing, aes(x = L_try, y = b_try, fill = budget_prop)) + 
  geom_tile() +
  scale_fill_viridis_c(name = "Percent budget\nfrom access fees",
                       labels = scales::percent) +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend) +
  scale_y_continuous(trans = "log10", expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

effort_heatmap_optimizing  <- 
  ggplot(data = heatmap_data_optimizing, aes(x = L_try, y = b_try, fill = prop_illegal_harvests)) + 
  geom_tile() +
  scale_fill_viridis_c(name = "Percent harvests\nillegal",
                       labels = scales::percent) +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend) +
  scale_y_continuous(trans = "log10", expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

plot_grid(biomass_heatmap_optimizing,
          budget_heatmap_optimizing,
          effort_heatmap_optimizing,
          ncol = 1)
