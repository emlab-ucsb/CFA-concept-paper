# Load packages
library(here)
library(cowplot)
library(tidyverse)
library(scales)

# Source functions
source(here("scripts", "00_helpers.R"))
source(here("scripts", "01_simulations", "01_model.R"))
source(here("scripts", "01_simulations", "02_wrapper.R"))
source(here("scripts", "01_simulations", "03_default_parameters.R"))

#### Running simulations #######################################################
#
# To run simulations for many combinations of parameters, we use the 
# model wrapper function (wrapper.R). It takes as input all of the parameters
# passed to the model, as well as the desired variable (X_vec is the default).
#
################################################################################

# Define some default legends
l_legend <- "Proportion as lease area (L)"
b_legend <- "Equilibrium biomass (X / K)"
b_legend_short <- "(X / K)"

# Find best Chi for a given L (and default parameters)
Ls <- seq(0, 1, by = 0.05)                           # Define a vector of L values
opt_par <- opt_val <- rel_b <- rel_e_i <- rel_e_l <- rel_e_e <- numeric(length(Ls))      # Define state variables

# Begin for loop to iterate across all L values
for(i in 1:length(Ls)){
  
  # Create a list to pass all the parameters
  pars <- list(r = r,
               K = K,
               X0 = X0,
               D = D,
               p = p,
               q = q,
               c = c,
               beta = beta,
               L = Ls[i],                               # The ith value of L gets passed to the list
               alpha = alpha,
               mu = mu,
               w = w,
               years = years,
               b = b)
  
  # Call optim
  opt_results <- optim(par = 1,                      # Guess that we know works (OA equilibrium)
                       fn = get_chi,                 # Function to call
                       pars = pars,                  # All other parametrs passed as a list, and extracted in wraper
                       control = list(fnscale = -1), # Indicate that we are maximizing
                       method = "L-BFGS-B",          # Use a method that allows for min and max
                       lower = 0,                    # Minimum charge is 0
                       upper = 1e6)                  # Maximum fee is 1 million
  
  opt_par[i] <- opt_results$par                      # Save best chi
  opt_val[i] <- opt_results$value                    # Save corresponding biomass value
  
  run <- wrapper(chi = opt_results$par,         # Call the simulation to calculate B_r / B_f
                 r = r,
                 K = K,
                 X0 = X0,
                 D = D,
                 p = p,
                 q = q,
                 c = c,
                 beta = beta,
                 L = Ls[i],
                 alpha = alpha,
                 mu = mu,
                 w = w,
                 years = years,
                 want = "All",
                 b = b) %>% 
    mutate(X_rel = X_r_vec / X_f_vec)
  
  rel_b[i] <- run %>% 
    pull(X_rel)
  
  rel_e_i[i] <- run %>%
    pull(E_i_vec)
  
  rel_e_l[i] <- run %>%
    pull(E_l_vec)
  
  rel_e_e[i] <- run %>%
    pull(E_e_vec)
  
}


# Put the results together into a tibble
best_results <- tibble(L = Ls, chi = opt_par, X = opt_val, X_rel = rel_b, E_i = rel_e_i, E_l = rel_e_l, E_e = rel_e_e) %>% 
  mutate(index = as.numeric(row.names(.)))                                      # I will use this column to join later

write.csv(x = best_results,
          file = here("results", "best_combination_of_L_and_Chi.csv"),
          row.names = F)

# Create a plot of L vs B
optimal_fee_for_L_plot <- 
  ggplot(data = best_results,
         mapping = aes(x = L, y = X / K, fill = chi, size = X_rel)) +
  geom_point(color = "black", shape = 21) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = bquote("Access fee("~chi~")"),
                               frame.colour = "black",
                               ticks.colour = "black"),
         size = guide_legend(title = expression(B[r] / B[f]))) +
  plot_theme() +
  labs(x = l_legend,
       y = b_legend)

optimal_fee_for_L_plot

#### UPDATE GEOM DEFAULTS FOR SUBPLOTS

update_geom_defaults(geom = "point", new = list(size = 1.5,
                                                color = "transparent",
                                                shape = 21))

# Different fines
L_X_and_fines <- expand_grid(index = c(1:20),
                             w = c(0.5 * w, w, 10 * w, 20 * w)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, w_try = w) %>% 
  # mutate(chi_try = 500) %>%                                     # Saving just in case we want to see a fixed-chi effect
  mutate(results = pmap(.l = list(L = L_try,
                                  chi = chi_try,
                                  w = w_try),
                        .f = wrapper,
                        r = r,
                        K = K,
                        X0 = X0,
                        D = D,
                        p = p,
                        q = q,
                        c = c,
                        beta = beta,
                        alpha = alpha,
                        mu = mu,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec,
         w_try = as.factor(w_try)) %>% 
  select(w_try, L_try, chi_try, X_vec, X_rel)

L_X_and_fines_plot <- 
  ggplot(data = L_X_and_fines,
         mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_line(aes(group = w_try, color = w_try)) +
  geom_point() +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = expression("Fine"))) +
  theme(legend.position = c(0.6, 0),
        legend.justification = c(0.5, 0)) +
  labs(x = "",
       y = b_legend_short)

L_X_and_fines_plot

# Different enforcement costs
L_X_and_enforcement_costs <- expand_grid(index = c(1:20), alpha = c(0.02 * alpha, 0.05 * alpha, alpha, alpha * 2)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, alpha_try = alpha) %>% 
  mutate(results = pmap(.l = list(L = L_try, chi = chi_try, alpha = alpha_try),
                        .f = wrapper,
                        r = r,
                        K = K,
                        X0 = X0,
                        D = D,
                        p = p,
                        q = q,
                        c = c,
                        beta = beta,
                        mu = mu,
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec,
         alpha_try = factor(alpha_try, levels = sort(unique(alpha_try)))) %>% 
  select(alpha_try, L_try, chi_try, X_vec, X_rel)


L_X_and_enforcement_costs_plot <- 
  ggplot(data = L_X_and_enforcement_costs,
       mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_line(aes(group = alpha_try, color = alpha_try)) +
  geom_point() +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(expression("Enforcement costs"))) +
  theme(legend.position = c(0.6, 0),
        legend.justification = c(0.5, 0)) +
  labs(x = "",
       y = "")

L_X_and_enforcement_costs_plot

# Different fishing costs
L_X_and_fishing_costs <- expand_grid(index = c(1:20), c = c(2500, 3000, 3500, 4000)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, c_try = c) %>% 
  mutate(results = pmap(.l = list(L = L_try, chi = chi_try, c = c_try),
                        .f = wrapper,
                        r = r,
                        K = K,
                        X0 = X0,
                        D = D,
                        p = p,
                        q = q,
                        beta = beta,
                        alpha = alpha,
                        mu = mu,
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec,
         c_try = factor(c_try, levels = sort(unique(c_try)))) %>% 
  select(c_try, L_try, chi_try, X_vec, X_rel)


L_X_and_fishing_costs_plot <- 
  ggplot(data = L_X_and_fishing_costs,
         mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_line(aes(group = c_try, color = c_try)) +
  geom_point() +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend("Fishing costs",
                              ncol = 2)) +
  theme(legend.position = c(0.6, 0),
        legend.justification = c(0.5, 0)) +
  labs(x = l_legend,
       y = b_legend_short)

L_X_and_fishing_costs_plot




# Different dispersal scenarios
make_D <- function(self_rec){
  
  exports <- 1 - self_rec
  
  D <- matrix(c(self_rec, exports,
                exports, self_rec),
              nrow = 2,
              byrow = T)
  
  return(D)
}



L_X_and_dispersal <- 
  expand_grid(index = c(1:20), self_rec = c(0.3, 0.5, 0.7, 0.9)) %>% 
  mutate(D = map(self_rec, make_D)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, D_try = D) %>% 
  mutate(results = pmap(.l = list(L = L_try, chi = chi_try, D = D_try),
                        .f = wrapper,
                        r = r,
                        K = K,
                        X0 = X0,
                        p = p,
                        q = q,
                        c = c,
                        beta = beta,
                        alpha = alpha,
                        mu = mu,
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec,
         self_rec = factor(self_rec, levels = sort(unique(self_rec)))) %>% 
  select(self_rec, L_try, chi_try, X_vec, X_rel)


L_X_and_dispersal_plot <- 
  ggplot(data = L_X_and_dispersal,
       mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_line(aes(group = self_rec, color = self_rec)) +
  geom_point() +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = "Dispersal", ncol = 2)) +
  theme(legend.position = c(0.5, 0),
        legend.justification = c(0.5, 0)) +
  labs(x = l_legend,
       y = "")

L_X_and_dispersal_plot



figure2_subplots <- plot_grid(L_X_and_fines_plot,
                              L_X_and_enforcement_costs_plot,
                              L_X_and_fishing_costs_plot,
                              L_X_and_dispersal_plot,
                              ncol = 2,
                              labels = c("B", "C", "D", "E"))


figure2 <- plot_grid(optimal_fee_for_L_plot,
                     figure2_subplots,
                     ncol = 1,
                     labels = c("A", NA))


lazy_ggsave(plot = figure2,
            filename = "figure_2_optimized_chi",
            width = 18, height = 22)



















