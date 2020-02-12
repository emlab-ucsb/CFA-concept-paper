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

for (L in Ls){
  
  run <- wrapper(chi = chi,         # Call the simulation to calculate B_r / B_f
                 r = r,
                 K = K,
                 X0 = X0,
                 s = s,
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
                 b = b) %>% 
    mutate(X_rel = X_r_vec / X_f_vec,
           L = L)
  
  if(L == Ls[1]){
    best_results <- run
  }
  
  best_results <- rbind(best_results, run)
  
}

### ------------------------------------------------------
# Plot 1: L vs B for default parameters (chi is fixed)
### ------------------------------------------------------

optimal_fee_for_L_plot <- 
  ggplot(data = best_results,
         mapping = aes(x = L, y = X_vec / K, fill = X_rel, size = X_rel)) +
  geom_point(color = "black", shape = 21, fill = NA) +
  geom_vline(data = filter(best_results, X_vec == max(X_vec)),
             aes(xintercept = L),
             linetype = "dashed") +
  scale_fill_viridis_c() +
  plot_theme() +
  guides(size = guide_legend(title = expression(X[M] / X[`F`]))) +
  theme(legend.position = c(0.01, 1),
        legend.justification = c(0, 1)) +
  labs(x = l_legend,
       y = b_legend)

optimal_fee_for_L_plot


#### UPDATE GEOM DEFAULTS FOR SUBPLOTS

update_geom_defaults(geom = "point", new = list(size = 1.5))

### --------------------------------------------------------
# Plot 2: L vs B for five different fines (chi is fixed)
### --------------------------------------------------------

w_range <- c(0.1 * w,
             0.5 * w, 
             w,
             2 * w,
             10 * w)

L_X_and_fines <- expand_grid(L_try = Ls,
                             w_try = w_range) %>% 
  mutate(results = pmap(.l = list(L = L_try,
                                  w = w_try),
                        .f = wrapper,
                        chi = chi,
                        r = r,
                        K = K,
                        X0 = X0,
                        s = s,
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
  select(w_try, L_try, X_vec, X_rel)

L_X_and_fines_plot <- 
  ggplot(data = L_X_and_fines,
         mapping = aes(x = L_try, y = X_vec / K, group = w_try, color = w_try)) +
  geom_line() +
  geom_point(color = "black", fill = NA, shape = 21) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(color = guide_legend(title = bquote("Fine ("~psi~")"))) +
  theme(legend.position = c(0.75, 1),
        legend.justification = c(0, 1)) +
  labs(x = "",
       y = b_legend_short)

L_X_and_fines_plot

### --------------------------------------------------------------------
# Plot 3: L vs B for five different enforcement costs (chi is optimized)
### --------------------------------------------------------------------

alpha_range <- c(0.1 * alpha,
                 0.5 * alpha,
                 alpha,
                 2 * alpha,
                 10 * alpha)

L_X_and_enforcement_costs <- expand_grid(L_try = Ls,
                                         alpha_try = alpha_range) %>% 
  mutate(results = pmap(.l = list(L = L_try,
                                  alpha = alpha_try),
                        .f = wrapper,
                        chi = chi,
                        r = r,
                        K = K,
                        X0 = X0,
                        s = s,
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
  select(alpha_try, L_try, X_vec, X_rel)


L_X_and_enforcement_costs_plot <- 
  ggplot(data = L_X_and_enforcement_costs,
         mapping = aes(x = L_try, y = X_vec / K, group = alpha_try, color = alpha_try)) +
  geom_line() +
  geom_point(color = "black", fill = NA, shape = 21) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(bquote("Enforcement costs ("~alpha~")"))) +
  theme(legend.position = c(0.7, 1),
        legend.justification = c(0, 1)) +
  labs(x = "",
       y = "")

L_X_and_enforcement_costs_plot

### --------------------------------------------------------------------
# Plot 4: L vs B for five different fishing costs (chi is fixed)
### --------------------------------------------------------------------

c_range <- c(0.8 * c,
             0.9 * c,
             c,
             1.1 * c,
             1.2* c)

L_X_and_fishing_costs <- expand_grid(L_try = Ls,
                                     c_try = c_range) %>% 
  mutate(results = pmap(.l = list(L = L_try,
                                  c = c_try),
                        .f = wrapper,
                        chi = chi,
                        r = r,
                        K = K,
                        X0 = X0,
                        s = s,
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
  select(c_try, L_try, X_vec, X_rel)


L_X_and_fishing_costs_plot <- 
  ggplot(data = L_X_and_fishing_costs,
         mapping = aes(x = L_try, y = X_vec / K, group = c_try, color = c_try)) +
  geom_line() +
  geom_point(color = "black", fill = NA, shape = 21) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = "Fishing costs (c)",
                              ncol = 2)) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0.01, 1)) +
  labs(x = l_legend,
       y = b_legend_short)

L_X_and_fishing_costs_plot

### --------------------------------------------------------------------
# Plot 5: L vs B for four different dispersal scenarios (chi is fixed)
### --------------------------------------------------------------------

# Different dispersal scenarios

make_D <- function(self_rec){
  
  exports <- 1 - self_rec
  
  D <- matrix(c(self_rec, exports,
                exports, self_rec),
              nrow = 2,
              byrow = T)
  
  return(D)
}

self_rec_range <- c(0.3, 0.5, 0.7, 0.9)

L_X_and_dispersal <- 
  expand_grid(L_try = Ls,
              self_rec = self_rec_range) %>% 
  mutate(D_try = map(self_rec, make_D)) %>% 
  mutate(results = pmap(.l = list(L = L_try,
                                  D = D_try),
                        .f = wrapper,
                        chi = chi,
                        r = r,
                        K = K,
                        X0 = X0,
                        s = s,
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
  select(self_rec, L_try, X_vec, X_rel)


L_X_and_dispersal_plot <- 
  ggplot(data = L_X_and_dispersal,
         mapping = aes(x = L_try, y = X_vec / K, group = self_rec, color = self_rec)) +
  geom_line(aes()) +
  geom_point(shape = 21, color = "black", fill = NA) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = "Self-recruitment", ncol = 2)) +
  theme(legend.position = c(0.75, 1),
        legend.justification = c(0, 1)) +
  labs(x = l_legend,
       y = "")

L_X_and_dispersal_plot


### --------------
# Combined figure
### --------------


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
            filename = "figure_2_fixed_chi",
            width = 18, height = 22)



















