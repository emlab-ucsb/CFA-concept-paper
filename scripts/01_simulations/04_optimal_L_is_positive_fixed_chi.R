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

# For loop iterating across all values of L - all other parameters are fixed

for (L in L_range){
  
  run <- wrapper(chi = chi, 
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

  if(L == L_range[1]){
    best_results <- run
    
  }else{
    best_results <- rbind(best_results, run)
  }
  
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
  labs(x = L_legend,
       y = X_legend)

optimal_fee_for_L_plot


### ------------------------------------------------------
# Table 1: Information of optimal L for default paramters (chi is fixed)
### ------------------------------------------------------

# max_conservation_benefit <- best_results %>%
#   dplyr::filter(L == 0 | X_vec == max(X_vec)) %>%
#   dplyr::select(L, X = X_vec, )


#### UPDATE GEOM DEFAULTS FOR SUBPLOTS

update_geom_defaults(geom = "point", new = list(size = 1.5))

### --------------------------------------------------------
# Plot 2: L vs B for five different fines (chi is fixed)
### --------------------------------------------------------

L_X_and_fines <- expand_grid(L_try = L_range,
                             w_try = (w_range_multipliers*w)) %>% 
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
  geom_point() +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1",
                     labels = paste0(as.character(w_range_multipliers), "x")) +
  plot_theme() +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = bquote("Fine ("~psi~")"),
                              title.position = "top",
                              title.hjust = 0.5)) +
  labs(x = "",
       y = X_legend_short)

L_X_and_fines_plot

### --------------------------------------------------------------------
# Plot 3: L vs B for five different enforcement costs (chi is optimized)
### --------------------------------------------------------------------

L_X_and_enforcement_costs <- expand_grid(L_try = L_range,
                                         alpha_try = alpha_range_multipliers*alpha) %>% 
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
  geom_point() +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1",
                     labels = paste0(as.character(alpha_range_multipliers), "x")) +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = bquote("Enforcement costs ("~alpha~")"),
                              title.position = "top",
                              title.hjust = 0.5)) +
  theme(legend.position = "top") +
  labs(x = "",
       y = "")

L_X_and_enforcement_costs_plot

### --------------------------------------------------------------------
# Plot 4: L vs B for five different fishing costs (chi is fixed)
### --------------------------------------------------------------------

L_X_and_fishing_costs <- expand_grid(L_try = L_range,
                                     c_try = c_range_multipliers*c) %>% 
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
  geom_point() +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1",
                     labels = paste0(as.character(c_range_multipliers), "x")) +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = "Fishing costs (c)",
                              title.position = "top",
                              title.hjust = 0.5)) +
  theme(legend.position = "top") +
  labs(x = L_legend,
       y = X_legend_short)

L_X_and_fishing_costs_plot

### --------------------------------------------------------------------
# Plot 5: L vs B for four different dispersal scenarios (chi is fixed)
### --------------------------------------------------------------------

L_X_and_dispersal <- 
  expand_grid(L_try = L_range,
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
  geom_point(aes()) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1",
                     labels = as.character(self_rec_range)) +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = bquote("Self-recruitment ("~d[`M,M`]~")"),
                              title.position = "top",
                              title.hjust = 0.5)) +
  theme(legend.position = "top") +
  labs(x = L_legend,
       y = "")

L_X_and_dispersal_plot


### --------------
# Combined figure
### --------------


fig2_subplots <- plot_grid(L_X_and_fines_plot,
                              L_X_and_enforcement_costs_plot,
                              L_X_and_fishing_costs_plot,
                              L_X_and_dispersal_plot,
                              ncol = 2,
                              labels = c("B", "C", "D", "E"))


fig2 <- plot_grid(optimal_fee_for_L_plot,
                  fig2_subplots,
                  ncol = 1,
                  rel_heights = c(0.8,1),
                  labels = c("A", NA))


lazy_ggsave(plot = fig2,
            filename = "Figure2",
            width = 18, height = 22)



















