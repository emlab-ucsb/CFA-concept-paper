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

#### Creating heatmaps   #######################################################
#
# This script creates heatmaps of L (x-axis) and some key parametrs (y-axis)
# and equilibrium biomass (z-axis, or color...).
#
################################################################################

### --------------------------------------------------------
# Plot 1: L vs B for five different fines (chi is fixed)
### --------------------------------------------------------

L_X_and_fines <- expand_grid(L_try = L_range,
                             w_try = (w_range_multipliers_extend*w)) %>% 
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

L_X_and_fines_heatmap <- 
  ggplot(data = L_X_and_fines,
         mapping = aes(x = L_try, y = w_try, fill = X_vec / K)) +
  geom_raster() +
  scale_fill_viridis_c(name = X_legend_short, limits = c(0.65, 0.9)) +
  plot_theme() +
  labs(x = "",
       y = bquote("Fine ("~psi~")")) +
  scale_y_discrete(labels = paste0(as.character(w_range_multipliers_extend), "x"), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               frame.colour = "black"))

L_X_and_fines_heatmap

### --------------------------------------------------------------------
# Plot 2: L vs B for five different enforcement costs (chi is optimized)
### --------------------------------------------------------------------

L_X_and_enforcement_costs <- expand_grid(L_try = L_range,
                                         alpha_try = alpha_range_multipliers_extend*alpha) %>% 
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


L_X_and_enforcement_costs_heatmap <- 
  ggplot(data = L_X_and_enforcement_costs,
         mapping = aes(x = L_try, y = alpha_try, fill = X_vec / K)) +
  geom_raster() +
  scale_fill_viridis_c(name = X_legend_short, limits = c(0.65, 0.9)) +
  plot_theme() +
  labs(x = "",
       y = bquote("Enforcement costs ("~alpha~")")) +
  scale_y_discrete(labels = paste0(as.character(alpha_range_multipliers_extend), "x"), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

L_X_and_enforcement_costs_heatmap

### --------------------------------------------------------------------
# Plot 3: L vs B for five different fishing costs (chi is fixed)
### --------------------------------------------------------------------

L_X_and_fishing_costs <- expand_grid(L_try = L_range,
                                     c_try = c * c_range_multipliers_extend) %>% 
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


L_X_and_fishing_costs_heatmap <- 
  ggplot(data = L_X_and_fishing_costs,
         mapping = aes(x = L_try, y = c_try, fill = X_vec / K)) +
  geom_raster() +
  scale_fill_viridis_c(name = X_legend_short, limits = c(0.65, 0.9)) +
  plot_theme() +
  labs(x = "",
       y = bquote("Fishing costs (c)")) +
  scale_y_discrete(labels = paste0(as.character(c_range_multipliers_extend), "x"), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

L_X_and_fishing_costs_heatmap

### --------------------------------------------------------------------
# Plot 4: L vs B for four different dispersal scenarios (chi is fixed)
### --------------------------------------------------------------------

L_X_and_dispersal <- 
  expand_grid(L_try = L_range,
              self_rec = self_rec_range_extend) %>% 
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
  mutate(X_rel = X_r_vec / X_f_vec) %>% 
  select(self_rec, L_try, X_vec, X_rel)


L_X_and_dispersal_heatmap <- 
  ggplot(data = L_X_and_dispersal,
         mapping = aes(x = L_try, y = self_rec, fill = X_vec / K)) +
  geom_raster() +
  scale_fill_viridis_c(name = X_legend_short, limits = c(0.65, 0.9)) +
  plot_theme() +
  labs(x = "",
       y =bquote("Self-recruitment ("~d[`M,M`]~")")) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

L_X_and_dispersal_heatmap


### --------------------------------------------------------------------
# Plot 5: L vs B for four different access fee scenarios (chi)
### --------------------------------------------------------------------

L_X_and_chi <- 
  expand_grid(L_try = L_range,
              chi_try = chi * chi_range_multipliers_extend) %>% 
  mutate(results = pmap(.l = list(L = L_try,
                                  chi = chi_try),
                        .f = wrapper,
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
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec,
         chi_try = factor(chi_try, levels = sort(unique(chi_try)))) %>% 
  select(chi_try, L_try, X_vec, X_rel)


L_X_and_chi_heatmap <- 
  ggplot(data = L_X_and_chi,
         mapping = aes(x = L_try, y = chi_try, fill = X_vec / K)) +
  geom_raster() +
  scale_fill_viridis_c(name = X_legend_short, limits = c(0.65, 0.9)) +
  plot_theme() +
  labs(x = "",
       y = bquote("Access fee ("~chi~")")) +
  scale_y_discrete(labels = paste0(as.character(chi_range_multipliers_extend), "x"), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

L_X_and_chi_heatmap


### --------------------------------------------------------------------
# Plot 6: L vs B for four different prices for catch (fixed chi)
### --------------------------------------------------------------------

L_X_and_p <- 
  expand_grid(L_try = L_range,
              p_try = p * p_range_multipliers) %>% 
  mutate(results = pmap(.l = list(L = L_try,
                                  p = p_try),
                        .f = wrapper,
                        chi = chi,
                        r = r,
                        K = K,
                        X0 = X0,
                        s = s,
                        D = D,
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
         p_try = factor(p_try, levels = sort(unique(p_try)))) %>% 
  select(p_try, L_try, X_vec, X_rel)


L_X_and_p_heatmap <- 
  ggplot(data = L_X_and_p,
         mapping = aes(x = L_try, y = p_try, fill = X_vec / K)) +
  geom_raster() +
  scale_fill_viridis_c(name = X_legend_short, limits = c(0.65, 0.9)) +
  plot_theme() +
  labs(x = "",
       y = "Price of fish (p)") +
  scale_y_discrete(labels = paste0(as.character(p_range_multipliers), "x"), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

L_X_and_p_heatmap


### ------------------------------------------------------------------
# MAIN PLOT
### ------------------------------------------------------------------


legend <- get_legend(L_X_and_fines_heatmap)

main_plot <- plot_grid(L_X_and_fines_heatmap +
                         theme(legend.position = "none"),
                       L_X_and_enforcement_costs_heatmap,
                       L_X_and_fishing_costs_heatmap,
                       L_X_and_dispersal_heatmap,
                       L_X_and_chi_heatmap,
                       L_X_and_p_heatmap,
                       ncol = 2)

plot_with_legend <- plot_grid(main_plot,
                              legend,
                              rel_widths = c(9, 1))

plot_with_legend

lazy_ggsave(plot = plot_with_legend,
            filename = "FigureS2",
            width = 18,
            height = 22)

















