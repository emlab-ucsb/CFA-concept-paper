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

# Different fines
Ls <- seq(0.05, 1, by = 0.05)                           # Define a vector of L values

L_X_and_fines <- expand_grid(L_try = Ls,
                        w_try = c(0, 5000, 10000, 15000)) %>% 
  mutate(results = pmap(.l = list(L = L_try,
                                  w = w_try),
                        .f = wrapper,
                        chi = chi,
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
  select(w_try, L_try, X_vec, X_rel)

L_X_and_fines_plot <- 
  ggplot(data = L_X_and_fines,
         mapping = aes(x = L_try, y = X_vec / K)) +
  geom_line(aes(group = w_try, color = w_try)) +
  geom_point(fill = "steelblue", size = 3, color = "black", shape = 21) +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = expression("Fine("~psi~")"))) +
  labs(x = l_legend,
       y = b_legend)

L_X_and_fines_plot

# Different enforcement costs
L_X_and_enforcement_costs <- expand_grid(L_try = Ls,
                                         alpha_try = c(10, 100, 1000, 10000)) %>% 
  mutate(results = pmap(.l = list(L = L_try, alpha = alpha_try),
                        .f = wrapper,
                        chi = chi,
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
  select(alpha_try, L_try, X_vec, X_rel)


L_X_and_enforcement_costs_plot <- 
  ggplot(data = L_X_and_enforcement_costs,
         mapping = aes(x = L_try, y = X_vec / K)) +
  geom_line(aes(group = alpha_try, color = alpha_try)) +
  geom_point(fill = "steelblue", size = 3, color = "black", shape = 21) +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(expression("Enforcement\ncosts ("~alpha~")"))) +
  labs(x = l_legend,
       y = b_legend)

L_X_and_enforcement_costs_plot

# Different fishing costs
L_X_and_fishing_costs <- expand_grid(L_try = Ls,
                                     c_try = c(1000, 3000, 5000)) %>% 
  mutate(results = pmap(.l = list(L = L_try, c = c_try),
                        .f = wrapper,
                        chi = chi,
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
  select(c_try, L_try, X_vec, X_rel)


L_X_and_fishing_costs_plot <- 
  ggplot(data = L_X_and_fishing_costs,
         mapping = aes(x = L_try, y = X_vec / K)) +
  geom_line(aes(group = c_try, color = c_try)) +
  geom_point(fill = "steelblue", size = 3, color = "black", shape = 21) +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend("Fishing\ncosts (c)")) +
  labs(x = l_legend,
       y = b_legend)

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
  expand_grid(L_try = Ls,
              self_rec = c(0.3, 0.5, 0.7, 0.9)) %>% 
  mutate(D_try = map(self_rec, make_D)) %>% 
  mutate(results = pmap(.l = list(L = L_try, D = D_try),
                        .f = wrapper,
                        chi = chi,
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
  select(self_rec, L_try, X_vec, X_rel)


L_X_and_dispersal_plot <- 
  ggplot(data = L_X_and_dispersal,
         mapping = aes(x = L_try, y = X_vec / K)) +
  geom_line(aes(group = self_rec, color = self_rec)) +
  geom_point(fill = "steelblue", size = 3, color = "black", shape = 21) +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE,
         size = FALSE,
         color = guide_legend(title = "Self-recruitment")) +
  labs(x = l_legend,
       y = b_legend)

L_X_and_dispersal_plot



figure2_subplots_fixed_chi <- plot_grid(L_X_and_fines_plot,
                                        L_X_and_enforcement_costs_plot,
                                        L_X_and_fishing_costs_plot,
                                        L_X_and_dispersal_plot,
                                        ncol = 2,
                                        labels = "AUTO")


lazy_ggsave(plot = figure2_subplots_fixed_chi,
            filename = "figure2_subplots_fixed_chi",
            width = 10, height = 5)



















