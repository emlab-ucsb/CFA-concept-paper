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
b_legend <- "Equilibrium\nbiomass\n(X / K)"


best_results <- read.csv(here("results", "best_combination_of_L_and_Chi.csv"))

# Illegal Harvest
H_L_v_psi_chi <- expand_grid(index = c(1:20),
                             w_try = c(0, 5000, 10000, 15000)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi) %>% 
  mutate(results = pmap(.l = list(L = L_try, chi = chi_try, w = w_try),
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
  select(L_try, w_try, contains("H_i")) %>% 
  mutate(H_il_vec = H_il_vec / K,
         H_in_vec = H_in_vec / K,
         H_i = H_il_vec + H_in_vec)

# Total
total_illegal_harvest_plot <- 
  ggplot(H_L_v_psi_chi, aes(x = L_try, y = H_i, color = w_try, group = w_try)) +
  geom_line() +
  plot_theme() +
  scale_color_viridis_c() +
  scale_x_continuous(limits = c(0.1, 1),
                     breaks = seq(0.1, 1, by = 0.1)) +
  guides(color = guide_colorbar(title = quote("Fine ("~psi~")"),
                                frame.colour = "black",
                                ticks.colour = "black")) +
  labs(x = l_legend, y = "Equilibrium illegal\nharvest (H / K)")

lazy_ggsave(plot = total_illegal_harvest_plot,
            filename = "illegal_harvest_total_L_v_psi_plot",
            width = 10,
            height = 6.5)

# By patch
illegal_harvest_subset_plots <- 
  H_L_v_psi_chi %>% 
  select(-H_i) %>% 
  gather(patch, harvests, -c(L_try, w_try)) %>% 
  mutate(patch_new = case_when(patch == "H_il_vec" ~ "Lease zone (illegal)",
                               patch == "H_in_vec" ~ "No-take zone (illegal)")) %>%
  ggplot(aes(x = L_try, y = harvests, color = w_try, group = w_try)) +
  geom_line() +
  facet_wrap(~patch_new) +
  plot_theme() +
  scale_color_viridis_c() +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  guides(color = guide_colorbar(title = quote("Fine ("~psi~")"),
                                frame.colour = "black",
                                ticks.colour = "black")) +
  labs(x = l_legend, y = "Equilibrium illegal\nharvest (H/K)")

lazy_ggsave(plot = illegal_harvest_subset_plots,
            filename = "illegal_harvest_patch_L_v_psi_plot",
            width = 10,
            height = 6.5)

# Combine
fine_legend <- cowplot::get_legend(
  # create some space to the left of the legend
  total_illegal_harvest_plot +
    theme(legend.box.margin = margin(0, 0, 10, 10))
)


illegal_harvest_combined_plot <- 
  cowplot::plot_grid(
    # plots
    cowplot::plot_grid(
      total_illegal_harvest_plot + theme(legend.position="none"), 
      illegal_harvest_subset_plots + theme(legend.position = "none"),
      ncol = 1,
      rel_heights = c(1.4,1),
      labels = "AUTO"
    ),
    # legend
    fine_legend,
    rel_widths = c(2,0.5)
  )


illegal_harvest_combined_plot

lazy_ggsave(plot = illegal_harvest_combined_plot,
            filename = "figure_3_illegal_harvest_combined_plot",
            width = 10,
            height = 10)
