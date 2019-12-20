##### Add file header!!!

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
# The code below shows an example of how to call it for a series of possible
# combinations of L and w. Note how these are passed through a list referencing
# column names in a tibble, while the rest of the variables are passed as the 
# ... argument to pmap.
#
################################################################################

l_legend <- "Lease area as a proportion of total reserve size (L)"
b_legend <- "Equilibrium\nbiomass\n(X / K)"
e_legend <- "Equilibrium\neffort"
  
### -----------------
### BIOMASS HEAT MAPS
### -----------------

L <- seq(0.1, 1, by = 0.01)

# 1) Equilibrium biomass as a function of lease area (L) and enforcement coefficient (mu)

mu_new <- seq(0.00005, 0.01, by = 0.0002)

optim_B_mu <- expand_grid(L, mu_new) %>% 
  mutate(results = pmap_dbl(.l = list(L = L, mu = mu_new),
                            .f = biomass_optim_wrapper,
                            r = r,
                            K = K,
                            X0 = X0,
                            D = D,
                            p = p,
                            q = q,
                            c = c,
                            beta = beta,
                            alpha = alpha,
                            w = w,
                            years = years,
                            want = "All")) %>%
  unnest(cols = results)

# 2) Equilibrium biomass as a function of lease area (L) and fine (W)

w_new <- seq(100, 20000, by = 1000)

B_L_v_psi <- expand_grid(L, w_new) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, w = w_new),
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
                            chi = chi,
                            years = years,
                            want = "X_vec"))

# 3) Equilibrium biomass as a function of lease area (L) and access fee (chi)

chi_new <- seq(1, 30000, by = 500)

B_L_v_chi <- expand_grid(L, chi_new) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, chi = chi_new),
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
                            w = w,
                            years = years,
                            want = "X_vec"))


# 4) Equilibrium biomass as a function of lease area (L) and enforcement cost (alpha)

alpha_new <- seq(10, 50000, by = 1000)

B_L_v_alpha <- expand_grid(L, alpha_new) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, alpha = alpha_new),
                            .f = wrapper,
                            r = r,
                            K = K,
                            X0 = X0,
                            D = D,
                            p = p,
                            q = q,
                            chi = chi,
                            beta = beta,
                            c = c,
                            mu = mu,
                            w = w,
                            years = years,
                            want = "X_vec"))

# Get common biomass scale 

biomass_range <- c(range(B_L_v_mu$equil_b), range(B_L_v_psi$equil_b), range(B_L_v_chi$equil_b), range(B_L_v_alpha$equil_b))/K
biomass_min <- min(biomass_range)
biomass_max <- max(biomass_range)
biomass_breaks <- seq(round(biomass_min, 2), round(biomass_max, 2), length.out = 5)


# Make plots
# 1)
B_L_v_mu_plot <- ggplot(B_L_v_mu, aes(x = L, y = mu_new, fill = equil_b / K, z = equil_b)) +
  geom_raster(interpolate = T) +
  geom_contour(color = "black") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Enforcement coefficient ("*mu*")")) +
  scale_fill_gradientn(colors = viridis_pal(option = "C")(9), limits=c(biomass_min, biomass_max), breaks = biomass_breaks, 
                       na.value = "#FDE725FF")+
  guides(fill = guide_colorbar(title = b_legend,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  plot_theme()

lazy_ggsave(plot = B_L_v_mu_plot,
            filename = "biomass_L_v_mu_heat_plot",
            width = 10,
            height = 6.5)

# 2)
B_L_v_psi_plot <- ggplot(B_L_v_psi, aes(x = L, y = w_new, fill = equil_b / K, z = equil_b)) +
  geom_raster(interpolate = T) +
  geom_contour(color = "black") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Fine ("*psi*")"), labels = dollar) +
  scale_fill_gradientn(colors = viridis_pal(option = "C")(9), limits=c(biomass_min, biomass_max), breaks = biomass_breaks, 
                       na.value = "#FDE725FF")+
  guides(fill = guide_colorbar(title = b_legend,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  plot_theme()

lazy_ggsave(plot = B_L_v_psi_plot,
            filename = "biomass_L_v_psi_heat_plot",
            width = 10,
            height = 6.5)

# 3)
B_L_v_chi_plot <- ggplot(B_L_v_chi, aes(x = L, y = chi_new, fill = equil_b / K, z = equil_b)) +
  geom_raster(interpolate = T) +
  geom_contour(color = "black") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Access fee ("*chi*")"), labels = dollar) +
  scale_fill_gradientn(colors = viridis_pal(option = "C")(9), limits=c(biomass_min, biomass_max), breaks = biomass_breaks, 
                       na.value = "#FDE725FF")+
  guides(fill = guide_colorbar(title = b_legend,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  plot_theme()

lazy_ggsave(plot = B_L_v_chi_plot,
            filename = "biomass_L_v_chi_heat_plot",
            width = 10,
            height = 6.5)

# 4)
B_L_v_alpha_plot <- ggplot(B_L_v_alpha, aes(x = L, y = alpha_new, fill = equil_b / K, z = equil_b)) +
  geom_raster(interpolate = T) +
  geom_contour(color = "black") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Variable cost of enforcement ("*alpha*")"), labels = dollar) +
  scale_fill_gradientn(colors = viridis_pal(option = "C")(9), limits=c(biomass_min, biomass_max), breaks = biomass_breaks, 
                       na.value = "#FDE725FF")+
  guides(fill = guide_colorbar(title = b_legend,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  plot_theme()

lazy_ggsave(plot = B_L_v_alpha_plot,
            filename = "biomass_L_v_alpha_heat_plot",
            width = 10,
            height = 6.5)

# Combine

B_legend <- cowplot::get_legend(
  # create some space to the left of the legend
  B_L_v_mu_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
)


B_combined_plot <- 
    cowplot::plot_grid(
      # plots
      cowplot::plot_grid(
        B_L_v_mu_plot + theme(legend.position="none"), 
        B_L_v_psi_plot + theme(legend.position = "none"),
        B_L_v_chi_plot + theme(legend.position = "none"),
        B_L_v_alpha_plot + theme(legend.position = "none"),
        nrow = 2, 
        ncol = 2,
        labels = "AUTO",
        label_size = 16
        ),
      # legend
      B_legend,
      rel_widths = c(2,0.3)
    )
  
# Save plot
lazy_ggsave(plot = B_combined_plot,
            filename = "biomass_combined_heat_plot",
            width = 10,
            height = 8.5)



### EFFORT HEAT MAPS ###

## 1) Equilibrium effort as a function of lease area (L) and access fee (chi)
E_L_v_chi <- expand_grid(L = L, chi = chi_new) %>% 
  mutate(results = pmap(.l = list(L = L, chi = chi),
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
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  select(L, chi, contains("E_"), -E_e_vec) %>% 
  # filter(E_l_vec > 0) %>% 
  gather(patch, effort, -c(L, chi)) %>% 
  group_by(patch) %>% 
  mutate(max_e = max(effort)) %>% 
  ungroup() %>% 
  mutate(effort_norm = effort / max_e,
         patch = case_when(patch == "E_f_vec" ~ "Fishing zone",
                           patch == "E_l_vec" ~ "Lease zone (legal)",
                           patch == "E_il_vec" ~ "Lease zone (illegal)",
                           T ~ "No-take zone (illegal)"))

# 2) Equilibrium effort as a function of lease area (L) and enforcement cost (alpha)
E_L_v_alpha <- expand_grid(L = L, alpha = alpha_new) %>% 
  mutate(results = pmap(.l = list(L = L, alpha = alpha),
                        .f = wrapper,
                        r = r,
                        K = K,
                        X0 = X0,
                        D = D,
                        p = p,
                        q = q,
                        c = c,
                        beta = beta,
                        chi = chi,
                        mu = mu,
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  select(L, alpha, contains("E_"), -E_e_vec) %>% 
  # filter(E_l_vec > 0) %>% 
  gather(patch, effort, -c(L, alpha)) %>% 
  group_by(patch) %>% 
  mutate(max_e = max(effort)) %>% 
  ungroup() %>% 
  mutate(effort_norm = effort / max_e,
         patch = case_when(patch == "E_f_vec" ~ "Fishing zone",
                           patch == "E_l_vec" ~ "Lease zone (legal)",
                           patch == "E_il_vec" ~ "Lease zone (illegal)",
                           T ~ "No-take zone (illegal)"))


# Make plots
# 1)
E_L_v_chi_plot <-
  ggplot(data = E_L_v_chi,
         mapping = aes(x = L, y = chi, fill = effort_norm, z = effort_norm)) +
  geom_raster(interpolate = T) +
  geom_contour(color = "black") +
  facet_wrap(~patch, ncol = 2) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(expand = c(0, 0), labels = dollar) +
  scale_fill_viridis_c(option = "A") +
  guides(fill = guide_colorbar(title = e_legend,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = l_legend, y = quote("Access Fee ("~chi~")")) +
  #ggtitle(label = "Equilibrium effort in each zone") +
  plot_theme()

lazy_ggsave(plot = E_L_v_chi_plot,
            filename = "effort_L_v_chi_heat_plot",
            width = 10,
            height = 8.5)

# 2)
E_L_v_alpha_plot <- 
  ggplot(data = E_L_v_alpha,
         mapping = aes(x = L, y = alpha, fill = effort_norm, z = effort_norm)) +
  geom_raster(interpolate = T) +
  geom_contour(color = "black") +
  facet_wrap(~patch, ncol = 2) +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = dollar) +
  scale_fill_viridis_c(option = "A") +
  guides(fill = guide_colorbar(title = e_legend,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = l_legend, y = quote("Variable cost of enforcement ("~alpha~")")) +
  #ggtitle(label = "Equilibrium effort in each zone") +
  plot_theme()

lazy_ggsave(plot = E_L_v_alpha_plot,
            filename = "effort_L_v_alpha_heat_plot",
            width = 10,
            height = 8.5)


### SCATTER PLOTS ###

# 1) Biomass 
B_L_v_psi_chi <- expand_grid(L, w_new, chi_new = c(500, 10000)) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, w = w_new, chi = chi_new),
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
                            want = "X_vec") / K,
         chi_label = paste0("Access fee : $", format(chi_new, big.mark = ",")),
         chi_label = fct_reorder(chi_label, chi_new))

B_L_v_psi_line_plot <- 
  ggplot(B_L_v_psi_chi, aes(x = L, y = equil_b, color = w_new, group = w_new)) +
  geom_line() +
  plot_theme() +
  scale_color_viridis_c()+
  facet_wrap(~chi_label, ncol = 1) +
  scale_x_continuous(limits = c(0.1, 1),
                     breaks = seq(0.1, 1, by = 0.1)) +
  guides(color = guide_colorbar(title = quote("Fine ("~psi~")"),
                                frame.colour = "black",
                                ticks.colour = "black")) +
labs(x = l_legend, y = b_legend)

lazy_ggsave(plot = B_L_v_psi_line_plot,
            filename = "biomass_v_L_line_plot",
            width = 7,
            height = 8.5)




# 3) Biomass accrual

# Biomass accrual by fine
w_new <- seq(100, 20000, by = 4000)

biomass_accrual_w <- expand_grid(L, w_new) %>% 
  mutate(results = pmap(.l = list(L = L, w = w_new),
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
                        chi = chi,
                        mu = mu,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec)


biomass_accrual_w_plot <-
  ggplot(data = biomass_accrual_w,
         mapping = aes(x = L, y = X_rel, size = X_vec / K, fill = w_new)) +
  geom_point(shape = 21, alpha = 0.75, color = "black") +
  labs(x = l_legend,
       y = expression(frac(X[R], X[L]))) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, 1)) +
  guides(size = guide_legend(title = b_legend),
         fill = guide_colorbar(title = quote("Fine ("~psi~")"),
                               frame.colour = "black",
                               ticks.colour = "black")) +
  plot_theme() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1)


lazy_ggsave(plot = biomass_accrual_w_plot,
            filename = "biomass_accrual_w_plot",
            width = 10,
            height = 6.5)


# Biomass accrual by access fee (only include scenarios where legal fishing occurs in the lease area)
chi_new <- seq(0, 24000, by = 2000)

biomass_accrual_chi <- expand_grid(L, chi_new) %>% 
  mutate(results = pmap(.l = list(L = L, chi = chi_new),
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
                        w = w,
                        mu = mu,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec)


biomass_accrual_chi_plot <- biomass_accrual_chi %>%
  dplyr::filter(E_l_vec > 0) %>%
  ggplot()+
  aes(x = L, y = X_rel, size = X_vec / K, fill = chi_new) +
  geom_point(shape = 21, alpha = 0.75, color = "black") +
  labs(x = l_legend,
       y = expression(frac(X[R], X[L]))) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, 1)) +
  guides(size = guide_legend(title = b_legend),
         fill = guide_colorbar(title = quote("Access fee ("~chi~")"),
                               frame.colour = "black",
                               ticks.colour = "black")) +
  plot_theme() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1)


lazy_ggsave(plot = biomass_accrual_chi_plot,
            filename = "biomass_accrual_chi_fishingonly_plot",
            width = 10,
            height = 6.5)

# show what happens when combinations of chi and L result in no legal fishing
chi_new_high <- seq(0, 42000, by = 2000)

biomass_accrual_chi_high <- expand_grid(L, chi_new_high) %>% 
  mutate(results = pmap(.l = list(L = L, chi = chi_new_high),
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
                        w = w,
                        mu = mu,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec)

biomass_accrual_chi_high_plot <-
  ggplot(data = biomass_accrual_chi_high,
         mapping = aes(x = L, y = X_rel, size = X_vec / K, fill = chi_new_high)) +
  geom_point(shape = 21, alpha = 0.75, color = "black") +
  labs(x = l_legend,
       y = expression(frac(X[R], X[L]))) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, 1)) +
  guides(size = guide_legend(title = b_legend),
         fill = guide_colorbar(title = quote("Access fee ("~chi~")"),
                               frame.colour = "black",
                               ticks.colour = "black")) +
  plot_theme() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1)

lazy_ggsave(plot = biomass_accrual_chi_high_plot,
            filename = "biomass_accrual_chi_all_plot",
            width = 10,
            height = 6.5)



