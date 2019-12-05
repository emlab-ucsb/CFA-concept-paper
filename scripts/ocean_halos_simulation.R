##### Add file header!!!

# Load packages
library(here)
library(cowplot)
library(tidyverse)

# Source functions
source(here("scripts", "01_model.R"))
source(here("scripts", "02_wrapper.R"))
source(here("scripts", "03_default_parameters.R"))

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

#### Plot theme #####

text_size <- 12
title_size <- 14

plot_theme <- theme_bw()+
  theme(text = element_text(size = text_size),
        plot.title = element_text(size = title_size))

l_legend <- "Lease area, L, as a fraction of total reserve size"
b_legend <- "Equilibrium \n biomass"
  
#### Plots we want ########

# HEAT MAPS

L <- seq(0.1, 1, length.out = 10)
w <- w*10

# Biomass heat maps here

# 1) mu vs L
# Changing W and running this shifts the pattern left (optimal L shifts from ~ 0.5 to ~ 0.35 if W increases by an order of magnitude)
# Changing chi and running this shifts the pattern right (optimal L shifts from ~0.5 to ~ 0.58 if chi increases by an order of magnitude)
# Changine c and running this changes the pattern slightly (increasing c by 5-30x)

mu_new <- seq(0.0001, 0.01, by = 0.0002)

# Call the model on each combination of parameters
res_1 <- expand_grid(L, mu_new) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, mu = mu_new),
                            .f = wrapper,
                            r = r,
                            K = K,
                            X0 = X0,
                            f = f,
                            p = p,
                            q = q,
                            c = c,
                            beta = beta,
                            alpha = alpha,
                            w = w,
                            chi = chi,
                            years = years,
                            want = "X_vec"))


res_1_plot <- ggplot(res_1, aes(x = L, y = mu_new, fill = equil_b, z = equil_b)) +
  geom_raster(interpolate = T) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Enforcement coefficient, "*mu)) +
  scale_fill_viridis_c(name = b_legend)
  plot_theme 

# 2) fine vs L
# Changing r and running this cleans up the patchiness (try 0.5-2 times r). Slower growing species favor higher lease areas once the fine exceeds ~ 38000. Faster growing species favor a small lease area (the weird channel) above that point.  
# Increasing chi doesn't change the pattern, but shifts everything right (increase chi by order of magnitude) 
# Increasing c by an order of magnitude both increases total biomass and changes the pattern (increasing c order of magnitude)

w_new <- seq(0, 34000, by = 500)

# Call the model on each combination of parameters
res_2 <- expand_grid(L, w_new) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, w = w_new),
                            .f = wrapper,
                            r = r,
                            K = K,
                            X0 = X0,
                            f = f,
                            p = p,
                            q = q,
                            c = c,
                            beta = beta,
                            alpha = alpha,
                            mu = mu,
                            chi = chi,
                            years = years,
                            want = "X_vec"))


res_2_plot <- ggplot(res_2, aes(x = L, y = w_new, fill = equil_b, z = equil_b)) +
  geom_raster(interpolate = T) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Fine, "*psi)) +
  scale_fill_viridis_c(name = b_legend) +
  plot_theme


# 3) access fee vs L

chi_new <- seq(0, 40000, by = 500)

# Call the model on each combination of parameters
res_3 <- expand_grid(L, chi_new) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, chi = chi_new),
                            .f = wrapper,
                            r = r,
                            K = K,
                            X0 = X0,
                            f = f,
                            p = p,
                            q = q,
                            c = c,
                            beta = beta,
                            alpha = alpha,
                            mu = mu,
                            w = w,
                            years = years,
                            want = "X_vec"))


res_3_plot <- ggplot(res_3, aes(x = L, y = chi_new, fill = equil_b, z = equil_b)) +
  geom_raster(interpolate = T) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Access fee, "*chi)) +
  scale_fill_viridis_c(name = b_legend) +
  plot_theme

# 4) cost vs L

alpha_new <- seq(10, 1000, by = 10)

# Call the model on each combination of parameters
res_4 <- expand_grid(L, alpha_new) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, alpha = alpha_new),
                            .f = wrapper,
                            r = r,
                            K = K,
                            X0 = X0,
                            f = f,
                            p = p,
                            q = q,
                            chi = chi,
                            beta = beta,
                            c = c,
                            mu = mu,
                            w = w,
                            years = years,
                            want = "X_vec"))


res_4_plot <- ggplot(res_4, aes(x = L, y = alpha_new, fill = equil_b, z = equil_b)) +
  geom_raster(interpolate = T) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 1, by = 0.1), name = l_legend) +
  scale_y_continuous(expand = c(0,0), name = expression("Variable cost of enforcement, "*alpha)) +
  scale_fill_viridis_c(name = b_legend) +
  plot_theme


# Combine

legend <- cowplot::get_legend(
  # create some space to the left of the legend
  res_1_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
)

title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    paste0("Defaults: mu = ", mu, ", psi = ", w, ", chi = ", chi, ", alpha = ", alpha),
    fontface = "bold",
    size = text_size,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

biomass_heat_plots <- 
  cowplot::plot_grid(
    # title
    title,
    cowplot::plot_grid(
      # plots
      cowplot::plot_grid(
        res_1_plot + theme(legend.position="none"), 
                                        res_2_plot + theme(legend.position="none"), 
                                        res_3_plot + theme(legend.position="none"), 
                                        res_4_plot + theme(legend.position="none"),
                                        nrow = 2, ncol = 2,
                                        labels = "AUTO",
                                        label_size = text_size
        ),
      # legend
      legend,
      rel_widths = c(2,0.3)
    ),
    ncol = 1,
    rel_heights = c(0.1,2)
  )
  
biomass_heat_plots

# Effort heat maps here
w <- 180e2
chis <- seq(0, 30000, by = 500)
Ls <- seq(0.1, 1, by = 0.05)
alphas <- seq(alpha / 10, alpha * 10, length.out = 50)


E_chi_L <- expand_grid(L = Ls, chi = chis) %>% 
  mutate(equil_E_f = pmap_dbl(.l = list(L = L, chi = chi),
                              .f = wrapper,
                              r = r,
                              K = K,
                              X0 = X0,
                              f = f,
                              p = p,
                              q = q,
                              c = c,
                              beta = beta,
                              alpha = alpha,
                              mu = mu,
                              w = w,
                              years = years,
                              want = "E_f_vec"),
         equil_E_l = pmap_dbl(.l = list(L = L, chi = chi),
                              .f = wrapper,
                              r = r,
                              K = K,
                              X0 = X0,
                              f = f,
                              p = p,
                              q = q,
                              c = c,
                              beta = beta,
                              alpha = alpha,
                              mu = mu,
                              w = w,
                              years = years,
                              want = "E_l_vec"),
         equil_E_i = pmap_dbl(.l = list(L = L, chi = chi),
                              .f = wrapper,
                              r = r,
                              K = K,
                              X0 = X0,
                              f = f,
                              p = p,
                              q = q,
                              c = c,
                              beta = beta,
                              alpha = alpha,
                              mu = mu,
                              w = w,
                              years = years,
                              want = "E_i_vec")) %>% 
  gather(patch, effort, -c(L, chi)) %>% 
  group_by(patch) %>% 
  mutate(max_e = max(effort)) %>% 
  ungroup() %>% 
  mutate(effort_norm = effort / max_e,
         patch = case_when(patch == "equil_E_f" ~ "Fishing zone",
                           patch == "equil_E_l" ~ "Lease zone",
                           T ~ "No-take zone")
  )

ggplot(data = E_chi_L,
       mapping = aes(x = L, y = chi, fill = effort_norm, z = effort_norm)) +
  geom_raster(interpolate = T) +
  facet_wrap(~patch, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "Normalized\neffort",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  startR::ggtheme_plot() +
  labs(x = "Proportion of reserve\nas lease zone (L)", y = quote("Lease price ("~chi~")")) +
  ggtitle(label = "Equilibrium effort in each zone")



# E_chi_L %>% 
#   filter(L %in% c(0.1, 0.5, 0.9)) %>% 
#   mutate(dist = case_when(patch == "Fishing zone" ~ 1.5 * f,
#                            patch == "Lease zone" ~ f * (1 - L) + (f * L / 2),
#                            T ~ 0.5 * f * (1 - L)),
#          L = paste("L = ", L)) %>% 
#   ggplot(mapping = aes(x = dist, y = effort, color = chi, group = chi)) +
#   geom_line() +
#   facet_wrap(~L, ncol = 1) + 
#   theme_minimal()

# alpha, L, and efforts

E_alpha_L <- expand_grid(L = Ls, alpha = alphas) %>% 
  mutate(equil_E_f = pmap_dbl(.l = list(L = L, alpha = alpha),
                              .f = wrapper,
                              r = r,
                              K = K,
                              X0 = X0,
                              f = f,
                              p = p,
                              q = q,
                              c = c,
                              beta = beta,
                              chi = chi,
                              mu = mu,
                              w = w,
                              years = years,
                              want = "E_f_vec"),
         equil_E_l = pmap_dbl(.l = list(L = L, alpha = alpha),
                              .f = wrapper,
                              r = r,
                              K = K,
                              X0 = X0,
                              f = f,
                              p = p,
                              q = q,
                              c = c,
                              beta = beta,
                              chi = chi,
                              mu = mu,
                              w = w,
                              years = years,
                              want = "E_l_vec"),
         equil_E_i = pmap_dbl(.l = list(L = L, alpha = alpha),
                              .f = wrapper,
                              r = r,
                              K = K,
                              X0 = X0,
                              f = f,
                              p = p,
                              q = q,
                              c = c,
                              beta = beta,
                              chi = chi,
                              mu = mu,
                              w = w,
                              years = years,
                              want = "E_i_vec")) %>% 
  gather(patch, effort, -c(L, alpha)) %>% 
  group_by(patch) %>% 
  mutate(max_e = max(effort)) %>% 
  ungroup() %>% 
  mutate(effort_norm = effort / max_e,
         patch = case_when(patch == "equil_E_f" ~ "Fishing zone",
                           patch == "equil_E_l" ~ "Lease zone",
                           T ~ "No-take zone")
  )

ggplot(data = E_alpha_L,
       mapping = aes(x = L, y = alpha, fill = effort_norm, z = effort_norm)) +
  geom_raster(interpolate = T) +
  facet_wrap(~patch, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "Normalized\neffort",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  startR::ggtheme_plot() +
  labs(x = "Proportion of reserve\nas lease zone (L)", y = quote("Cost of enforcement("~alpha~")")) +
  ggtitle(label = "Equilibrium effort in each zone")

# SCATTER PLOTS (lines, or whatever)












