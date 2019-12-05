##### Add file header!!!

# Load packages
library(here)
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

# Some range of parameters to test for
L <- seq(0.1, 1, length.out = 10)
w <- seq(c, 20 * c, length.out = 10)

# Call the model on each combination of parameters
res <- expand_grid(L, w) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = L, w = w),
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


# Plot it
ggplot(res, aes(x = L, y = w, fill = equil_b, z = equil_b)) +
  geom_raster(interpolate = T) +
  geom_contour(color = "black") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_viridis_c() +
  theme_bw() +
  ggtitle(label = "Equilibrium biomass",
          subtitle = "for a combination of fines (ws, color) and enforcement coefficients (mu, facets)")


#### Plots we want ########

# HEAT MAPS

# Biomass heat maps here









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












