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








# SCATTER PLOTS (lines, or whatever)












