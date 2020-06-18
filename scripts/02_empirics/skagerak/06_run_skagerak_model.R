# Load packages
library(here)
library(cowplot)
library(tidyverse)
library(scales)

# Source functions
source(here("scripts", "00_helpers.R"))
source(here("scripts", "01_simulations", "01_model.R"))
source(here("scripts", "01_simulations", "02_wrapper.R"))
source(here("scripts", "02_empirics", "05_cod_default_parameters.R"))

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

(best_results$E_i_vec / best_results$E_f_vec)[1]

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

max_conservation_benefit <- best_results %>%
  dplyr::filter(L == 0 | X_vec == max(X_vec)) %>%
  dplyr::select(L, X = X_vec)
