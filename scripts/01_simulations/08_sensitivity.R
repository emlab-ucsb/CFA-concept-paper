# Load packages
library(here)
library(cowplot)
library(furrr)
library(tidyverse)
library(scales)

# Source functions
source(here("scripts", "00_helpers.R"))
source(here("scripts", "01_simulations", "01_model.R"))
source(here("scripts", "01_simulations", "02_wrapper.R"))
source(here("scripts", "01_simulations", "03_default_parameters.R"))


plan(multiprocess)


all_combinations <- expand_grid(L_try = seq(0, 1, by = 0.05),
                                w_try = w * w_range_multipliers,
                                alpha_try = alpha * alpha_range_multipliers,
                                c_try = c * c_range_multipliers,
                                chi_try = chi * chi_range_multipliers,
                                self_rec = self_rec_range,
                                p_try = p * p_range_multipliers) %>%
  mutate(D_try = map(self_rec, make_D)) %>% 
  mutate(results = future_pmap(.l = list(chi = chi_try,
                                         L = L_try,
                                         w = w_try,
                                         alpha = alpha_try,
                                         c = c_try,
                                         D = D_try,
                                         p = p_try),
                               .f = wrapper,
                               r = r,
                               K = K,
                               X0 = X0,
                               s = s,
                               q = q,
                               beta = beta,
                               mu = mu,
                               years = years,
                               want = "All")) %>% 
  unnest(cols = results)

plan(sequential)


density_plot <- all_combinations %>% 
  group_by(w_try, alpha_try, c_try, self_rec) %>% 
  mutate(max_X = max(X_vec, na.rm = T)) %>% 
  ungroup() %>% 
  filter(X_vec == max_X) %>% 
  ggplot(aes(x = L_try)) +
  geom_density(fill = "steelblue") +
  plot_theme() +
  scale_x_continuous(limits = c(0, 1))

lazy_ggsave(p1, filename = "FigureS3", width = 9, height = 6)





