

Ls <- seq(0.05, 1, length.out = 20)
opt_par <- opt_val <- rel_b <- numeric(length(Ls))

for(i in 1:length(Ls)){
  
  pars <- list(r = r,
               K = K,
               X0 = X0,
               D = D,
               p = p,
               q = q,
               c = c,
               beta = beta,
               L = Ls[i],
               alpha = alpha,
               mu = mu,
               w = w,
               years = years)
  
  opt_results <- optim(par = 0,                     # Guess that we know works
                       fn = get_chi,                 # Function to call
                       pars = pars,                  # All other parametrs passed as a list, and extracted in wraper
                       control = list(fnscale = -1), # Indicate that we are maximizing
                       method = "L-BFGS-B",          # Use a method that allows for min and max
                       lower = 0,                    # Minimum charge is 0
                       upper = 1e6)                  # Maximum fee is 1 million
  
  opt_par[i] <- opt_results$par
  opt_val[i] <- opt_results$value
  rel_b[i] <- wrapper(chi = opt_results$par,
                      r = r,
                      K = K,
                      X0 = X0,
                      D = D,
                      p = p,
                      q = q,
                      c = c,
                      beta = beta,
                      L = Ls[i],
                      alpha = alpha,
                      mu = mu,
                      w = w,
                      years = years, want = "All") %>% 
    mutate(X_rel = X_r_vec / X_f_vec) %>% 
    pull(X_rel)
  
}


best_results <- tibble(L = Ls, chi = opt_par, X = opt_val, X_rel = rel_b) %>% 
  mutate(index = as.numeric(row.names(.)))

optimal_fee_for_L_plot <- 
  ggplot(data = best_results,
         mapping = aes(x = L, y = X / K, fill = chi, size = X_rel)) +
  geom_point(color = "black", shape = 21) +
  scale_fill_viridis_c() +
  plot_theme()

optimal_fee_for_L_plot




# L_X_and_fines <- tibble(w = c(w / 10, w / 5, w / 2, w, w * 2, w * 5)) %>% 
#   mutate(results = map(w, try_par)) %>% 
#   unnest(cols = results)
# 
# # L_X_and_fines_plot
# ggplot(data = L_X_and_fines,
#        mapping = aes(x = L, y = X / K, fill = chi)) +
#   geom_point(aes(size = X_rel), color = "black", shape = 21) +
#   geom_line(aes(group = w, color = w)) +
#   scale_fill_viridis_c() +
#   plot_theme()




# Different fines
L_X_and_fines <- expand_grid(index = c(1:20),
                             w = c(0, 5000, 10000, 15000)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, w_try = w) %>% 
  # mutate(chi_try = 500) %>%                                     # Saving just in case we want to see a fixed-chi effect
  mutate(results = pmap(.l = list(L = L_try,
                                  chi = chi_try,
                                  w = w_try),
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
  mutate(X_rel = X_r_vec / X_f_vec,
         w_try = as.factor(w_try)) %>% 
  select(w_try, L_try, chi_try, X_vec, X_rel)

L_X_and_fines_plot <- 
  ggplot(data = L_X_and_fines,
         mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_line(aes(group = w_try, color = w_try)) +
  geom_point(aes(size = X_rel), color = "black", shape = 21) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE, size = FALSE)

L_X_and_fines_plot

# Different enforcement costs
L_X_and_enforcement_costs <- expand_grid(index = c(1:20), alpha = c(1000, 5000, 10000, 15000)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, alpha_try = alpha) %>% 
  mutate(results = pmap(.l = list(L = L_try, chi = chi_try, alpha = alpha_try),
                        .f = wrapper,
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
  select(alpha_try, L_try, chi_try, X_vec, X_rel)


L_X_and_enforcement_costs_plot <- 
  ggplot(data = L_X_and_enforcement_costs,
       mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_point(aes(size = X_rel), color = "black", shape = 21) +
  geom_line(aes(group = alpha_try, color = alpha_try)) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE, size = FALSE)

L_X_and_enforcement_costs_plot

# Different fishing costs
L_X_and_fishing_costs <- expand_grid(index = c(1:20), c = c(2500, 3000, 3500, 4000)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, c_try = c) %>% 
  mutate(results = pmap(.l = list(L = L_try, chi = chi_try, c = c_try),
                        .f = wrapper,
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
  select(c_try, L_try, chi_try, X_vec, X_rel)


L_X_and_fishing_costs_plot <- 
  ggplot(data = L_X_and_fishing_costs,
         mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_point(aes(size = X_rel), color = "black", shape = 21) +
  geom_line(aes(group = c_try, color = c_try)) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE, size = FALSE)

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
  expand_grid(index = c(1:20), self_rec = c(0.3, 0.5, 0.7, 0.9)) %>% 
  mutate(D = map(self_rec, make_D)) %>% 
  left_join(best_results, by = c("index")) %>% 
  rename(L_try = L, chi_try = chi, D_try = D) %>% 
  mutate(results = pmap(.l = list(L = L_try, chi = chi_try, D = D_try),
                        .f = wrapper,
                        r = r,
                        K = K,
                        X0 = X0,
                        p = p,
                        q = q,
                        c = 3000,
                        beta = beta,
                        alpha = alpha,
                        mu = mu,
                        w = w,
                        years = years,
                        want = "All")) %>% 
  unnest(cols = results) %>% 
  mutate(X_rel = X_r_vec / X_f_vec,
         self_rec = factor(self_rec, levels = sort(unique(self_rec)))) %>% 
  select(self_rec, L_try, chi_try, X_vec, X_rel)


L_X_and_dispersal_plot <- 
  ggplot(data = L_X_and_dispersal,
       mapping = aes(x = L_try, y = X_vec / K, fill = chi_try)) +
  geom_point(aes(size = X_rel), color = "black", shape = 21) +
  geom_line(aes(group = self_rec, color = self_rec)) +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  guides(fill = FALSE, size = FALSE)

L_X_and_dispersal_plot



figure2_subplots <- plot_grid(L_X_and_fines_plot,
                              L_X_and_enforcement_costs_plot,
                              L_X_and_fishing_costs_plot,
                              L_X_and_dispersal_plot,
                              ncol = 2,
                              labels = c("B", "C", "D", "E"))


figure2 <- plot_grid(optimal_fee_for_L_plot,
                     figure2_subplots,
                     ncol = 1)


lazy_ggsave(plot = figure2, filename = "figure_2", width = 10, height = 10)



















