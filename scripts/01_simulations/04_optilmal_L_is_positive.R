

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
  
  opt_results <- optim(par = 500,                    # Guess that we know works
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

tibble(L = Ls, chi = opt_par, X = opt_val, X_rel = rel_b) %>% 
  ggplot(aes(x = L, y = X / K, fill = chi, size = rel_b)) +
  geom_point(color = "black", shape = 21) +
  scale_fill_viridis_c() +
  plot_theme()
