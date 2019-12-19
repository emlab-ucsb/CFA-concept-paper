################################################################
#                         WRAPPER                              #
################################################################
##### Description
# This function calls the model over parameters specified and
# extracts the value of a given varialbe (want) for the last
# timestep. It defaults to extracting equilibrium biomass.# 
# 
################################################################

wrapper <- function(chi, r, K, X0, D, p, q, c, beta, L, alpha, mu, w, years, want = "X_vec"){
  
  if(!want == "All"){
    value <- model(chi = chi,
                   r = r,
                   K = K,
                   X0 = X0,
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
                   tolerance = 0.05) %>%     # run the model
      filter(time == max(time)) %>%                                          # keep the last timestep only
      pull({{want}})                                                         # return desired variable 
  } else {
    value <- model(chi = chi,
                   r = r,
                   K = K,
                   X0 = X0,
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
                   tolerance = 0.05) %>%     # run the model
      filter(time == max(time))                                              # keep the last timestep only
  }
  
  return(value)                                                            # Return the value
}

# Optimization wraper 2.0
get_chi <- function(chi, pars){
  
  # Extract non-optimization parameters
  r <- pars$r
  K <- pars$K
  X0 <- pars$X0
  D <- pars$D
  p <- pars$p
  q <- pars$q
  c <- pars$c
  beta <- pars$beta
  L <- pars$L
  alpha <- pars$alpha
  mu <- pars$mu
  w <- pars$w
  years <- pars$years
  
  # Run the scenario with a given chi
  biomass <- wrapper(chi = chi,
                     r = r,
                     K = K,
                     X0 = X0,
                     D = D,
                     p = p,
                     q = q,
                     c = c,
                     beta = beta,
                     L = L,
                     alpha = alpha,
                     mu = mu,
                     w = w,
                     years = years)
  
  return(biomass)
}

################################################################
#                 BIOMASS OPTIMIZATION WRAPPER                 #
################################################################
##### Description
# This function finds the value of chi that   
# maximizes total system biomass for a given set of parameters
################################################################

biomass_optim_wrapper <- function(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, years, chi_min = 0, chi_max = 100000, want = "X_vec"){
  
  # Sequence of chi values to hunt over
  chi_seq <- seq(chi_min, chi_max, length.out = 50)
  
  # Run model for all values of chi - this is a rough cut to give optim a better starting point
  rough_value <- expand_grid(chi_seq) %>% 
    mutate(equil_b = pmap_dbl(.l = list(chi = chi_seq),
                              .f = wrapper,
                              r = r,
                              K = K,
                              X0 = X0,
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
                              want = "X_vec"))
  
  # If there is no difference in biomass
  if(min(rough_value$equil_b) == max(rough_value$equil_b)){
    
    out <- wrapper(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, chi = 0, years, want)
   
  # Otherwise extract chi cooresponding to max value of biomass 
  }else{
    
    chi_start <- rough_value$chi_seq[rough_value$equil_b == max(rough_value$equil_b)]
    chi_min <- chi_seq[which(chi_seq == chi_start) - 1]
    chi_max <- chi_seq[which(chi_seq == chi_start) + 1]
    
    best_value <- nlminb(start = 30000, model, r = r, K = K, X0 = X0, D = D, p = p, q = q, c = c, beta = beta, L = L, alpha = alpha, mu = mu, w = w, years = years, purpose = "optim", control = list(step.min = 10, step.max = 100))
    
    # out
    out <- wrapper(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, chi = best_value$par, years, want)
    
  }
  
  return(out)
                                                      
}
