################################################################
#                           MODEL                             #
################################################################
##### DESCRIPTION
# This script contains a discrete-time mete-population
# model of Ocean Halos. It is a two-patch model with
# a spatially implicit no-take marine reserve in one of
# the patches.
#
# The first patch is an MPA, which contains a no-take zone
# and a lease zone. The state variables for these areas are
# subindexed as _n (no-take) and _l (lease). Fishing effort
# in each area is determined by profits and costs. Costs
# in the no-take are cost of fishing + fines, while costs
# in the lease zone are cost of fishing + access fees.
#
# The second patch is a fishing area, where effort is given
# by open-access dynamics (or management...).
#
# 
##### INPUTS
# The model is coded as a function of all parameters,
# with no prescribed default values. The parameter names are
# the following:
# 
# r - intrinsic growth rate
# K - total carrying capacity (tones)
# X0 - total initial population size (tones)
# s - reserve size as a proportion of the entire system - scales K and X0 
# D - dispersal matrix 
# p - price of fish (USD / ton)
# q - catchability
# c - cost of fishing (USD / effort)
# beta - cost scaling (usually 1.3)
# L - fraction of the reserve as a lease zone
# alpha - cost of enforcement (USD / effort)
# mu - enforcement efficiency
# w - fine paid if caught illegally fishing (USD / effort)
# chi - vessel-day price paid to fish in L (USD / effort)
# years - number of years to run simulation
# tolerance - difference between biomass in the final timestep previous to that before that must be satisfied for equilibrium
# b - annual external enforcement budget (USD)
# 
################################################################

model <- function(chi, r, K, X0, s, D, p, q, c, beta, L, alpha, mu, w, years, tolerance = 0.05, b = 0) {
  
  # Define vectors to store state variables through time
  X_vec <-
    X_r_vec <-
    X_f_vec <-
    E_in_vec <-
    E_il_vec <-
    E_i_vec <-
    E_l_vec <- 
    E_f_vec <- 
    E_e_vec <- 
    H_i_vec <- 
    H_l_vec <- 
    H_f_vec <- numeric(length = years)
  
  
  X_now_r <- X0*(s)                           # Initial biomass in reserve
  X_now_f <- X0*(1-s)                           # Initial biomass in fishing area
  time <- seq_len(years)                    # Create vector of time
  
  K_r <- K*(s)
  K_f <- K*(1-s)
  
  #### Initial budget depends on access fee revenues only
  E_i <- 0
  theta <- 0
  
  #### BEGIN FOR LOOP ####
  for (i in time) {
    
    # Biomass in each patch
    X_r <- X_now_r
    X_f <- X_now_f
    X_tot <- X_r + X_f
    
    # Fleet #1 - Selection of open-access effort in fishing zone and harvest
    E_f <- max(((p * q * X_f) / (beta * c)) ^ (1 / (beta - 1)), 0, na.rm = T)                                     # Fishing effort in fishing zone
    H_f <- q * X_f * E_f                                                                                          # Harvest in fishing zone
    
    # Fleet #2 - Selection of legal effort in lease-zone and harvest
    E_l <- max((((p * q * X_r * L) - chi) / (beta * c)) ^ (1 / (beta - 1)), 0, na.rm = T)
    H_l <- q * X_r * L * E_l  
    
    # Enforcement - Selection of enforcement effort given budget
    E_e <- (b + (E_l * chi)) / alpha                                                                              # Enforcement effort given budget
    theta <- 1 - exp(-mu * E_e)                                                                                   # Probability of detection given enforcement
    
    # Fleet #3 - Selection of illegal effort in reserve and harvest
    E_i <- max((((p * q * X_r * (1 - (q * E_l))) - (theta * w)) / (beta * c)) ^ (1 / (beta - 1)), 0, na.rm = T)   # Illegal effort (M)
    H_i <- q * X_r * (1 - (q * E_l)) * E_i                                                                        # Illegal harvest (M)
    
    # Total harvest in the reserve
    H_r <- H_l + H_i
    
    # Growth in each area
    X_growth_f <- X_f + (X_f * r * (1 - (X_f / K_f)))  - H_f                                                    # Gordon-Schafer growth (F)
    X_growth_r <- X_r + (X_r * r * (1 - (X_r / K_r)))  - H_r                                                    # Gordon-Schafer growth (M)
    
    # Dispersal
    X_next_f <- (X_growth_f * D[1,1]) + (X_growth_r * D[1,2])                                                     # Dispersal in F         
    X_next_r <- (X_growth_r * D[2,2]) + (X_growth_f * D[2,1])                                                     # Dispersal in R                
    
    # Track state variables
    X_vec[i] <- X_tot
    X_f_vec[i] <- X_f
    X_r_vec[i] <- X_r
    E_f_vec[i] <- E_f 
    E_l_vec[i] <- E_l 
    E_i_vec[i] <- E_i
    E_e_vec[i] <- E_e 
    H_f_vec[i] <- H_f
    H_l_vec[i] <- H_l
    H_i_vec[i] <- H_i
    
    X_now_r <- X_next_r                                                                                           # Update biomass for next iteration
    X_now_f <- X_next_f
  }
  #### END FOR LOOP ####
  
  data.frame(X_vec, H_l_vec, H_i_vec)

  # Check that equilibrium was reached
  if(!near(X_vec[i-1],
           X_vec[i],
           tol = tolerance * X_vec[i])) {
    message <- paste("Equilibrium was not reached!",
                     "\n==========================\n",
                     "These were the parameters:\n",
                     "L     =", L, "\n",
                     "alpha =", alpha, "\n",
                     "mu    =", mu, "\n",
                     "w     = ", w, "\n",
                     "chi   = ", chi, "\n",
                     "Failing with last two values of biomass of:\n",
                     "X_t-1 = ", X_vec[i - 1], "\n",
                     "X_t = ", X_vec[i], "\n"
                     )
    warning(message)
    }
  
  # Combine all results into a tibble
  results <-
    tibble::tibble(
      time,
      X_vec,
      X_r_vec,
      X_f_vec,
      E_i_vec,
      E_l_vec,
      E_f_vec,
      E_e_vec,
      H_i_vec,
      H_l_vec,
      H_f_vec
    )
  
  return(results)                 # Return the results as a table                                                                             

}
