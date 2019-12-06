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
# K - carrying capacity (tones)
# X0 - initial population size (tones)
# f - fraction of total biomass contained in the reserve
# p - price of fish (USD / ton)
# c - cost of fishing (USD / effort)
# beta - cost scaling (usuallu 1.3)
# L - fraction of the reserve as a lease zone
# alpha - cost of enforcement (USD / effort)
# mu - enforcement efficiency
# w - fine paid if caught illegally fishing (USD / effort)
# chi - vessel-day price paid to fish in L (USD / effort)
# 
################################################################

model <- function(r, K, X0, f, p, q, c, beta, L, alpha, mu, w, chi, years) {
  # Define vectors to store state variables through time
  X_vec <-
    X_r_vec <-
    X_f_vec <-
    E_i_vec <-
    E_l_vec <- 
    E_f_vec <- 
    E_e_vec <- 
    H_i_vec <- 
    H_l_vec <- 
    H_f_vec <- numeric(length = years)
  
  
  X_now <- X0                           # Initial biomass
  time <- seq_len(years)                # Create vector of time
  
  #### Wild assumption 1: Initial budget depends on access fee revenues only
  E_i <- 0
  theta <- 0
  
  #### BEGIN FOR LOOP ####
  for (i in time) {
    
    # Biomass dispersal
    X <- X_now
    X_r <- f * X
    X_f <- (1 - f) * X
    
    # Selection of effort
    E_f <- max(((p * q * X_f) / (beta * c)) ^ (1 / (beta - 1)), 0, na.rm = T)                            # Fishing effort in fishing zone
    E_l <- max((((p * q * X_r * L) - chi) / (beta * c)) ^ (1 / (beta - 1)), 0, na.rm = T)                # Fishing effort inlease zone
    E_e <- ((E_l * chi) + (theta * w * E_i)) / alpha                                                     # Enforcement effort given budget
    theta <- 1 - exp(-mu * E_e)                                                                          # Probability of detection given enforcement
    E_i <- max((((p * q * X_r * (1 - L)) - (theta * w)) / (beta * c)) ^ (1 / (beta - 1)), 0, na.rm = T)  # Illegal fishing effort
    
    # Harvest
    H_f <- q * X_f * E_f                                                                                 # Harvest in fishing zone
    H_l <- q * X_r * L * E_l                                                                             # Harvest in lease zone
    H_i <- q * X_r * (1 - L) * E_i                                                                       # Illegal harvest
    H <- H_f + H_l + H_i                                                                                 # Total harvest
    
    # Growth
    X_next <- X + (X * r * (1 - (X / K))) - H                                                            # Gordon-Schafer
    
    # Track state variables
    X_vec[i] <- X_now
    X_r_vec[i] <- X_r
    X_f_vec[i] <- X_f
    E_i_vec[i] <- E_i
    E_l_vec[i] <- E_l 
    E_f_vec[i] <- E_f 
    E_e_vec[i] <- E_e 
    H_i_vec[i] <- H_i 
    H_l_vec[i] <- H_l 
    H_f_vec[i] <- H_f
    
    
    X_now <- X_next                                                                                      # Update biomass for next iteration
  }
  #### END FOR LOOP ####
  
  
  # Check that equilibrium was reached
  if(!near(
    X_vec[i-1],
    X_vec[i],
    tol = 0.05 * X_vec[i])) {
    stop("Error: Equilibrium was not reached!")
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
  
  return(results)                                                                                             # Return the results as a table
}
