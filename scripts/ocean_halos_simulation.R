##########################################################
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
# The parameterization of some parameters comes from
# the 2015 Skipjack stock assessment.
##########################################################

# Load packages
library(tidyverse)

#### The model #################################################################
# 
# The model is coded as a function of all parameters,
# with no prescribed default values. A collection of
# default parameters is included below.
# 
################################################################################

model <- function(r, K, X0, f, p, q, c, beta, L, alpha, theta_max, mu, w, chi) {
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
    # In the above, enforcement budget is this years access fees plus last years fines for illegal fishing
    theta <- theta_max * (1 - exp(-mu * E_e))                                                 # Probability of detection given enforcement
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
    
    # Update biomass for next iteration
    X_now <- X_next
  }
  #### END FOR LOOP ####
  
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
  
  return(results)
}

#### PARAMETERS ########################################################
# 
# The following are default parameters that get passed to
# the model (wrapped as a function).
# 
########################################################################

## Bio
r <- 0.57                 # Growth rate
K <- 6.876526e6           # Carrying capacity (tons)
X0 <- 3.507028e6          # Initial biomass (tons)
f <- 0.5                  # Dispersal between patches

## Econ
p <- 1200                 # Price (USD / ton)
q <- 1.5e-5               # Catchability (made up)
c <- 1800                 # Cost coefficient (USD / unit of effort)
beta <- 1.3               # Cost exponent (From upsides?)

## Management
L <- 0.7                  # Size of the lease zone
alpha <- 90               # Marginal cost of enforcement
theta_max <- 1L           # Maximum attainable probability of detection
mu <- 1e-3                # Enforcement coefficient
w <- 1800                 # Per-unit-effort fine
chi <- 1000               # Per-unit-access fee

## Duration of simulation
years <- 20L

#### Running simulations ################################################
#
#
#########################################################################

# Does it run?
test <- model(r, K, X0, f, p, q, c, beta, L, alpha, theta_max, mu, w, chi)

# Does it make sense?
# (plot all state variables through time)
test %>% 
  gather(variable, value, -time) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

# Wrapper function to call with map and get equilibrium biomass (normalized to K)
get_equil_biomass <- function(L, mu, w){
  model(r, K, X0, f, p, q, c, beta, L = L, alpha, theta_max, mu = mu, w = w, chi) %>% 
    filter(time == max(time)) %>% 
    pull(X_vec) / K
}

# Some range of parameters to test for
Ls <- seq(0.1, 1, by = 0.05)
mus <- seq(mu, mu * 10, length.out = 10)
ws <- seq(c, 20 * c, length.out = 10)

# Call the model on each combination of parameters
res <- expand_grid(Ls, mus, ws) %>% 
  mutate(equil_b = pmap_dbl(.l = list(L = Ls, mu = mus, w = ws),
                            .f = get_equil_biomass))


# Plot it
ggplot(res, aes(x = Ls, y = equil_b, color = ws, group = ws)) +
  geom_line() +
  facet_wrap(~mus, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_viridis_c() +
  theme_bw() +
  ggtitle(label = "Equilibrium biomass", subtitle = "for a combination of fines (ws, color) and enforcement coefficients (mu, facets)")












