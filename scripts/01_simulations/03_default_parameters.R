#### PARAMETERS ########################################################
#
# Parameters with an asterisk (*) on the description means they are fixed
# and we may have a source.
#
########################################################################

## Bio
r <- 0.57                 # *Growth rate
K <- 6.876526e6           # *Carrying capacity (tons)
X0 <- 0.8 * K             # *Initial biomass (tons)
s <- 0.5                  # Initial distribution of biomass to the reserve
D <- matrix(c(0.7, 0.3,
              0.3, 0.7),
            nrow = 2,
            byrow = T)    # *Dispersal between patches

## Econ
p <- 1100                 # *Price (USD / ton)
q <- 2e-5                 # *Catchability (approximated) But see (http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.491.7163&rep=rep1&type=pdf)
c <- 3000                 # *Cost coefficient (USD / unit of effort)
beta <- 1.3               # *Cost exponent (From upsides Costello et al., 2016)

## Management
L <- 0.1                  # Size of the lease zone
alpha <- 5000             # Marginal cost of enforcement
mu <- 5e-4                # Enforcement coefficient
w <- 365 * c              # Per-unit-effort fine
chi <- 0.5 * c            # Per-unit-access fee, for when we don't optimize it
b <- 0                    # Exogenous enforcement budget

## Duration of simulation
years <- 50L              # *Duration of simulaionS


###########################################################################

## Parameter ranges (absolute values)
# Ls
L_range <- seq(0, 1, by = 0.01)
self_rec_range <- c(0.1, 0.3, 0.5, 0.7, 0.9)

## Parameter Ranges (multipliers of the default value
w_range_multipliers <- c(0.1, 0.5, 1, 2, 10)
alpha_range_multipliers <- c(0.1, 0.5, 1, 2, 10)
c_range_multipliers <- c(0.8, 0.9, 1, 1.1, 1.2)
chi_range_multipliers <- c(0.1, 0.5, 1, 2, 10)


##  Extended parameter Ranges (multipliers of the default value
w_range_multipliers_extend <- c(0.1, 0.5, 1, 1.5, 2, 2.5, 5, 7.5, 10)
alpha_range_multipliers_extend <- c(0.1, 0.5, 1, 1.5, 2, 2.5, 5, 7.5, 10)
c_range_multipliers_extend <- c(0.8, 0.85, 0.9, 0.95, 1.05, 1.15)
chi_range_multipliers_extend <- c(0.1, 0.5, 1, 2, 3, 5, 7, 10, 13, 16, 20)
self_rec_range_extend <- seq(0, 1, by = 0.05)
p_range_multipliers <- c(0.8, 0.85, 0.9, 0.95, 1.05, 1.15)

##############################################################################

# Define some default legends
L_legend <- "Proportion as lease area (L)"
X_legend <- "Equilibrium biomass (X / K)"
X_legend_short <- "X / K"
b_legend_days <- "External enforcement budget (# of enforcement days)"
b_legend_money <- "External enforcement budget (b)"