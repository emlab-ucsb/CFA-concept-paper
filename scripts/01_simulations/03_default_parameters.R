#### PARAMETERS ########################################################
#
# Parameters with an asterisk (*) on the description means they are fixed
# and we may have a source.
#
########################################################################

## Bio
r <- 0.57                 # *Growth rate
K <- 6.876526e6           # *Carrying capacity (tons)
X0 <- 0.3 * K             # *Initial biomass (tons)
D <- matrix(c(0.7, 0.3,
              0.3, 0.7),
            nrow = 2,
            byrow = T)    # *Dispersal between patches

## Econ
p <- 1100                 # *Price (USD / ton)
q <- 2e-5                 # *Catchability (made up)
c <- 3000                 # *Cost coefficient (USD / unit of effort)
beta <- 1.3               # *Cost exponent (From upsides?)

## Management
L <- 0.7                  # Size of the lease zone
alpha <- 9000             # Marginal cost of enforcement
mu <- 5e-4                # Enforcement coefficient
w <- 1.5 * c              # Per-unit-effort fine
chi <- 200                # Per-unit-access fee
b <- 0                    # Exogenous enforcement budget

## Duration of simulation
years <- 50L              # *Duration of simulaionS
