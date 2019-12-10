#### PARAMETERS ########################################################
#
# Parameters with an asterisk (*) on the description means they are fixed
# and we may have a source.
#
########################################################################

## Bio
r <- 0.57                 # *Growth rate
K <- 6.876526e6           # *Carrying capacity (tons)
X0 <- 3.507028e6          # *Initial biomass (tons)
D <- matrix(c(0.7, 0.3,
              0.3, 0.7),
            nrow = 2,
            byrow = T)    # *Dispersal between patches

## Econ
p <- 1100                 # *Price (USD / ton)
q <- 2e-5                 # *Catchability (made up)
c <- 1800                 # *Cost coefficient (USD / unit of effort)
beta <- 1.3               # *Cost exponent (From upsides?)

## Management
L <- 0.7                  # Size of the lease zone
alpha <- 9000             # Marginal cost of enforcement
mu <- 1e-3                # Enforcement coefficient
w <- 5 * c                # Per-unit-effort fine
chi <- 10000              # Per-unit-access fee

## Duration of simulation
years <- 30L              # *Duration of simulaionS
