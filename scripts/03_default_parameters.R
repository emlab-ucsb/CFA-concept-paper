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
f <- 0.5                  # *Dispersal between patches

## Econ
p <- 1200                 # *Price (USD / ton)
q <- 1.5e-5               # *Catchability (made up)
c <- 1800                 # *Cost coefficient (USD / unit of effort)
beta <- 1.3               # *Cost exponent (From upsides?)

## Management
L <- 0.7                  # Size of the lease zone
alpha <- 90               # Marginal cost of enforcement
theta_max <- 1L           # Maximum attainable probability of detection
mu <- 1e-3                # Enforcement coefficient
w <- 1800                 # Per-unit-effort fine
chi <- 1000               # Per-unit-access fee

## Duration of simulation
years <- 30L              # *Duration of simulaionS
