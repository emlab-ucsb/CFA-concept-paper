#### COD PARAMETERS ########################################################
#
# Parameters with an asterisk (*) on the description means they are fixed
# and we may have a source.
#
########################################################################
#
########################################################################
# STOCK ASSESSMENT INFORMATION
# 
# Cod (Gadus morhua) in Subdivision 21 (Kattegat)
# Species: Gadus morhua
# Stock code: cod.27.21
# Assessment Year:2019
# Purpose:Advice
# Assessment Key: 13064
# Stock Key: 169086
# 
########################################################################

## Bio
r <- 0.52                 # *Growth rate http://fishbase.org/summary/Gadus-morhua.html
K <- 2.7e6                # *Carrying capacity (tons) <<<<<<-------------- FIX THIS< IT IS MADE UP FOR NOW !!!!!!!!!!!!!!!
X0 <- 0.1*K	            # *Initial biomass (tons) SSB from SA: http://standardgraphs.ices.dk/ViewSourceData.aspx?key=9841
s <- readRDS(here::here("data", "skagerak_s.rds"))                 # The reserve covers 40% of the system skagerak systems
D <- readRDS(here::here("data", "skagerak_d.rds"))

## Econ
p <- 7000                 # *Price (USD / ton) About 7 usd per Kg based on: https://issuu.com/globefish/docs/epr__may_2020
q <- 24 * exp(-16.95)     # *Catchability (From EIDE et al., 2003, MRE)
c <- 400                  # *Cost coefficient (USD / unit of effort)
beta <- 1.206               # *Cost exponent (From EIDE et al., 2003, MRE)

## Management
alpha <- 4000             # Marginal cost of enforcement
mu <- 5e-4                # Enforcement coefficient
w <- 365 * c              # Per-unit-effort fine
chi <- 0.5 * c            # Per-unit-access fee, for when we don't optimize it
b <- 0                    # Exogenous enforcement budget

## Duration of simulation
years <- 50L              # *Duration of simulaions


###########################################################################

## Parameter ranges (absolute values)
# Ls
L_range <- seq(0, 1, by = 0.01)

# Define some default legends
L_legend <- "Proportion as lease area (L)"
X_legend <- "Equilibrium biomass (X / K)"
X_legend_short <- "X / K"
b_legend_days <- "External enforcement budget (# of enforcement days)"
b_legend_money <- "External enforcement budget (b)"