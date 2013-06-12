# Run source() on each of the follwing files for your machine in order to 
# load the neccessary functons to run the simulation

BASE.DIR <- getwd()

source(file.path(BASE.DIR, 'R', "invRef.R"))
source(file.path(BASE.DIR, 'R', "transRef.R"))
source(file.path(BASE.DIR, 'R', "hubRef.R"))
source(file.path(BASE.DIR, 'R', "simulation.R"))
source(file.path(BASE.DIR, 'R', "Analysis.R"))

DATA <- read.csv(file.path(BASE.DIR, 'data', "RF-BUD1207.csv"), 
                 colClasses = c(rep("character", 3), rep("numeric", 13)))


dOpen <- 6
nSim <- 1500
disrupt <- c(1,0)
bias <- c(0.5, 0.5)
amp <- 0.25
qnt <- 0.95
seed <- NA
STRAT <- "ROP"

#### STRAT can be PW2RWS, ROP or USED
# PW2RWS is our strategy
# ROP is reorder point strategy
# USED is order what we used

simulation(DATA, nSim, dOpen, disrupt, qnt, bias, amp, seed, STRAT)
