# Run source() on each of the follwing files for your machine in order to 
# load the neccessary functons to run the simulation

BASE.DIR <- getwd()

source(file.path(BASE.DIR, 'R', "invRef.R"))
source(file.path(BASE.DIR, 'R', "transRef.R"))
source(file.path(BASE.DIR, 'R', "hubRef.R"))
source(file.path(BASE.DIR, 'R', "simulation.R"))

DATA <- read.csv(file.path(BASE.DIR, 'data', "RF-BUD1207.csv"), 
                 colClasses = c(rep("character", 3), rep("numeric", 13)))


dOpen <- 6
nSim <- 1500
disrupt <- as.matrix(rbind(c(0,1), c(750,21), c(750,14), c(750,7)))
bias <- 0
qnt <- seq(0.1, 0.9, 0.1)
seed <- NA


##### 36 total simulations #####
for (DISR in 1:nrow(disrupt)) {
  for (QUANT in 1:length(qnt)) {
    simulation(DATA, nSim, dOpen, disrupt[DISR, ], qnt[QUANT], bias, seed)
  }
}

