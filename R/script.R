# Run source() on each of the follwing files for your machine in order to 
# load the neccessary functons to run the simulation

source('C:/Users/a421356/R-GitHub/SCM_Sim/R/invRef.R')
source('C:/Users/a421356/R-GitHub/SCM_Sim/R/transRef.R')
source('C:/Users/a421356/R-GitHub/SCM_Sim/R/facRef.R')
source('C:/Users/a421356/R-GitHub/SCM_Sim/R/hubRef.R')
source('C:/Users/a421356/R-GitHub/SCM_Sim/R/simulation.R')

setwd("C:/Users/a421356/R-GitHub/SCM_Sim/data")
simData <- read.csv("BUD1207.csv", colClasses = c(rep("character", 3), rep("numeric", 10)))

# sets directory to Output so we can keep our output seperate from the program files
setwd("C:/Users/a421356/R-GitHub/SCM_Sim/Output")

sim.name <- "test"
dOpen <- 5
dOrder <- 1
nSim <- 1000

BMT <- as.matrix(simData[(simData[ ,1] == "Beaumont") & (simData[ ,3] != "Lux Hub"), ])
LUX <- simData[(simData[ ,3] == "Lux Hub"), ]
LUX <- as.matrix(rbind(LUX, simData[(simData[ ,1] == "Lux Hub"), ]))

BMT.inv <- vector("list", length = nrow(BMT))
BMT.trns <- vector("list", length = nrow(BMT))

for (row in 1:nrow(BMT)) {
  dmd <- BMT[row, 10:11]
  modes <- BMT[row, 4:9]
  
  BMT.inv[[row]] <- gen.inv(nSim, BMT[row,3], BMT[row, 12], c(dmd[1], dmd[2]), c(dmd[1], dmd[2]), dOpen, dOrder)
  BMT.trns[[row]] <- gen.trans(nSim, modes)
}

# nSim, nm, curr, expect, act, opNdays, ordNdays, modes, info = NA
hub <- gen.hub(nSim, LUX[,3], LUX[,12], LUX[,10:11], LUX[,10:11], rep(6, nrow(LUX)), rep(1, nrow(LUX)))



