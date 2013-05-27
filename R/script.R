# Run source() on each of the follwing files for your machine in order to 
# load the neccessary functons to run the simulation

source('C:/Users/a421356/R-GitHub/SCMsim/R/invRef.R')
source('C:/Users/a421356/R-GitHub/SCMsim/R/transRef.R')
source('C:/Users/a421356/R-GitHub/SCMsim/R/hubRef.R')
source('C:/Users/a421356/R-GitHub/SCMsim/R/simulation.R')

setwd('C:/Users/a421356/R-GitHub/SCMsim/data')
DATA <- read.csv("BUD1207.csv", colClasses = c(rep("character", 3), rep("numeric", 10)))

# sets directory to Output so we can keep our output seperate from the program files
setwd("C:/Users/a421356/R-GitHub/SCMsim/Output")

#### Name & Quantile ####
name <- "test"
q <- 0.95

#### Subsetting ####
BMT.val <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 4:13], rownames.force = FALSE)
BMT.nms <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 1:3], rownames.force = FALSE)
LUX <- rbind(DATA[(DATA[ ,3] == "Lux Hub"), ], DATA[(DATA[ ,1] == "Lux Hub"), ])
LUX.val <- as.matrix(LUX[,4:13], rownames.force = FALSE)
LUX.nms <- as.matrix(LUX[1:3], rownames.force = FALSE)
rm(list = c("LUX", "DATA"))

#### Initialization ####
dOpen <- 5
dOrder <- 1
nSim <- 1000
BMT.inv <- vector("list", length = nrow(BMT.val))
BMT.trns <- vector("list", length = nrow(BMT.val))

#### Initialization Loop ####
for (row in 1:nrow(BMT.val)) {
  dmd <- BMT.val[row, 7:8]
  modes <- BMT.val[row, 1:6]
  
  BMT.inv[[row]] <- gen.inv(nSim, BMT.nms[row,3], BMT.val[row, 9], dmd, dOpen, dOrder)
  BMT.trns[[row]] <- gen.trans(nSim, modes)
}

# gen.hub <- function(nSim, nm, curr, act, opNdays, ordNdays, modes, info = NA)
lux.hub <- gen.hub(nSim, LUX.nms[,3], LUX.val[,9], LUX.val[,7:8], dOpen, dOrder, LUX.val[, 1:6])

len <- length(BMT.inv)

####  SIMULATION LOOP  ####
for (time in 1:nSim) {
  for (fac in 1:len) {
    simulate(time, (BMT.inv[[fac]]), (BMT.trns[[fac]]), q)
  }
  simulate.hub(time, lux.hub, q)
}

saveData(BMT.inv, BMT.trns, lux.hub, name, quant)


