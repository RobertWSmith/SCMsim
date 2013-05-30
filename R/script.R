# Run source() on each of the follwing files for your machine in order to 
# load the neccessary functons to run the simulation

source('C:/Users/Rob/R/SCMsim/R/invRef.R')
source('C:/Users/Rob/R/SCMsim/R/transRef.R')
source('C:/Users/Rob/R/SCMsim/R/hubRef.R')
source('C:/Users/Rob/R/SCMsim/R/simulation.R')

setwd('C:/Users/Rob/R/SCMsim/data')
DATA <- read.csv("BUD1207.csv", colClasses = c(rep("character", 3), rep("numeric", 10)))

# sets directory to Output so we can keep our output seperate from the program files
setwd('C:/Users/Rob/R/SCMsim/Output')

#### Name & Quantile ####
name <- "TEST"
qnt <- 0.95

#### Subsetting ####
BMT.val <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 4:13], rownames.force = FALSE)
BMT.nms <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 1:3], rownames.force = FALSE)
LUX <- rbind(DATA[(DATA[ ,3] == "Lux Hub"), ], DATA[(DATA[ ,1] == "Lux Hub"), ])
LUX.val <- as.matrix(LUX[,4:13], rownames.force = FALSE)
LUX.nms <- as.matrix(LUX[1:3], rownames.force = FALSE)
rm(list = c("LUX", "DATA"))

#### Initialization ####
dOpen <- 6
dOrder <- 1
nSim <- 1000
BMT.inv <- vector("list", length = nrow(BMT.val))
BMT.trns <- vector("list", length = nrow(BMT.val))

#### Initialization Loop ####
for (row in 1:nrow(BMT.val)) {
  dmd <- BMT.val[row, 7:8]
  modes <- BMT.val[row, 1:6]
  
  BMT.inv[[row]] <- gen.inv(nSim, BMT.nms[row,3], BMT.val[row, 9], dmd, dOpen, dOrder)
  BMT.trns[[row]] <- gen.trans(nSim, modes, dis = 14)
}

# gen.hub <- function(nSim, nm, curr, act, opNdays, ordNdays, modes, info = NA)
lux.hub <- gen.hub(nSim, LUX.nms[,3], LUX.val[,9], LUX.val[,7:8], dOpen, dOrder, LUX.val[, 1:6], dis = 14)

####  SIMULATION LOOP  ####
len <- length(BMT.inv)
for (time in 1:nSim) {
  for (fac in 1:len) {
    simulate(time, (BMT.inv[[fac]]), (BMT.trns[[fac]]), qnt)
  }
  simulate.hub(time, lux.hub, qnt)
}

saveData(BMT.inv, BMT.trns, lux.hub, name, qnt)

rm(list = c("BMT.nms", "BMT.val", "LUX.nms", "LUX.val", "BMT.inv", "BMT.trns", "dOpen", "dOrder", "dmd", "fac", "len", "lux.hub", "modes", "name", "qnt", "row", "time"))

source('C:/Users/Rob/R/SCMsim/R/Analysis.R')
