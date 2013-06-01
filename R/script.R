# Run source() on each of the follwing files for your machine in order to 
# load the neccessary functons to run the simulation

#### object definitions required to run simulation ####
source('C:/Users/a421356/R-GitHub/SCMsim/R/invRef.R')
source('C:/Users/a421356/R-GitHub/SCMsim/R/transRef.R')
source('C:/Users/a421356/R-GitHub/SCMsim/R/hubRef.R')
source('C:/Users/a421356/R-GitHub/SCMsim/R/simulation.R')

setwd('C:/Users/a421356/R-GitHub/SCMsim/data')
DATA <- read.csv("BUD1207.csv", colClasses = c(rep("character", 3), rep("numeric", 10)))

# sets directory to Output so we can keep our output seperate from the program files
setwd('C:/Users/a421356/R-GitHub/SCMsim/Output')

#### Name, Quantile & Initial Conditions ####
disrupt <- 21
name <- paste("7 Day Pipeline - Disruption Days -", disrupt)
qnt <- 0.95
dOpen <- 6
dOrder <- 1

# remember at the end that the first 500 records are thrown away without analysis! 
# (maybe save full data dump later)
nSim <- 1500

#### do we want to set the seed number (for reproducability)? ####
# defaults to NO, otherwise change NA to INTEGER of choice
seed <- NA
if (!is.na(seed)) set.seed(seed)

#### Subsetting ####
BMT.val <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 4:13], rownames.force = FALSE)
BMT.nms <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 1:3], rownames.force = FALSE)
LUX <- rbind(DATA[(DATA[ ,3] == "Lux Hub"), ], DATA[(DATA[ ,1] == "Lux Hub"), ])
LUX.val <- as.matrix(LUX[,4:13], rownames.force = FALSE)
LUX.nms <- as.matrix(LUX[1:3], rownames.force = FALSE)

#### All after this line are part of repeating structure for multiple simulation runs ####
# Previous lines organize the data & shouldn't change between batches of simulation runs
## Initialization for Factories, Hub & Transit
BMT.inv <- vector("list", length = nrow(BMT.val))
BMT.trns <- vector("list", length = nrow(BMT.val))

for (row in 1:nrow(BMT.val)) {
  dmd <- BMT.val[row, 7:8]
  modes <- BMT.val[row, 1:6]
  
  BMT.inv[[row]] <- gen.inv(nSim, BMT.nms[row,3], BMT.val[row, 9], dmd, dOpen, dOrder)
  BMT.trns[[row]] <- gen.trans(nSim, modes, dis = disrupt)
}
lux.hub <- gen.hub(nSim, LUX.nms[,3], LUX.val[,9], LUX.val[,7:8], dOpen, dOrder, LUX.val[, 1:6], dis = disrupt)


####  SIMULATION LOOP  ####
len <- length(BMT.inv)
for (time in 1:nSim) {
  for (fac in 1:len) {
    simulate(time, (BMT.inv[[fac]]), (BMT.trns[[fac]]), qnt)
  }
 simulate.hub(time, lux.hub, qnt)
}

OUTPUT.LOC <- saveData(BMT.inv, BMT.trns, lux.hub, name, qnt)
source('C:/Users/a421356/R-GitHub/SCMsim/R/Analysis.R')

#### Remove Reference Classes to prevent memory leaks ####
# use while in production NOT while debugging
# rm(list = c("BMT.inv", "BMT.trns", "lux.hub", "row", "time"))













