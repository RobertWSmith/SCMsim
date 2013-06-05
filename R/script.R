# Run source() on each of the follwing files for your machine in order to 
# load the neccessary functons to run the simulation

BASE.DIR <- getwd()

source(file.path(BASE.DIR, 'R', "invRef.R"))
source(file.path(BASE.DIR, 'R', "transRef.R"))
source(file.path(BASE.DIR, 'R', "hubRef.R"))
source(file.path(BASE.DIR, 'R', "simulation.R"))

DATA <- read.csv(file.path(BASE.DIR, 'data', "BUD1207.csv"), 
                 colClasses = c(rep("character", 3), rep("numeric", 10)))

#### Name & Quantile ####
dOpen <- 6
dOrder <- 6
nSim <- 1500
disrupt <- c(1, 1)
bias <- 0
qnt <- 0.95

seed <- NA
if (!is.na(seed)) set.seed(seed)

name <- paste0("OP", dOpen, " DAYS", nSim, " DISR", disrupt[1], " LEN", disrupt[2], " SL", (qnt * 100))

#### Subsetting ####
BMT.val <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 4:13], rownames.force = FALSE)
BMT.nms <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 1:3], rownames.force = FALSE)
LUX <- rbind(DATA[(DATA[ ,3] == "Lux Hub"), ], DATA[(DATA[ ,1] == "Lux Hub"), ])
LUX.val <- as.matrix(LUX[,4:13], rownames.force = FALSE)
LUX.nms <- as.matrix(LUX[1:3], rownames.force = FALSE)

#### Initialization ####
BMT.inv <- vector("list", length = nrow(BMT.val))
BMT.trns <- vector("list", length = nrow(BMT.val))

#### Initialization Loop ####
for (row in 1:nrow(BMT.val)) {
  dmd <- BMT.val[row, 7:8]
  modes <- BMT.val[row, 1:6]
  
  BMT.inv[[row]] <- gen.inv(nSim, BMT.nms[row,3], BMT.val[row, 9], dmd, dOpen, dOrder)
  BMT.trns[[row]] <- gen.trans(nSim, modes)
}

lux.hub <- gen.hub(nSim, LUX.nms[,3], LUX.val[,9], LUX.val[,7:8], dOpen, dOrder, LUX.val[, 1:6])

####  SIMULATION LOOP  ####
for (time in 1:nSim) {
  for (fac in 1:length(BMT.inv)) {
    simulate(time, (BMT.inv[[fac]]), (BMT.trns[[fac]]), qnt)
  }
  simulate.hub(time, lux.hub, qnt)
}

SIM.DIR <- saveData(BMT.inv, BMT.trns, lux.hub, name, qnt)

source(file.path(BASE.DIR, 'R', "Analysis.R"))





