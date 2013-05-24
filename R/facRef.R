
# temp

# test.inv <- vector("list", length = 10)
# test.trns <- vector("list", length = 10)
# 
# for (row in 1:length(test)) {
#   dmd <- as.numeric(simData[row, 10:11])
#   nSim <- 1000
#   start <- simData[row, 12]
#   a.dmd <- sqrt((rnorm(nSim, dmd[1], dmd[2]))^2)
#   e.dmd <- sqrt((rnorm(nSim, dmd[1], dmd[2]))^2)
#   dOpen <- 5
#   dOrder <- 1
#   
#   modes <- as.numeric(simData[row, 4:9])
#   
#   test.inv[[row]] <- gen.inv(nSim, simData[row,3], start, e.dmd, a.dmd, dOpen, dOrder)
#   test.trns[[row]] <- gen.trans(nSim, modes)
# }
# 
# for (i in 1:10) {
#   print((test.inv[[i]])$getName())
# }

