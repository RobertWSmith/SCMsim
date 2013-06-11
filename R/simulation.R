#### Simulation functions

#'
#'
simulate <- function(time, inv, trans, quant) {
  inv$iterate(time)
  inv$receive(time, trans)
  inv$order(time, trans, quant)
}

simulate.hub <- function(time, hub, quant) {
  for (fac in 1:length(hub$getFactory())) {
    simulate(time, ((hub$factory)[[fac]]), ((hub$f.trans)[[fac]]), quant)
  }
  simulate(time, hub$warehouse, hub$h.trans, quant)
}

simDF <- function(inv, trans) {
  rand.n <- as.data.frame(trans$transit.time)
  colnames(rand.n) <- c("orig_prep", "orig_dray", "orig_port", "ocean_transit", 
                        "dest_port", "dest_dray", "info_cycle")
  
  dfData <- as.data.frame(cbind(
    date = 1:nrow(rand.n),
    factory = inv$name,
    region = inv$region,
    daily_inv = inv$current,
    in_transit = trans$in.trns,
    actual_dmd = inv$actual,
    expected_dmd = inv$expected, 
    forecast_err = inv$error,
    running_err_sum = inv$error.sum,
    order_vol = trans$volume,
    release_dt = trans$rel.dt,
    pipeline_tgt = inv$pipe.tgt,
    inTransit_ord = inv$ITorder,
    demand_ord = inv$DMDorder,
    factory_op = inv$operating,
    factory_ord = inv$ordering,
    delivered = trans$delivered,
    rand.n
  ))
  
  return(dfData)
}

#' Saves inventory and transit class data in CSV form
#' 
#' This function enables the user to save the transit data, including all random numbers generated for the transit route
#' 
#' @param inv list of class inventory, created by gen.inv()
#' @param trans list of class transit, created by gen.trans()
#' @param hub class hub, created by gen.hub()
#' @param sim.name unique name for the simulation, to help organize the output
saveData <- function(inv, trans, hub, sim.name, quant) {
  L <- length(inv)
  dfSim <- simDF(inv[[1]], trans[[1]])
  for (i in 2:L) {
    dfSim <- rbind(dfSim, simDF(inv[[i]], trans[[i]]))
  }
  
  L <- length((hub$factory))
  for (i in 1:L) {
    dfSim <- rbind(dfSim, simDF(hub$factory[[i]], (hub$f.trans[[i]])))
  }
  
  dfSim <- rbind(dfSim, simDF(hub$warehouse, (hub$h.trans)))
  
  SIM.DIR <- file.path(BASE.DIR, sim.name)
  
  dir.create(SIM.DIR)
  nm <- "raw_output.csv"
  write.csv(dfSim, file = file.path(SIM.DIR, nm), quote = FALSE, row.names = FALSE)
  
  return(c(SIM.DIR, nm))
}

saveTrunc <- function(inv, trans, hub, sim.name, quant) {
  L <- length(inv)
  
  temp <- simDF(inv[[1]], trans[[1]])
  dfTrnc <- temp[500:nrow(temp), ]
  for (i in 2:L) {
    temp <- simDF(inv[[i]], trans[[i]])
    dfTrnc <- rbind(dfTrnc, temp[500:nrow(temp), ])
  }
  
  L <- length((hub$factory))
  for (i in 1:L) {
    temp <- simDF(hub$factory[[i]], (hub$f.trans[[i]]))
    dfTrnc <- rbind(dfTrnc, temp[500:nrow(temp), ])
  }
  
  temp <- simDF(hub$warehouse, (hub$h.trans))
  dfTrnc <- rbind(dfTrnc, temp[500:nrow(temp), ])
  
  SIM.DIR <- file.path(BASE.DIR, sim.name)
  
  nm <- "trunc_output.csv"
  write.csv(dfTrnc, file = file.path(SIM.DIR, nm), quote = FALSE, row.names = FALSE)
}

#' Runs the simulation
#' 
#' @param DATA a specially formatted data frame
#' @param nSim the number of simulatino days to run
#' @param dOpen the number of days the factories will be open per week
#' @param disrupt a vector length two, element one is disruption begin, element two is disruption length
#' @param qnt the quantile of transit time to consider
#' @param bias the forecast bias to consider (0 is no bias, -1 is consistent overestimates, +1 is consistent underestimate)
#' @param seed the random number seed for repeatability
simulation <- function(DATA, nSim, dOpen, disrupt, qnt, bias, amp, seed = NA, STRAT) {
  if (!is.na(seed)) set.seed(seed)
  
  name <- paste0("STRAT ", STRAT, " OP", dOpen, " DAYS", nSim, " DISR", disrupt[1], " LEN", disrupt[2], " SL", (qnt * 100))
  
  #### Subsetting ####
  BMT.val <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 4:ncol(DATA)], rownames.force = FALSE)
  BMT.nms <- as.matrix(DATA[((DATA[ ,1] == "Beaumont") & (DATA[ ,3] != "Lux Hub")), 1:3], rownames.force = FALSE)
  LUX <- rbind(DATA[(DATA[ ,3] == "Lux Hub"), ], DATA[(DATA[ ,1] == "Lux Hub"), ])
  LUX.val <- as.matrix(LUX[,4:ncol(DATA)], rownames.force = FALSE)
  LUX.nms <- as.matrix(LUX[1:3], rownames.force = FALSE)
  
  #### Initialization ####
  BMT.inv <- vector("list", length = nrow(BMT.val))
  BMT.trns <- vector("list", length = nrow(BMT.val))
  
  #### Initialization Loop ####
  for (row in 1:nrow(BMT.val)) {
    fac.name <- as.character(BMT.nms[row, 3])
    fac.rgn <- as.character(BMT.nms[row, 2])
    curr.inv <- as.numeric(BMT.val[row, 9]) * 2
    act.dist <- as.numeric(BMT.val[row, 7:8])
    ord.days <- as.numeric(BMT.val[row, 11])
    modes <- as.numeric(BMT.val[row, 1:6])
    ship.sz <- as.numeric(BMT.val[row, 12])
    info.cycl <- as.numeric(BMT.val[row, 13])
    
    BMT.inv[[row]] <- gen.inv(nSim, fac.name, fac.rgn, curr.inv, act.dist, dOpen, ord.days, bias, amp, STRAT)
    BMT.trns[[row]] <- gen.trans(nSim, modes, ship.sz, info.cycl)
  }
  
  hub.nms <- as.character(LUX.nms[ ,3])
  hub.rgn <- as.character(LUX.nms[ ,2])
  hub.curr <- as.numeric(LUX.val[ ,9]) * 2
  hub.actDist <- as.matrix(LUX.val[ ,7:8])
  hub.ordDays <- as.numeric(LUX.val[ ,11])
  hub.modes <- as.matrix(LUX.val[ ,1:6])
  hub.shipSz <- as.numeric(LUX.val[ ,12])
  hub.infoCycl <- as.numeric(LUX.val[ ,13])
  
  lux.hub <- gen.hub(nSim, hub.nms, hub.rgn, hub.curr, hub.actDist, dOpen, 
                     hub.ordDays, hub.modes, hub.shipSz, bias, amp, hub.infoCycl, STRAT)
  
  #### freeing up memory ####
  rm(list = c("fac.name", "fac.rgn", "curr.inv", "act.dist", "ord.days", "modes", "ship.sz", "info.cycl"))
  rm(list = c("hub.nms", "hub.rgn", "hub.curr", "hub.actDist", "hub.ordDays", "hub.modes", "hub.shipSz", "hub.infoCycl"))
  rm(list = c("BMT.val", "BMT.nms", "LUX", "LUX.val", "LUX.nms"))
  
  ####  SIMULATION LOOP  ####
  for (time in 1:nSim) {
    for (fac in 1:length(BMT.inv)) {
      simulate(time, (BMT.inv[[fac]]), (BMT.trns[[fac]]), qnt)
    }
    simulate.hub(time, lux.hub, qnt)
  }
  SIM.DIR <- saveData(BMT.inv, BMT.trns, lux.hub, name, qnt)
  saveTrunc(BMT.inv, BMT.trns, lux.hub, name, qnt)
  
  # to contain memory leaks
  rm(list = c("BMT.inv", "BMT.trns", "lux.hub"))
  
  if (disrupt[2] <= 0) {
    disrupt <- FALSE
  }
  
  SIM.DIR[2] <- "trunc_output.csv"
  
  analysis((nSim - 500), SIM.DIR[1], SIM.DIR[2], DATA, disrupt)
}


