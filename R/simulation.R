#### Simulation functions

#'
#'
simulate <- function(time, inv, trans, quant) {
  inv$iterate(time)
  inv$receive(time, trans)
  inv$order(time, trans, quant)
}

simulate.hub <- function(time, hub, quant) {
  L <- length(hub$getFactory())
  for (fac in 1:L) {
    simulate(time, ((hub$factory)[[fac]]), ((hub$f.trans)[[fac]]), quant)
  }
  simulate(time, hub$warehouse, hub$h.trans, quant)
}

simDF <- function(inv, trans) {
  rand.n <- as.data.frame(trans$transit.time)
  colnames(rand.n) <- paste0("RN#", 1:ncol(rand.n))
  
  dfData <- as.data.frame(cbind(
    factory = inv$name,
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
    temp <- simDF(hub$factory[[i]], (hub$f.trans[[i]]))
    dfSim <- rbind(dfSim, temp)
  }
  
  temp <- temp <- simDF(hub$warehouse, (hub$h.trans))
  dfSim <- rbind(dfSim, temp)
  
  nm <- paste0(sim.name, " - quant ", quant, ".csv")
  write.csv(dfSim, file = nm, quote = FALSE)
}

