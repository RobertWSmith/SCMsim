#### Simulation functions

#'
#'
simulate <- function(nSim, inv, trans, dest.name, sim.name, quant = 0.95) {
  time <- 0
  while (time < (nSim - 1)) {
    time <- time + 1
    inv$iterate(time)
    inv$receive(time, trans)
    inv$order(time, trans, quant)
  }
  
  saveData(inv, trans, dest.name, sim.name)
}

#' Saves inventory and transit class data in CSV form
#' 
#' This function enables the user to save the transit data, including all random numbers generated for the transit route
#' 
#' @param inv object of class inventory, created by gen.inv()
#' @param trans object of class transit, created by gen.trans()
#' @param dest.name unique name for the destination factory, to help organize the output
#' @param sim.name unique name for the simulation, to help organize the output
saveData <- function(inv, trans, dest.name, sim.name) {
  rand.n <- trans$transit.time
  colnames(rand.n) <- paste0("RN#", 1:ncol(rand.n))
  
  dfData <- as.data.frame(
    cbind(
      current = inv$current,
      actual = inv$actual,
      expected = inv$expected,
      in_transit_inventory = trans$in.trns,
      fcst_error = inv$error,
      operating_day = inv$operating,
      ordering_day = inv$ordering,
      shipped_volume = trans$volume,
      release_date = trans$rel.dt,
      delivered = trans$delivered,
      random_numbers = rand.n
    )
  )
  
  nm <- paste0(sim.name, " ", dest.name, ".csv")
  write.csv(dfData, file = nm, quote = FALSE)
}
  


