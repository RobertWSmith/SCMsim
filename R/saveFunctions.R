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
