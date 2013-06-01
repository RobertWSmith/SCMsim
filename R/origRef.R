#### Origin Factory Reference Class ####
# requires, invRef.R, transRef.R, hubRef.R

ORIGIN <- setRefClass(
  "origin",
  fields = list(
    name = "character",
    inventory = "numeric",
    prod.rate = "numeric",
    work.sched = "logical",
    port.sched = "logical",
    hub = vector("list", length = 1),
    factories = vector("list", length = 1),
    trasit.lanes = vector("list", length = 1)
  )
)

ORIGIN$methods(
  first = function(nm, inv, pr, ws, ps, HUB, FAC, TRNS) {
    stopifnot(is.numeric(pr))
    stopifnot(length(pr) == 2)
    stopifnot(is.list(FAC))
    stopifnot(is.list(TRNS))
    stopifnot(length(FAC) == length(TRNS))
    
    inventory <<- rep(0, )
    inventory[1] <<- inv
    work.sched <<- ws
    port.sched <<- ps
    
    hub <- as.list(HUB)
    if (length(HUB) > 1) {
      for (i in 2:length(HUB)) {
        hub <- as.list(hub, HUB[[i]])
      }
    }
    
    factories <- as.list(FAC[[1]])
    transit.lanes <- as.list(TRNS[[1]])
    if (length(FAC) > 1) {
      for (i in 2:length(FAC)) {
        factories <- as.list(factories, FAC[[i]])
        transit.lanes <- as.list(transit.lanes, TRNS[[i]])
      }
    }
    
    # exppect pr as c(mean, sd) vector
    tmp <- sqrt((rnorm(length(work.sched), 0, pr[2]))^2)
    tmp[1] <- pr[1]
    prod.rate <<- cumsum(tmp)
    
    for (i in 1:length(prod.rate)) {
      if (!work.sched[i]) {
        prod.rate[i] <- 0
      }
    }
    name <<- rep(nm, length(work.sched))
  },
  iterate = function(time) {
    inventory[(time + 1)] <- getInventory(time) + prod.rate
    for (i in 1:length(factories)) {
      (factories[[i]])$iterate(time)
    }
    for (i in 1:length(hub)) {
      hub[[i]]$iterate(time)
    }
  },
  order = function(time, vol, quant) {
    
    
    
  },
  getWorking = function(time) {
    return(work.sched[time])
  },
  getPort = function(time) {
    return(port.sched[time])
  },
  getMatlName = function() {
    return(matl.name)
  },
  setInventory = function(time, reduction) {
    inventory[time] <<- getInventory(time) - reduction
  },
  getInventory = function(time) {
    return(inventory[time])
  },
  getProdRate = function(time) {
    return(prod.rate[time])
  },
  getOrginName = function() {
    return(name)
  }
)

# nOp the number of open days per week
# totSim the number of days in the simulation experiment
gen.sched <- function(nOp, totSim) {
  wrk <- logical(length = totSim)
  for (i in 1:totSim) {
    if (i %% 7 < nOp) wrk[i] <- TRUE
    else  wrk[i] <- FALSE
  }
  return(wrk)
}


#' Generates Origin Factory / Simulation Container
#' 
#' This function is the S3 wrapper that helps 'tidy up' the initialization of 
#' Reference Objects of class "origin"
#' 
#' @param o.name character string for the origin name
#' @param 0.inv numeric value for the starting inventory
#' @param o.RR numeric vector for the Replenishment Rate ex: c(mean, sd)
#' @param wd numeric value for the number of work days per week
#' @param pd numeric vector for the number of port cutoffs per week (should be equal to # of factories + # of hubs)
#' @param HUB object or list containing elements of class "hub"
#' @param FAC object or list containing elements of class "factory"
#' @param TRSN object or list containing elements of class "transit"
gen.origin <- function(nSim, o.name, o.inv, o.RR, wd, pd, HUB, FAC, TRNS ) {
  stopifnot(wd <= 7)
  stopifnot(wd > 0)
  stopifnot(pd <= 7)
  stopifnot(pd > 0)
  
  work <- gen.sched(wd)
  
  if (pd == wd) {
    port <- work
  } else if (pd == 1) {
    port <- gen.sched(pd, nSim)
  } else {
    port <- gen.sched(pd, nSim)
    tmp <- (c(rep(FALSE, 7 - pd), port))[1:nSim]
    port <- port | tmp
  }

  or <- ORIGIN$new()
  or$first(o.name, o.inv, o.RR, work, port, HUB, FAC, TRNS)
  return(or)
}









