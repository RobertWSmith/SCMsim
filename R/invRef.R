### Inventory Reference Class




inv <- setRefClass(
  "inventory",
  fields = list(
    name = "character",
    current = "numeric",
    actual = "numeric",
    expected = "numeric",
    error = "numeric",
    error.sum = "numeric",
    pipe.tgt = "numeric",
    ITorder = "numeric",
    DMDorder = "numeric",
    operating = "logical",
    ordering = "logical",
    disruption = "logical"
  )
)


inv$methods(
  first = function(nm, curr, expect, act, op, ord) {
    err <- (act - expect)
    err.sm <- cumsum(err)
    
    name <<- nm
    current <<- curr
    expected <<- expect
    actual <<- act
    error <<- err
    error.sum <<- err.sm
    pipe.tgt <<- rep(0, length(curr))
    ITorder <<- rep(0, length(curr))
    DMDorder <<- rep(0, length(curr))
    operating <<- op
    ordering <<- ord
  },
  iterate = function(time) {
    if (getOperating(time)) {
      chk <-  getCurrent(time) - getActual(time)
      if (chk < 0) chk <- 0
      updCurrent(time, chk)
    } else {
      updCurrent(time, getCurrent(time))
    }
  },
  receive = function(time, trans) {
    updCurrent(time, trans$inbound(time))
  },
  order = function(time, trans, quant) {
    trans$setITVolume(time) # no matter what, update in transit volume
    
    if (getOrdering(time)) {
      samples <- 50
      #       the next few lines find an estimated transit time
      estTT <- matrix(0, nrow = samples, ncol = ncol(trans$transit.time))
      for (i in 1:samples) {
        estTT[i, ] <- trans$randTrans()
      } 
      # quant is the quantile supplied for transit time, 
      qTT <- round(quantile(apply(estTT, 1, sum), probs = quant))
      
      setPipeTgt(time, (qTT * mean(expected)))
      setITorder(time, (getPipeTgt(time) - trans$getITVolume(time)))
      setDMDorder(time, (sum(getExpectedR((time), 7)) + sum(getErrorR(time, 7))))
      
      shp.sz <- trans$getShipmentSize()
      
      order <- (ceiling((getITorder(time) + getDMDorder(time) - getCurrent(time)) / shp.sz)) * shp.sz
      if (order <= 0) order <- 0
      trans$outbound(time, order)
    }
  },
  getName = function() {
    return(name[1])
  },
  getPipeTgt = function(time) {
    return(pipe.tgt[time])
  },
  setPipeTgt = function(time, vol) {
    pipe.tgt[time] <<- vol
  },
  getDMDorder = function(time) {
    return(DMDorder[time])
  },
  setDMDorder = function(time, vol) {
    DMDorder[time] <<- vol
  },
  getITorder = function(time) {
    return(ITorder[time])
  },
  setITorder = function(time, vol) {
    ITorder[time] <<- vol
  },
  getErrorR = function(time, range) {
    start <- ifelse((time - range) < 1, 1, (time - range + 1))
    return(error[start:time])
  },
  getError = function(time) {
    return(error[time])
  },
  getCurrent = function(time) {
    return(current[time])
  },
  updCurrent = function(time, vol) {
    if (!is.na(getCurrent((time + 1)))) {
      current[(time+1)] <<- (getCurrent((time + 1))) + vol
    }
  },
  getExpectedR = function(time, range) {
    end <- ifelse((time + range) >= (length(expected)), (length(expected)), (time + range - 1))
    return(expected[(time):end])
  },
  getExpected = function(time) {
    return(expected[time])
  },
  getActualR = function(time, range) {
    start <- ifelse((time - range) < 1, 1, (time - range + 1))
    return(actual[start:time])
  },
  getActual = function(time) {
    return(actual[time])
  },
  getOperatingR = function(time, range) {
    end <- ifelse((time + range) > (length(operating)), (length(operating)), (time + range - 1))
    return(operating[time:end])
  },
  getOperating = function(time) {
    return(operating[time])
  },
  getOrdering = function(time) {
    return(ordering[time])
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

#' Function to set up a simulation inventory
#' 
#' This function sets up the inventory reference class for the raw material simulation.
#' 
#' @param nSim is a integer representation of the number of days in the simulation
#' @param curr is a number for the first day's inventory
#' @param expect is a vector of expected demand
#' @param act is a vector with element 1 == mean of demand, element 2 == sd of demand
#' @param opNdays is the nummber of days the factory works per week
#' @param ordNdays is the number of days the factory places an order per week
#' @param bias value for the expected demand bias, 0 corresponds to no bias, -1 is a consistent underestimate, 1 is a consistent overestimate
#' 
#' @keyword inventory initializer
#' 
#' @example
#' test <- gen.inv()
gen.inv <- function(nSim, nm, curr, act, opNdays, ordNdays, bias = 0) {
  stopifnot(is.numeric(nSim) & (length(nSim) == 1))
  stopifnot(is.character(nm))
  stopifnot(is.numeric(nSim) & (length(nSim) == 1))
  stopifnot(is.numeric(curr) & (length(curr) == 1))
  stopifnot(is.numeric(opNdays) & (length(opNdays) == 1))
  stopifnot(is.numeric(ordNdays) & (length(ordNdays) == 1))
  
  name <- rep(nm, nSim)
  opNdays <- gen.sched(opNdays, nSim)
  ordNdays <- gen.sched(ordNdays, nSim)
  curr.inv <- c(curr, rep(0, length = (nSim - 1)))
  
  a.dmd <- sqrt((rnorm(nSim, act[1], act[2]))^2)
  e <- runif(nSim, 0.5, 1.5) 
  e.dmd <- a.dmd * e
  
  for (i in 1:nSim) {
    if (!opNdays[i]) {
      a.dmd[i] <- 0
      e.dmd[i] <- 0
    }
  }
  
  temp <- inv$new()
  temp$first(name, curr.inv, e.dmd, a.dmd, opNdays, ordNdays)
  
  return(temp)
}
