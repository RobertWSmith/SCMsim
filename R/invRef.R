### Inventory Reference Class

inv <- setRefClass(
  "inventory",
  fields = list(
    name = "character",
    region = "character",
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
    ord.pW = "numeric",
    STRAT = "character",
    disruption = "logical"
  )
)


inv$methods(
  first = function(nm, rgn, curr, expect, act, op, ord, ord.n, strat) {
    err <- (act - expect)
    err.sm <- cumsum(err)
    
    name <<- nm
    region <<- rgn
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
    STRAT <<- strat
    ord.pW <<- ord.n
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
    
    if (STRAT == "PW2RWS") {
      PW2RWS(time, trans, quant)
    } else if (STRAT == "ROP") {
      ROP(time, trans)
    } else if (STRAT == "USED") {
      USED(time, trans)
    }
  },
  PW2RWS = function(time, trans, quant) {
    if (getOrdering(time)) {
      samples <- 50
      # the next few lines find an estimated transit time
      estTT <- matrix(0, nrow = samples, ncol = ncol(trans$transit.time))
      for (i in 1:samples) {
        estTT[i, ] <- trans$randTrans()
      } 
      # quant is the quantile supplied for transit time, 
      qTT <- round(quantile(apply(estTT, 1, sum), probs = quant))
      
      setPipeTgt(time, (qTT * mean(expected)))
      setITorder(time, (getPipeTgt(time) - trans$getITVolume(time)))
      setDMDorder(time, (sum(getExpectedR((time), 7)) + sum(getErrorR(time, 7))))
      
      shpSZ <- trans$getShipmentSize()
      
      order <- (ceiling((getITorder(time) + getDMDorder(time) - getCurrent(time)) / shpSZ)) * shpSZ
      if (order <= 0) order <- 0
      trans$outbound(time, order)
    }
  },
  ROP = function(time, trans) {
    samples <- 50
    
    shpSZ <- trans$getShipmentSize()
    
    trans.samp <- getTransSamp(time, samples, trans)
    trans.mn <- round(mean(trans.samp))
    dmd.mn <- mean(getExpectedR(time, samples))
    
    min <- trans.mn * dmd.mn * 0.75
    max <- trans.mn * dmd.mn
    
    if (getCurrent(time) < min) {
      order <- max - getCurrent(time)
      order <- (ceiling(order / shpSZ)) * shpSZ
    } else {
      order <- 0
    }
    
    trans$outbound(time, order)
  },
  USED = function(time, trans) {
    shpSZ <- trans$getShipmentSize()
    
    if (time > 2) {
      setDMDorder(time, getActual(time) + getDMDorder((time - 1)))
      today <- getDMDorder(time)
    } else {
      today <- 0
    }
    
    if (today > shpSZ) {
      trans$outbound(time, shpSZ)
      setDMDorder(time, (getDMDorder(time) - shpSZ))
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
  getDMDorderR = function(time, range) {
    start <- ifelse((time - range) < 1, 1, (time - range + 1))
    return(DMDorder[start:time])
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
  getTransSamp = function(time, range, trans) {
    if (time > range) {
      t <- apply(trans$getTransR(time, range), 1, sum, na.rm = TRUE)
    } else {
      t <- rep(0, range)
      for (i in 1:range) {
        t[i] <- sum(trans$randTrans(), na.rm = TRUE)
      }
    }
    
    ot <- ceiling(length(t) / ord.pW)
    temp <- matrix(0, nrow = ot, ncol = ord.pW)
    for (i in 1:ord.pW) {
      temp[ ,i] <- seq(i, by = 7, length.out = ot)
    }
    temp <- as.numeric(temp)
    smp <- sort(temp[1:length(t)], method = "quick")
    
    t <- t + smp
    t <- diff(t)
    return(t)
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
#' @param amp value for the amplitude of forecast error (multiplicative)
#' 
#' @keyword inventory initializer
#' 
#' @example
#' test <- gen.inv()
gen.inv <- function(nSim, nm, rgn, curr, act, opNdays, ordNdays, bias = c(0.5, 0.5), amp = 0.25, STRAT) {
  stopifnot(is.numeric(nSim) & (length(nSim) == 1))
  stopifnot(is.character(nm))
  stopifnot(is.character(rgn))
  stopifnot(is.numeric(nSim) & (length(nSim) == 1))
  stopifnot(is.numeric(curr) & (length(curr) == 1))
  stopifnot(is.numeric(opNdays) & (length(opNdays) == 1))
  stopifnot(is.numeric(ordNdays) & (length(ordNdays) == 1))
  
  name <- rep(nm, nSim)
  region <- rep(rgn, nSim)
  op.v <- gen.sched(opNdays, nSim)
  ord.v <- gen.sched(ordNdays, nSim)
  curr.inv <- c(curr, rep(0, length = (nSim - 1)))
  
  a.dmd <- sqrt((rnorm(nSim, act[1], act[2]))^2)
  e <- abs(rnorm(nSim, 0, act[2]))
  e.bias <- sample(c(-1,1), size = nSim, replace = TRUE, prob = bias)
  e.dmd <- a.dmd + (e * e.bias * amp)
  
  
  for (i in 1:nSim) {
    if (!op.v[i]) {
      a.dmd[i] <- 0
      e.dmd[i] <- 0
    }
  }
  
  temp <- inv$new()
  temp$first(name, region, curr.inv, e.dmd, a.dmd, op.v, ord.v, ordNdays, STRAT)
  
  return(temp)
}
