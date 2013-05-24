### Inventory Reference Class


inv <- setRefClass(
  "inventory",
  fields = list(
    name = "character",
    current = "numeric",
    actual = "numeric",
    expected = "numeric",
    error = "numeric",
    error.sq = "numeric",
    pipe.tgt = "numeric",
    ITorder = "numeric",
    DMDorder = "numeric",
    operating = "logical",
    ordering = "logical"
    )
  )

inv$methods(
  first = function(nm, curr, expect, act, op, ord) {
    name <<- nm
    current <<- curr
    expected <<- expect
    actual <<- act
    error <<- (act - expect)
    error.sq <<- (act - expect)^2
    pipe.tgt <<- rep(0, length(curr))
    ITorder <<- rep(0, length(curr))
    DMDorder <<- rep(0, length(curr))
    operating <<- op
    ordering <<- ord
  },
  iterate = function(time) {
    if (getOperating(time)) {
      chk <- current[time] - actual[time]
      if (chk < 0) chk <- 0
      current[(time+1)] <<- chk
    } else {
      current[(time+1)] <<- current[time]
    }
  },
  receive = function(time, trans) {
    setCurrent(time, trans$inbound(time))
  },
  order = function(time, trans, quant) {
    trans$setITVolume(time) # no matter what, update in transit volume
    
    if (getOrdering(time)) {
      # the next few lines find an estimated transit time
      estTT <- rep(0, length = 50)
      for (i in 1:50) {
        estTT[i] <- sum(trans$randTrans())
      } 
      # qTT is the quantile supplied for transit time, mTT is the mean transit time
      qTT <- ceiling(quantile(estTT, probs = quant))
      
      setPipeTgt(time, (mean(getExpectedR(time, qTT)) * qTT))
      setITorder(time, getPipeTgt(time) - trans$getITVolume(time))
      setDMDorder(time, (sum(getExpectedR(time, 7)) + sum(getErrorR(time, 7))))
      
      order <- (ceiling((getITOrder(time) + getDMDorder(time)) / 18000)) * 18000
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
    start <- ifelse((time - range) < 1, 1, (time - range))
    return(error[start:time])
  },
  getError = function(time) {
    return(error[time])
  },
  getErrorSqR = function(time, range) {
    start <- ifelse((time - range) < 1, 1, (time - range))
    return(error.sq[start:time])
  },
  getErrorSq = function(time) {
    return(error.sq[time])
  },
  getCurrent = function(time) {
    return(current[(time + 1)])
  },
  setCurrent = function(time, vol) {
    current[(time+1)] <<- current[(time+1)] + vol
  },
  getExpectedR = function(time, range) {
    end <- ifelse((time + range) >= (length(expected)), (length(expected)), (time + range))
    return(expected[time:end])
  },
  getExpected = function(time) {
    return(expected[time])
  },
  getActualR = function(time, range) {
    start <- ifelse((time - range) < 1, 1, (time - range))
    return(actual[start:time])
  },
  getActual = function(time) {
    return(actual[time])
  },
  getOperatingR = function(time, range) {
    end <- ifelse((time + range) > (length(operating)), (length(operating)), (time + range))
    return(operating[time:end])
  },
  getOperating = function(time) {
    return(operating[time])
  },
  getOrdering = function(time) {
    return(ordering[time])
  }
  )

# difference between nWork (working days in a week)
# totSim is the total days in the simulation
### example
# (sch <- gen.sched(5, 7, 10))
gen.sched <- function(nOp, totSim) {
  wrk <- vector("logical", totSim)
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
#' @param act is a vector of actual demand
#' @param opNdays is the nummber of days the factory works per week
#' @param ordNdays is the number of days the factory places an order per week
#' @keyword inventory initializer
#' 
#' @example
#' test <- gen.inv()
gen.inv <- function(nSim, nm, curr, expect, act, opNdays, ordNdays) {
  stopifnot(is.character(nm))
  stopifnot(is.numeric(nSim) & length(nSim) == 1)
  stopifnot(is.numeric(curr) & length(curr) == 1)
  stopifnot(is.numeric(expect) & length(expect) == 2)
  stopifnot(is.numeric(act) & length(act) == 2)
  stopifnot(is.numeric(opNdays) & length(opNdays) == 1)
  stopifnot(is.numeric(ordNdays) & length(ordNdays) == 1)
  
  name <- rep(nm, nSim)
  opNdays <- gen.sched(opNdays, nSim)
  ordNdays <- gen.sched(ordNdays, nSim)
  curr.inv <- c(curr, rep(0, length = (nSim - 1)))
  
  a.dmd <- sqrt((rnorm(nSim, act[1], act[2]))^2)
  e.dmd <- sqrt((rnorm(nSim, expect[1], expect[2]))^2)
  
  a.dmd[!opNdays] <- 0
  e.dmd[!opNdays] <- 0
  
  temp <- inv$new()
  temp$first(name, curr.inv, e.dmd, a.dmd, opNdays, ordNdays)
  
  return(temp)
}






















