library(roxygen2)
library(devtools)

#' A package to aide in Supply Chain Research problems.
#' 
#' @author Rob Smith <Rob_Smith@goodyear.com>
#' @maintainer Rob_Smith <Rob_Smith@goodyear.com>
#' 
#' @import ggplot2 scales triangle
#' @name OpsSim
#' @docType package
NULL

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


# basic intializer for tranist refernce class
#' Initializes bare transit class
#' 
#' @param mean numeric vector of mean transit times 
#' @param nDays number of days the simulation will run
#' @param info average amount of time to factor for information cycle, NA represents
#' @keywords transit initiaializer
#' @export
#' @examples
#' test1 <- transit(1000, c(2,4,18,6,1))
#' test2 <- transit(25, c(4,5,16,2), 2)
#' 
gen.trans <- function(nSim, modes, shpSz, info = NA) {
  vl <- rep(0, length = nSim)
  rl <- rep((nSim + 1), nSim)
  dl <- rep(FALSE, nSim)
  tt <- gen.randT(nSim, modes, info)
  
  trns <- trans$new()
  trns$first(vl, rl, dl, tt, shpSz)
  
  return(trns)
}



# everything in gen.inv that is length == 1 is a vector length n
# evrything that is a vector is a matrix, with columns length n, rows length nSim
# HUB DATA IS ROW 1, FACTORY DATA IS ROW 2 THROUGH THE END OF THE MATRIX
gen.hub <- function(nSim, nm, rgn, curr, act, opNdays, ordNdays, modes, shpSz, bias, amp, info = NA, STRAT) {
  h.name <- as.character(nm[1])
  f.name <- as.character(nm[-1])
  h.rgn <- as.character(rgn[1])
  f.rgn <- as.character(rgn[-1])
  h.curr <- as.numeric(curr[1])
  f.curr <- as.numeric(curr[-1])
  h.act <- (act[1, ])
  f.act <- (act[2:nrow(act), ])
  h.modes <- (modes[1, ])
  f.modes <- (modes[2:nrow(modes), ])
  h.shpSz <- as.numeric(shpSz[1])
  f.shpSz <- as.numeric(shpSz[-1])
  h.ord <- as.numeric(ordNdays[1])
  f.ord <- as.numeric(ordNdays[-1])
  h.info <- as.numeric(info[1])
  f.info <- as.numeric(info[-1])
  
  h <- HUB$new()
  
  h$first.fa(nSim, f.name, f.rgn, f.curr, f.act, opNdays, f.ord, bias, amp, STRAT)
  h$first.ob(nSim, f.modes, f.shpSz, f.info)
  h$first.wh(nSim, h.name, h.rgn, h.curr, opNdays, h.ord, STRAT)
  h$first.ib(nSim, h.modes, h.shpSz, h.info)
  
  return(h)
}



#### Transit Lane Reference Class

### Random numbers for transit generation

#' Generate random numbers along a discrete triangular distribution
#' 
#' This function generates random triangular distributed nubmers given a mode and
#' the number desired. The function is able to accept a vector of modes or a single
#' mode, and returns a matrix of random numbers. Info, if initialized will return a
#' additional column of binomial distributed random numbers with the probability given.
#' 
#' @param nSim number of rows of random numbers to be returned
#' @param modes vector of modes of different transit steps
#' @param info mean number of days for information cycle
#' @param seed set if seed is desired
#' @keyword simulation setup
#' @examples
#' test1 <- gen.randT(1000, c(2, 3, 5))
#' test2 <- gen.randT(1000, c(2, 4, 8, 3), info = 2, seed = 42)
gen.randT <- function(nSim, modes, info = NA) {
  #  discrete triangle distribution random numbers
  library(triangle)
  rTriNums <- function(num, min, max, mode) {
    temp <- round(rtriangle(num, min, max, mode))
    return(temp)
  }
  
  # default initializer
  mins <- ifelse((modes - 3) > 0, (modes - 3), 1)
  maxs <- ifelse((modes + 4) > (2 * modes), (modes + 2), (modes + 4))
  
  rng <- matrix(0, nrow = nSim, ncol = length(modes))
  for (i in 1:length(modes)) {
    if (!is.na(modes[i])) {
      rng[ ,i] <- rTriNums(nSim, mins[i], maxs[i], modes[i])
    } else {
      rng[ ,i] <- rep(0, nSim)
    }
  }
  
  if (!is.na(info)) {
    rng <- cbind(rng, rbinom(nSim, 1, (1/info)))
  } else {
    rng <- cbind(rng, rep(0, nSim))
  }
  
  return(rng)
}