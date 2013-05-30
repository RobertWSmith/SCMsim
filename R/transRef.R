#### Transit Lane Reference Class

library(triangle)

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
gen.randT <- function(nSim, modes, info = NA, seed = NA) {
  #  discrete triangle distribution random numbers
  rTriNums <- function(num, min, max, mode) {
    temp <- round(rtriangle(num, min, max, mode))
    return(temp)
  }
    
  # default initializer
  mins <- ifelse((modes - 3) > 0, (modes - 3), 1)
  maxs <- ifelse((modes + 4) > (2 * modes), (modes + 2), (modes + 4))
  
  # I don't want to set the seed on each iteration of rTriNums
  # This allows it to be optional
  if (!is.na(seed)) set.seed(seed)
  
  rng <- matrix(0, nrow = nSim, ncol = length(modes))
  for (i in 1:length(modes)) {
    if (!is.na(modes[i])) {
      rng[ ,i] <- rTriNums(nSim, mins[i], maxs[i], modes[i])
    } else {
      rng[ ,i] <- rep(0, nSim)
    }
  }
  
  if (!is.na(info)) rng <- cbind(rng, rbinom(nSim, 1, (1/info)))
  else  rng <- rng

  return(rng)
}

### TRANSIT REFERENCE CLASS
trans <- setRefClass(
  "transit",
  fields = list(
    volume = "numeric",
    rel.dt = "numeric",
    delivered = "logical",
    in.trns = "numeric",
    disruption = "logical",
    transit.time = "matrix"
    )
  )

### TRANSIT REFERENCE METHODS
trans$methods(
  first = function(vol, rel, del, rng, dis) {
    volume <<- vol
    rel.dt <<- rel
    delivered <<- del
    in.trns <<- rep(0, length = length(vol))
    transit.time <<- rng
    disruption <<- dis
  },
  getDisruption = function(time) {
    return(disruption[time])
  },
  setVolume = function(time, vol) {
    volume[time] <<- vol
  },
  getVolume = function() {
    return(volume)
  },
  setReleaseDate = function(time, rel) {
    rel.dt[time] <<- rel
  },
  getReleaseDate = function() {
    return(rel.dt) 
  },
  setDelivered = function(time) {
    delivered[time] <<- TRUE
  },
  getDelivered = function() {
    return(delivered)
  },
  randTrans = function() {
    return(transit.time[sample(nrow(transit.time), 1), ])
  },
  outbound = function(time, vol) {
    if (!(getDisruption(time))) {
      setVolume(time, vol)
      setReleaseDate(time, (sum(randTrans()) + time))
    }
  },
  inbound = function(time) {
    start <- ifelse((time - 21) < 1, 1, (time - 21))
    v <- getVolume()
    vL <- (v > 0)
    rL <- (getReleaseDate() <= time)
    dL <- !(getDelivered())
    rt <- 0
    
    for (i in 1:time) {
      if (vL[i] & rL[i] & dL[i]) {
        rt <- rt + v[i]
        setDelivered(i)
      }
    }
    return(rt)
  },
  setITVolume = function(time) {
    in.trns[time] <<- calcITVolume(time)
  },
  getITVolume = function(time) {
    return(in.trns[time])
  },
  calcITVolume = function(time) {
    dL <- !(getDelivered())
    rL <- (getReleaseDate() > time)
    v <- getVolume()
    vL <- (v > 0)
    rt <- 0
    
    for (i in 1:time) {
      if (vL[i] & rL[i] & dL[i]) {
        rt <- rt + v[i]
      }
    }
    return(rt)
  }
  )


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
gen.trans <- function(nSim, modes, info = NA, dis = NA) {

  vl <- rep(0, length = nSim)
  rl <- rep((nSim + 1), nSim)
  dl <- rep(FALSE, nSim)
  tt <- gen.randT(nSim, modes, info)
  
  dd <- rep(FALSE, length = nSim)
  if (!(is.na(dis))) {
    for (i in 250:(250 + dis - 1)) {
      dd[i] <- TRUE
    }
  }
  
  trns <- trans$new()
  trns$first(vl, rl, dl, tt,  dd)
  
  return(trns)
}




