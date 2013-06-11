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

### TRANSIT REFERENCE CLASS
trans <- setRefClass(
  "transit",
  fields = list(
    volume = "numeric",
    rel.dt = "numeric",
    delivered = "logical",
    in.trns = "numeric",
    ship.size = "numeric",
    transit.time = "matrix"
  )
)

### TRANSIT REFERENCE METHODS
trans$methods(
  first = function(vol, rel, del, rng, shp) {
    volume <<- vol
    rel.dt <<- rel
    delivered <<- del
    in.trns <<- rep(0, length = length(vol))
    ship.size <<- rep(shp, length = length(vol))
    transit.time <<- rng
  },
  getShipmentSize = function() {
    return(ship.size[1])
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
  getTransR = function(time, range) {
    start <- ifelse((time - range) < 1, 1, (time - range + 1))
    return(transit.time[start:time, ])
  },
  getTrans = function(time) {
    return(transit.time[time, ])
  },
  randTrans = function() {
    return(transit.time[sample(nrow(transit.time), 1), ])
  },
  outbound = function(time, vol) {
    setVolume(time, vol)
    setReleaseDate(time, (sum(randTrans()) + time))
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
    IT <- calcITVolume(time)
    
    in.trns[time] <<- sum(IT$vol)
  },
  getITVolume = function(time) {
    return(in.trns[time])
  },
  calcITVolume = function(time) {
    r <- getReleaseDate()
    v <- getVolume()
    
    dL <- !(getDelivered())
    rL <- (r > time)
    vL <- (v > 0)
    
    vol <- rep(0, 50)
    rel <- vol
    
    k <- 0
    for (i in 1:time) {
      if (vL[i] & rL[i] & dL[i]) {
        k <- k + 1
        vol[k] <- v[i]
        rel[k] <- (r[i] + sample(-3:3, 1))
      }
    }
    vol <- vol[1:k]
    rel <- rel[1:k]
    
    rt <- as.data.frame(cbind(vol, rel))
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
gen.trans <- function(nSim, modes, shpSz, info = NA) {
  vl <- rep(0, length = nSim)
  rl <- rep((nSim + 1), nSim)
  dl <- rep(FALSE, nSim)
  tt <- gen.randT(nSim, modes, info)
  
  trns <- trans$new()
  trns$first(vl, rl, dl, tt, shpSz)
  
  return(trns)
}


