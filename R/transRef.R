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
    
    in.trns[time] <<- sum(IT)
  },
  getITVolume = function(time) {
    return(in.trns[time])
  },
  calcITVolume = function(time) {
    v <- getVolume()
    
    dL <- !(getDelivered())
    rL <- (getReleaseDate() > time)
    vL <- (v > 0)
    
    vol <- rep(0, 50)
    
    k <- 0
    for (i in 1:time) {
      if (vL[i] & rL[i] & dL[i]) {
        k <- k + 1
        vol[k] <- v[i]
      }
    }
    
    return(vol[1:k])
  }
)
