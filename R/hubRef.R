#### Transit Hub Reference Class

HUB <- setRefClass(
  "hub",
  fields = c("h.trans", "warehouse", "f.trans", "factory")
  # inbound = HUB Transit
  # warehouse = HUB Inventory
  # f.trans = Child Factory Transit
  # factory = Child Factory Inventory
)

HUB$methods(
  first.fa = function(nSim, nm, rgn, curr, act, opNdays, ordNdays, bias, amp, STRAT) {
    nFac <- length(nm)
    factory <<- vector("list", length = nFac)
    
    for (i in 1:nFac) {
      name <- as.character(nm[i])
      region <- as.character(rgn[i])
      current <- as.numeric(curr[i])
      actual <- as.numeric(act[i, ])
      ordering <- as.numeric(ordNdays[i])
      
      factory[[i]] <<- gen.inv(nSim, name, region, current, actual, opNdays, ordering, bias, amp, STRAT)
    }
  },
  first.ob = function(nSim, modes, shpSz, info) {
    nFac <- nrow(modes)
    f.trans <<- vector("list", nFac)
    for (i in 1:nFac) {
      f.trans[[i]] <<- gen.trans(nSim, modes[i, ], shpSz[i], info[i])
    }
  },
  first.ib = function(nSim, modes, shpSz, info) {
    h.trans <<- gen.trans(nSim, modes, shpSz, info)
  },
  first.wh = function(nSim, nm, rgn, curr, opNdays, ordNdays, STRAT) {
    f <- getFactory()
    exp <- matrix(0, nrow = nSim, ncol = length(f))
    act <- matrix(0, nrow = nSim, ncol = length(f))
    
    for (i in 1:length(f)) {
      act[,i] <- (f[[i]])$actual
      exp[,i] <- (f[[i]])$expected
    } 
    act <- apply(act, 1, sum)
    exp <- apply(exp, 1, sum)
    
    warehouse <<- gen.inv(nSim, nm, rgn, curr, act, opNdays, as.numeric(ordNdays), 
                          bias = c(0.5, 0.5), amp = 0.25, STRAT)
    warehouse$actual <<- act
    warehouse$expected <<- exp
  },
  iterate = function(time) {
    for (fac in 1:length(factory)) {
      (factory[[fac]])$iterate(time)
    }
    warehouse$iterate(time)
  },
  recieve = function(time) {
    for (fac in 1:length(factory)) {
      (factory[[fac]])$recieve(time, (f.trans[[fac]]))
    }
    warehouse$recieve(time, h.trans)
  },
  order = function(time, quant) {
    for (fac in 1:length(factory)) {
      if (warehouse$getCurrent() > 0) {
        (factory[[fac]])$order(time, (f.trans[[fac]]), quant)
      }
    }
    warehouse$order(time, h.trans, quant)
  },
  getFactory = function() {
    return(factory)
  }
)

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


# nSim <- 25
# nm <- c("test1", "test2", "test3")
# curr <- c(1000, 10000, 100000)
# act <- as.matrix(rbind(c(100, 25), c(1000, 250), c(10000, 2500)))
# op <- 6
# ord <- 1
# modes <- as.matrix(rbind(c(2,4,17,2), c(4,6,2,5), c(2,4,6,4)))
# 
# h <- gen.hub(nSim, nm, curr, act, op, ord, modes)




