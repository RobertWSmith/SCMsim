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
