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
  first.fa = function(nSim, nm, curr, act, opNdays, ordNdays) {
    nFac <- length(nm)
    factory <<- vector("list", length = nFac)
    
    for (i in 1:nFac) {
      name <- as.character(nm[i])
      current <- as.numeric(curr[i])
      actual <- as.vector(act[i, ])
      factory[[i]] <<- gen.inv(nSim, name, current, actual, opNdays, ordNdays)
    }
  },
  first.ob = function(nSim, modes, info) {
    nFac <- nrow(modes)
    f.trans <<- vector("list", nFac)
    for (i in 1:nFac) {
      f.trans[[i]] <<- gen.trans(nSim, modes[i, ], info[i])
    }
  },
  first.ib = function(nSim, modes, info) {
    h.trans <<- gen.trans(nSim, modes, info)
  },
  first.wh = function(nSim, nm, curr, opNdays, ordNdays) {
    A <- length(getFactory())
    a <- getFactory()
    exp <- matrix(0, nrow = nSim, ncol = A)
    act <- matrix(0, nrow = nSim, ncol = A)
    
    for (i in 1:A) {
      act[,i] <- (a[[i]])$actual
      exp[,i] <- (a[[i]])$expected
    } 
    
    act <- apply(act, 1, sum)
    exp <- apply(exp, 1, sum)
    
    warehouse <<- gen.inv(nSim, nm, curr, act, opNdays, ordNdays)
    warehouse$expected <<- exp
  },
  iterate = function(time) {
    for (fac in 1:length(factory)) {
      (factory[[fac]])$iterate(time)
    }
    warehouuse$iterate(time)
  },
  recieve = function(time) {
    for (fac in 1:length(factory)) {
      (factory[[fac]])$recieve(time, (f.trans[[fac]]))
    }
    warehouse$recieve(time, h.trans)
  },
  order = function(time, quant) {
    for (fac in 1:length(factory)) {
      (factory[[fac]])$order(time, (f.trans[[fac]]), quant)
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
gen.hub <- function(nSim, nm, curr, act, opNdays, ordNdays, modes, info = NA) {
  h.name <- as.character(nm[1])
  f.name <- as.character(nm[-1])
  h.curr <- as.numeric(curr[1])
  f.curr <- as.numeric(curr[-1])
  h.act <- as.numeric(act[1, ])
  f.act <- (act[2:nrow(act), ])
  h.modes <- as.numeric(modes[1, ])
  f.modes <- (modes[2:nrow(modes), ])
  
  h <- HUB$new()
  
  h$first.fa(nSim, f.name, f.curr, f.act, opNdays, ordNdays)
  h$first.ob(nSim, f.modes, info)
  
  h$first.ib(nSim, h.modes, info)
  h$first.wh(nSim, h.name, h.curr, opNdays, ordNdays)
  
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

# 
# hub <- HUB$new()
# hub$first.fa(nSim, nm, curr, act, op, ord)


