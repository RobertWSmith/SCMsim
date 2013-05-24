#### Transit Hub Reference Class

HUB <- setRefClass(
  "hub",
  fields = c("inbound", "warehouse", "outbound", "factory")
  # inbound = HUB Transit
  # warehouse = HUB Inventory
  # outbound = Child Factory Transit
  # factory = Child Factory Inventory
  )

HUB$methods(
  first.fa = function(nSim, nm, curr, expect, act, opNdays, ordNdays) {
    nFac <- length(nm)
    factory <<- vector("list", nFac)
    
    for (i in 1:nFac) {
      factory[[i]] <<- gen.inv(nSim, nm[i], curr[i], expect[ ,i], act[ ,i], opNdays[i], ordNdays[i])
    }
  },
  first.ob = function(nSim, modes, info) {
    nFac <- length(nm)
    outbound <<- vector("list", nFac)
    for (i in 1:nFac) {
      outbound[[i]] <<- gen.trans(nSim, modes[ ,i], info[i])
    }
  },
  first.ib = function(nSim, modes, info) {
    inbound <<- gen.trans(nSim, modes, info)
  },
  first.wh = function(nSim, nm, curr, expect, act, opNdays, ordNdays) {
    warehouse <<- gen.inv(nSim, nm, curr, expect, act, opNdays, ordNdays)
  },
  getInbound = function() {
    return(inbound)
  },
  getWarehouse = function() {
    return(warehouse)
  },
  getOutbound = function() {
    return(outbound)
  },
  getFactory = function() {
    return(factory)
  }
  )

# everything in gen.inv that is length == 1 is a vector length n
# evrything that is a vector is a matrix, with columns length n, rows length nSim
# HUB DATA IS ROW 1, FACTORY DATA IS ROW 2 THROUGH THE END OF THE MATRIX
gen.hub <- function(nSim, nm, curr, expect, act, opNdays, ordNdays, modes, info = NA) {
  op <- matrix(FALSE, nrow = nSim, ncol = length(opNdays))
  ord <- matrix(FALSE, nrow = nSim, ncol = length(ordNdays))
  a.dmd <- matrix(0, nrow = nSim, ncol = (nrow(act) - 1))
  e.dmd <- matrix(0, nrow = nSim, ncol = (nrow(expect) - 1))
  
  a.mn <- act[ ,1]
  a.sd <- act[ ,2]
  e.mn <- expect[ ,1]
  e.sd <- expect[ ,2]
  
  for (i in 1:ncol(a.dmd)) {
    op[ ,i] <- gen.sched(opNdays[(i+1)], nSim)
    ord[ ,i] <- gen.sched(ordNdays[(i+1)], nSim)
    a.dmd[ ,i] <- sqrt((rnorm(nSim, a.mn[(i+1)], a.sd[(i+1)] ))^2)
    e.dmd[ ,i] <- sqrt((rnorm(nSim, e.mn[(i+1)], e.sd[(i+1)]))^2)
  }
  
  rnorm(nSim, act[(i+1),1], act[(i+1),2])

  h <- HUB$new()
  
  h$frist.ib(nSim, modes[1,], info)
  h$first.wh(nSim, nm[1], curr[1], rep(0, nSim), rep(0, nSim), opNdays[1], ordNdays[1])
  
  h$first.fa(nSim, nm[2:length(nm)], curr[2:length(curr)], expect[, 2:ncol(expect)], act[ ,2:ncol(act)], opNdays[2:length(opNdays)], ordNdays[2:length(ordNdays)])
  h$first.ob(nSim, modes[2:nrow(modes), ], info)
  
  return(h)
}












