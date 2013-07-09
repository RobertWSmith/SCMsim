library(ggplot2)
library(scales)
library(triangle)
library(knitr)

sample.random.walk <- function() {
  desc.data <- read.csv("C:/Users/a421356/R-GitHub/SCMsim/data/RF-BUD1207.csv")
  
  #### simulated random walk ####
  
  set.seed(42)
  sim.date <- 1:100
  
  akron.desc <- as.numeric(subset(desc.data, DESTINATION == "Lawton", select = c(10:11)))
  
  am.rw <- rnorm(max(sim.date), 0 , akron.desc[2])
  am.rw[1] <- akron.desc[1]
  am.rw <- abs(cumsum(am.rw))
  am.rw.est <- abs(am.rw + (rnorm(max(sim.date), 0 , (akron.desc[2]))))
  
  akron <- data.frame(
    date = sim.date,
    act = am.rw, 
    fcst = am.rw.est
  )
  
  akron <- with(
    akron, 
    data.frame(time = date,
               value = c(act, fcst),
               variable = factor(rep(c("Actual", "Forecast"), each = nrow(akron)))
    ))
  
  BRZ.example <- ggplot(akron, aes(x = time, y = value, colour = variable))
  
  print(
    BRZ.example + geom_line() + 
      labs(list(title ="Simulated Daily Demand Random Walk - Lawton Factory", 
                x = "Simulation Days", y = "Daily Demand (kg)")) +
      scale_y_continuous(labels = comma)
  )
}

sample.transit.time <- function() {
  desc.data <- read.csv("C:/Users/a421356/R-GitHub/SCMsim/data/RF-BUD1207.csv")
  
  americana.modes <- as.numeric(subset(desc.data, DESTINATION == "Lawton", select = c(4:9)))
  americana.trans <- apply(gen.randT(1000, americana.modes, NA), 1, sum)
  americana <- ggplot(as.data.frame(cbind(
    date = 1:1000, transit_time = americana.trans
  )), aes(x = transit_time, y = ..density..))
  
  print(
    americana + geom_histogram(binwidth = 1) + 
      #       stat_density(geom = "line", col = "orange") + 
      labs(list(title ="Simulated Transit Times - Lawton Factory", 
                x = "Transit Time (Days)", y = "Probability Density"))
  )
}

sample.output.graphics <- function(graph, rating) {
  stopifnot(is.numeric(graph))
  stopifnot(graph >=1 | graph <= 3)
  input.data <- read.csv("C:/Users/a421356/R-GitHub/SCMsim/data/BUD1207.csv")
  factory.input <- as.numeric(subset(input.data, DESTINATION == "Pulandian", select = c(12:13)))
  
  if (rating == "good") {
    input <- "C:/Users/a421356/Documents/Operations/Raw Material/Simulation Output/STRAT PW2RWS OP6 DAYS1500 DISR1 LEN0 SL95/trunc_output.csv"
  } else if (rating == "poor") {
    input <- "C:/Users/a421356/Documents/Operations/Raw Material/Simulation Output/STRAT PW2RWS OP6 DAYS1500 DISR1 LEN0 SL1/trunc_output.csv"
  }
  
  desc.data <- read.csv(input)
  desc.data <- as.data.frame(
    cbind(
      desc.data,
      total_inv = desc.data$daily_inv + desc.data$in_transit
    ))
  factory.data <- subset(desc.data, factory == "Pulandian")
  
  factory.actuals <- subset(factory.data, factory_op == TRUE, select = c(1,4:6,18:25))
  plot.data <- ggplot(factory.actuals)
  
  if (graph == 1) {
    print(
      plot.data + geom_point(aes(x = date, y = daily_inv)) + 
        stat_smooth(aes(x = date, y = daily_inv), data = factory.data, geom = "line", col = "orange") + 
        geom_hline(yintercept = factory.input[1], linetype = "dashed", col = "blue") + 
        labs(list(title = paste("Daily On-Hand Inventory -", "Pulandian"), 
                  x = "Simulation Days", y = "Daily On-Hand Inventory (kg)")) +
        scale_y_continuous(labels = comma, limits = c(0, 110000)) + 
        scale_x_continuous(labels = comma, limits = c(1000, 1100))
    )
  } else if (graph == 2) {
    print(
      plot.data + geom_point(aes(x = date, y = in_transit)) + 
        stat_smooth(aes(x = date, y = in_transit), data = factory.data, geom = "line", col = "orange") + 
        geom_hline(yintercept = factory.input[2], linetype = "dashed", col = "blue") + 
        labs(list(title = paste("Daily In-Transit Inventory -", "Pulandian"), 
                  x = "Simulation Days", y = "Daily In-Transit Inventory (kg)")) +
        scale_y_continuous(labels = comma, limits = c(0, 215000)) + 
        scale_x_continuous(labels = comma, limits = c(1000, 1100))
    )
    
  } else if (graph == 3) {
    total_input <- sum(factory.input)
    
    print(
      plot.data + geom_point(aes(x = date, y = total_inv)) + 
        stat_smooth(aes(x = date, y = total_inv), data = factory.data, geom = "line", col = "orange") + 
        geom_hline(yintercept = total_input, linetype = "dashed", col = "blue") + 
        labs(list(title = paste("Daily Total FOB Inventory -", "Pulandian"), 
                  x = "Simulation Days", y = "Daily Inventory (kg)")) +
        scale_y_continuous(labels = comma, limits = c(0, 300000)) + 
        scale_x_continuous(labels = comma, limits = c(1000, 1100))
    )
  }
  
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
#' @param sim.days number of rows of random numbers to be returned
#' @param modes vector of modes of different transit steps
#' @param info mean number of days for information cycle
#' @param seed set if seed is desired
#' 
#' @keyword transit
#' @examples
#' test1 <- gen.randT(1000, c(2, 3, 5))
#' test2 <- gen.randT(1000, c(2, 4, 8, 3), info = 2, seed = 42)
gen.randT <- function(sim.days, modes, info = NA) {
  #  discrete triangle distribution random numbers
  library(triangle)
  rTriNums <- function(num, min, max, mode) {
    temp <- round(rtriangle(num, min, max, mode))
    return(temp)
  }
  
  # default initializer
  mins <- ifelse((modes - 3) > 0, (modes - 3), 1)
  maxs <- ifelse((modes + 4) > (2 * modes), (modes + 2), (modes + 4))
  
  rng <- matrix(0, nrow = sim.days, ncol = length(modes))
  for (i in 1:length(modes)) {
    if (!is.na(modes[i])) {
      rng[ ,i] <- rTriNums(sim.days, mins[i], maxs[i], modes[i])
    } else {
      rng[ ,i] <- rep(0, sim.days)
    }
  }
  
  if (!is.na(info)) {
    rng <- cbind(rng, rbinom(sim.days, 1, (1/info)))
  } else {
    rng <- cbind(rng, rep(0, sim.days))
  }
  
  return(rng)
}



