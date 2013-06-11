

#' Function to perform specified analysis on the data
#' 
#' @param nSim the number of simulation days in one run
#' @param OUTPUT.DIR the file path to the directory holding the output CSV
#' @param OUTPUT.NAME the name of the output CSV
#' @param ORIG.DATA the name of the original data as inputted
#' @param disrupt either FALSE or a vector with the start date and length of disruption
analysis <- function(nSim, OUTPUT.DIR, OUTPUT.NAME, ORIG.DATA, disrupt = FALSE) {
  library(ggplot2)
  options(scipen = 10)
  
  output.file <- file.path(OUTPUT.DIR, OUTPUT.NAME)
  sim.data <- read.csv(output.file)
  
  fac.name <- as.character(ORIG.DATA$DESTINATION)
  #   HL1 <- as.numeric(ORIG.DATA$Facility.Inventory)
  #   HL2 <- as.numeric(ORIG.DATA$In.transit.Inventory)
  
  if (is.numeric(disrupt)) {
    zeroes <- matrix(0, nrow = length(fac.name), ncol = 3, 
                     dimnames = list(row = fac.name, col = c("WholeSim", "Disruption+100", "Proportion")))
  } else {
    zeroes <- matrix(0, nrow = length(fac.name), ncol = 1, dimnames = list(row = fac.name, col = "WholeSim"))
  }
  
  for (i in 1:length(fac.name)) {
    whole.sim <- subset(sim.data, factory == fac.name[i])
    
    tw <- ggplot(whole.sim)
    
    if (is.numeric(disrupt)) {
      d.vis <- ((disrupt[1]) - 50):((disrupt[1]) - 50 + 200)
      d.meas <- (disrupt[1]):((disrupt[1]) + 100)
      disrupt.ss <- whole.sim[d.vis, ]
      disr.ss <- whole.sim[d.vis, ]
      zrs <- c(sum(((whole.sim$daily_inv == 0))), sum((disr.ss$daily_inv == 0)))
      zrs <- c(zrs, (zrs[2] / zrs[1]))
      zeroes[i, ] <- zrs
      
      # shows disruption begin
      VL1 <- disrupt[1]
      # shows disruption end
      VL2 <- (disrupt[1] + disrupt[2]- 1)
      
      td <- ggplot(disrupt.ss)
    } else {
      d.vis <- NA
      d.meas <- NA
      disrupt.ss <- NA
      disr.ss <- NA
      zeroes[i,] <- sum(((whole.sim$daily_inv == 0)))
    }
    
    # begin PDF creation
    pdf(paste0(OUTPUT.DIR, "/", fac.name[i], ".pdf"), width = 11, height = 8.5, onefile = TRUE, title = fac.name[i])
    print(
      tw + geom_point(aes(x = date, y = daily_inv)) + 
        #         geom_hline(aes(yintercept = HL1[i]), col = "blue") +
        stat_smooth(aes(x = date, y = daily_inv), geom = "line", col = "red") + 
        labs(list(title = paste("Daily Inventory -", fac.name[i]), 
                  x = "Simulation Days", y = "Daily Inventory (kgs.)"))
    )
    
    print(
      tw + geom_point(aes(x = date, y = in_transit)) + 
        #         geom_hline(aes(yintercept = HL2[i]), col = "blue") +
        stat_smooth(aes(x = date, y = in_transit), geom = "line", col = "red") + 
        labs(list(title = paste("In Transit Inventory -", fac.name[i]), 
                  x = "Simulation Days", y = "In Transit Inventory (kgs.)"))
    )
    
    if (is.numeric(disrupt)) {
      print(
        td + geom_line(aes(x = date, y = daily_inv)) + xlim(min(d.vis), max(d.vis)) + 
          #           geom_hline(aes(yintercept = HL1[i]), col = "blue") + 
          geom_vline(aes(xintercept = min(d.meas)), col = "purple") +
          geom_vline(aes(xintercept = max(d.meas)), col = "purple") + 
          stat_smooth(aes(x = date, y = daily_inv), data = whole.sim, geom = "line", col = "red") +
          labs(list(title = paste("Disruption Daily Inventory -", fac.name[i]), 
                    x = "Simulation Days", y = "Daily Inventory (kgs.)"))
      )
    }
    
    print(
      tw + geom_point(aes(x = date, y = forecast_err)) + 
        stat_smooth(aes(x = date, y = forecast_err), geom = "line", col = "red") + 
        labs(list(title = paste("Forecast Error -", fac.name[i]), 
                  x = "Simulation Days", y = "Forecast Error (kgs.)"))
    )
    
    print(
      tw + geom_line(aes(x = date, y = forecast_err), col = "red") + 
        geom_point(aes(x = date, y = daily_inv)) +
        stat_smooth(aes(x = date, y = daily_inv), geom = "line", col = "red") + 
        stat_smooth(aes(x = date, y = forecast_err), geom = "line") +
        labs(list(title = "Forecast Error & Daily Inventory -", fac.name[i]), 
             x = "Simulation days", y = "Kilograms")
    )
    
    print(
      tw + geom_histogram(aes(x = forecast_err, y = ..density..)) + 
        geom_density(aes(x = forecast_err, y = ..density..), col = "red") +
        labs(list(title = "Histogram of Forecast Error -", fac.name[i]), 
             x = "Forecast Error (kgs.)", y = "Probability")
    )
    
    print(
      tw + geom_histogram(aes(x = daily_inv, y = ..density..)) + 
        geom_density(aes(x = daily_inv, y = ..density..), col = "red") + 
        labs(list(title = paste("Histogram of Daily Inventory -", fac.name[i]), 
                  x = "Daily Inventory (kgs.)", y = "Probability"))
    )
    
    print(
      tw + geom_histogram(aes(x = in_transit, y = ..density..)) + 
        geom_density(aes(x = daily_inv, y = ..density..), col = "red") + 
        labs(list(title = paste("Histogram of In Transit Inventory -", fac.name[i]), 
                  x = "In Transit Inventory (kgs.)", y = "Probability"))
    )
    
    dev.off()
    
  }
  
  
  #### write the table of values for days with zero inventory, starting point ####
  write.csv(zeroes, file.path(OUTPUT.DIR, "ZeroInventoryTable.csv"), quote = FALSE)
}



