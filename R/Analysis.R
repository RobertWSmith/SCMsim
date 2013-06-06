library(ggplot2)

options(scipen = 10)

#### USE THIS FOR FINDING THE CORRECT DIRECTORY FOR BATCH GRAPHICS ####
# (AVAIL.DIRS <- list.dirs())
# DATA.DIR <- vector("character", length(AVAIL.DIRS))
# cnt <- 1
# for (i in 1:length(AVAIL.DIRS)) {
#   if (substr(AVAIL.DIRS[i], 3,4) == "OP")  {
#     DATA.DIR[cnt] <- AVAIL.DIRS[i]
#     cnt <- cnt + 1
#   }
# }
# DATA.DIR <- DATA.DIR[1:(cnt - 1)]

#### Sets the output directory to the path returned at the end of script.R ####
data <- read.csv(paste0(SIM.DIR, "/trunc_output.csv")) ### WHOLE SIMULATION

f.name <- as.character(unique(data$factory))
nSim <- nrow(data) / length(f.name)
data$date[nSim:length(data$date)] <- ((data$date[nSim:length(data$date)] %% nSim) + 1)

if (disrupt[2] > 0) {
  zeroes <- matrix(0, nrow = length(f.name), ncol = 3, 
                   dimnames = list(row = f.name, col = c("WholeSim", "Disruption+100", "Proportion")))
} else {
  zeroes <- matrix(0, nrow = length(f.name), ncol = 1, 
                   dimnames = list(row = f.name, col = "WholeSim"))
}



#### loop to create multiple output graphs per simulation run, labeled independently ####
for (i in 1:length(f.name)) {
  whole.sim <- data[(data$factory == f.name[i]), ]
  
  tw <- ggplot(whole.sim)  
  if (disrupt[2] > 0) {
    d.vis <- ((disrupt[1]) - 50):((disrupt[1]) - 50 + 200)
    d.meas <- (disrupt[1]):((disrupt[1]) + 100)
    disrupt.ss <- whole.sim[d.vis, ] # graphing data
    disr.ss <- whole.sim[d.meas, ] # 
    zrs <- c(sum(((whole.sim$daily_inv == 0))), sum((disr.ss$daily_inv == 0)))
    zrs <- c(zrs, (zrs[2] / zrs[1]))
    zeroes[i, ] <- zrs
    
    td <- ggplot(disrupt.ss)
  } else {
    d.vis <- NA
    d.meas <- NA
    disrupt.ss <- NA
    disr.ss <- NA
    zeroes[i,] <- sum(((whole.sim$daily_inv == 0)))
  }
  
  # begin PDF creation
  pdf(paste0(SIM.DIR, "/", f.name[i], ".pdf"), width = 11, height = 8.5, onefile = TRUE, title = f.name[i])
  
  # plot of the whole simulation 
  # x = days, y = daily inventory
  print(tw + geom_point(aes(x = date, y = daily_inv)) + ylim(0, max(whole.sim$daily_inv)) +
          geom_hline(aes(yintercept = DATA[(DATA$DESTINATION == f.name[i]), 12]), linetype = "dashed", col = "blue") +
          stat_smooth(mapping = aes(x = date, y = daily_inv), geom = "line", col = "red") + 
          labs(list(title = paste("Daily Inventory -", f.name[i]), 
                    x = "Simulation Days", y = "Daily Inventory (kgs.)" )))
  
  # plot of the whole simulation 
  # x = days, y = in transit inventory
  print(tw + geom_point(aes(x = date, y = in_transit)) + ylim(0, max(whole.sim$in_transit)) +
          geom_hline(aes(yintercept = DATA[DATA$DESTINATION == f.name[i], 13]), linetype = "dashed", col = "blue")  +
          stat_smooth(mapping = aes(x = date, y = in_transit), geom = "line", col = "red") + 
          labs(list(title = paste("In Transit Inventory -", f.name[i]), 
                    x = "Simulation Days", y = "In Transit Inventory (kgs.)" )))
  
  if (disrupt[2] > 0) {
    # plot of the disruption range (+50 days before and +50 after) simulation 
    # x = days, y = daily inventory
    print(td + geom_line(aes(x = date, y = daily_inv)) + ylim(0, max(disrupt.ss$daily_inv)) + xlim(min(d.vis), max(d.vis)) + 
            geom_hline(aes(yintercept = DATA[DATA$DESTINATION == f.name[i], 13]), linetype = "dashed", col = "blue") +
            geom_vline(aes(xintercept = c(min(d.meas), (min(d.meas)+21)), linetype = "dotted", col = "purple")) +
            stat_smooth(mapping = aes(x = date, y = daily_inv), data = whole.sim, geom = "line", col = "red") + 
            labs(list(title = paste("Disruption Daily Inventory -", f.name[i]), 
                      x = "Simulation Days", y = "Daily Inventory (kgs.)" )))
  }
  
  # plot of the whole simulation
  # x = days, y = forecast error 
  # (error > 0 = forecast underestimate, error < 0 forecast overestimate)
  print(tw + geom_point(aes(x = date, y = forecast_err)) + 
          stat_smooth(mapping = aes(x = date, y = forecast_err), geom = "line", col = "red") + 
          labs(list(title = paste("Forecast Error -", f.name[i]), 
                    x = "Simulation Days", y = "Forecast Error (kgs.)" )))
  
  # plot of the whole simulation
  # x = days, y1 = forecast error, y2 = actual daily inventory
  # (error > 0 = forecast underestimate, error < 0 forecast overestimate)
  # also confirming that the error is small, and comparing large error outliers to the inventory
  print(tw + geom_line(aes(x = date, y = forecast_err), col = "red") + 
          labs(list(title = paste("Daily Inventory & Forecast Error -", f.name[i]), 
                    x = "Simulation Days", y = "Material (kgs.)" )) + 
          geom_point(aes(x = date, y = daily_inv), col = "black") +
          stat_smooth(mapping = aes(x = date, y = daily_inv), geom = "line", col = "red") + 
          stat_smooth(mapping = aes(x = date, y = forecast_err), geom = "line", col = "black"))
  
  # histogram confirming that errors are normally distributed 
  #   (but zero-inflated due to off days)
  print(tw + geom_histogram(mapping = aes(x = forecast_err, y = ..density..)) + 
          geom_density(aes(x = forecast_err, y = ..density..), col = "red") +
          labs(list(title = paste("Histogram of Forecast Error -", f.name[i]), 
                    x = "Forecast Error (kgs.)", y = "Probability" )))
    
  
  # histogram of daily inventory
  print(tw + geom_histogram(mapping = aes(x = daily_inv, y = ..density..)) + 
          geom_density(aes(x = daily_inv, y = ..density..), col = "red") +
          labs(list(title = paste("Histogram of Daily Inventory -", f.name[i]), 
                    x = "Daily Inventory (kgs.)", y = "Probability" )))
  
  # histogram of in transit inventory
  print(tw + geom_histogram(mapping = aes(x = in_transit, y = ..density..)) + 
          geom_density(aes(x = daily_inv, y = ..density..), col = "red") +
    labs(list(title = paste("Histogram of In Transit Inventory -", f.name[i]), 
              x = "In Transit Inventory (kgs.)", y = "Probability" )))

# end PDF creation
dev.off()
}

#### write the table of values for days with zero inventory, starting point ####
write.csv(zeroes, file.path(SIM.DIR, "ZeroInventoryTable.csv"), quote = FALSE)


