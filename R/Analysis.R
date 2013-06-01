library(ggplot2)

options(scipen = 10)

#### Don't use any directory assistance, unless testing ####
### USE THIS FOR ACTIVE DIRECTORY / SIMULATION
# DIR <- getwd()
# setwd(paste0(DIR, "/Output"))
### SIM.DIR should be coming from the 'script.R' which calls this file

# OUTPUT.LOC <- c(getwd(), "TEST - quant 0.95.csv")

#### Sets the output directory to the path returned at the end of script.R ####
setwd((OUTPUT.LOC[1]))
data <- read.csv((OUTPUT.LOC[2]))
colnames(data)[1] <- "date"
nSim <- nrow(data) / length(f.name)
data$date[nSim:length(data$date)] <- ((data$date[nSim:length(data$date)] %% nSim) + 1)
f.name <- as.character(unique(data$factory))

zeroes <- matrix(0, nrow = length(f.name), ncol = 3, dimnames = list(row = f.name, col = c("WholeSim", "Disruption+100", "Proportion")))
row.names(zeroes) <- f.name

#### loop to create multiple output graphs per simulation run, labeled independently ####
for (i in 1:length(f.name)) {
  whole.sim <- data[(data$factory == f.name[i]), ]
  disrupt <- whole.sim[700:900, ] # graphing data
  disr <- whole.sim[750:850, ] # 
  
  zrs <- c(sum(((whole.sim$daily_inv == 0) & (1:nSim >= 100))), sum((disr$daily_inv == 0)))
  zrs <- c(zrs, (zrs[2] / zrs[1]))
  zeroes[i, ] <- zrs
  
  tw <- ggplot(whole.sim)
  td <- ggplot(disrupt)
  
  # begin PDF creation
  pdf(paste0(OUTPUT.LOC[1], "/", f.name[i], ".pdf"), width = 11, height = 8.5, onefile = TRUE, title = f.name[i])
  
  # plot of the whole simulation 
  # x = days, y = daily inventory
  print(tw + geom_line(aes(x = date, y = daily_inv)) + ylim(0, max(whole.sim$daily_inv)) +
          geom_hline(aes(yintercept = DATA[DATA$DESTINATION == f.name[i], 12]), linetype = "dashed") +
          stat_smooth(mapping = aes(x = date, y = daily_inv), geom = "line", col = "red") + 
          labs(list(title = paste("Daily Inventory -", f.name[i]), 
                    x = "Simulation Days", y = "Daily Inventory (kgs.)" )))
  
  # plot of the whole simulation 
  # x = days, y = in transit inventory
  print(tw + geom_point(aes(x = date, y = in_transit)) + ylim(0, max(whole.sim$in_transit)) +
          geom_hline(aes(yintercept = DATA[DATA$DESTINATION == f.name[i], 13]), linetype = "dashed")  +
          stat_smooth(mapping = aes(x = date, y = in_transit), geom = "line", col = "red") + 
          labs(list(title = paste("In Transit Inventory -", f.name[i]), 
                    x = "Simulation Days", y = "In Transit Inventory (kgs.)" )))
  
  # plot of the disruption range (+50 days before and +50 after) simulation 
  # x = days, y = daily inventory
  print(td + geom_line(aes(x = date, y = daily_inv)) + ylim(0, max(disrupt$daily_inv)) + xlim(700, 900) + 
          geom_hline(aes(yintercept = DATA[DATA$DESTINATION == f.name[i], 13]), linetype = "dashed") +
          geom_vline(aes(xintercept = 750), linetype = "dotted") +
          geom_vline(aes(xintercept = (750+21), linetype = "dotted")) +
          stat_smooth(mapping = aes(x = date, y = daily_inv), 
                      data = whole.sim, geom = "line", col = "red") + 
          labs(list(title = paste("Disruption Daily Inventory -", f.name[i]), 
                    x = "Simulation Days", y = "Daily Inventory (kgs.)" )))
  
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
          geom_line(aes(x = date, y = daily_inv), col = "black") +
          stat_smooth(mapping = aes(x = date, y = daily_inv), geom = "line", col = "red") + 
          stat_smooth(mapping = aes(x = date, y = forecast_err), geom = "line", col = "black"))
  
  # histogram confirming that errors are normally distributed 
  #   (but zero-inflated due to off days)
  print(tw + geom_histogram(mapping = aes(x = forecast_err, y = ..density..)) + 
          geom_density(aes(x = forecast_err, y = ..density..), col = "red"))
  
  # end PDF creation
  dev.off()
}

#### write the table of values for days with zero inventory, starting point ####
write.csv(zeroes, "ZeroInventoryTable.csv", quote = FALSE)




