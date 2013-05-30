library(ggplot2)

save.pdf <- function(l.plots) {
  f.name <- names(l.plots)
  for (i in 1:length(l.plots)) {
    temp.list <- l.plots[[i]]
    pdf(paste0("C:/Users/a421356/R-GitHub/SCMsim/Output/Graphics/", f.name[i], ".pdf"), 
        width = 11, height = 8.5, onefile = TRUE, title = f.name[i])
    for (j in 1:temp.list) {
      print(temp.list[[j]])
    }
    dev.off()
  }
}

save.png <- function(l.plots) {
  f.name <- names(l.plots) 
  for (i in 1:length(l.plots)) {
    temp.list <- l.plots[[i]]
    p.names <- names(temp.list)
    for (j in 1:length(temp.list)) {
      png(paste0("C:/Users/a421356/R-GitHub/SCMsim/Output/Graphics/", f.name[i], " ", p.names, 
                 ".png"), width = 11, height = 8.5, onefile = TRUE, title = f.name[i])
      print(temp.list[[j]])
      dev.off()
    }
  }
}

options(scipen = 10)

### if need be
# setwd("C:/Users/a421356/R-GitHub/SCMsim/Output")

data <- read.csv("TEST - quant 0.95.csv")
temp <- colnames(data)
temp[1] <- "date"
colnames(data) <- temp
data$date[nSim:length(data$date)] <- ((data$date[nSim:length(data$date)] %% nSim) + 1)
rm(temp)

f.name <- as.character(unique(data$factory))
plots <- vector("list", length = length(f.name))
names(plots) <- f.name

for (i in 1:length(f.name)) {
  whole.sim <- data[(data$factory == f.name[i]), 1:8]
  disrupt <- whole.sim[200:400, ]
  
  temp.list <- vector("list", length = 5)
  names(temp.list) <- c("daily.inv", "in.transit.inv", "disruption", "fcst.err", "act.err")
  
  temp.list[[1]] <- dat.ss + stat_smooth(mapping = aes(x = date, y = daily_inv), 
                                         geom = "line", col = "red") + 
    geom_point(aes(x = date, y = daily_inv)) + 
    labs(list(title = paste("Daily Inventory -", f.name[i]), 
              x = "Simulation Days", y = "Daily Inventory (kgs.)" ))
  
  temp.list[[2]] <- dat.ss + stat_smooth(mapping = aes(x = date, y = in_transit), 
                                         geom = "line", col = "red") + 
    geom_point(aes(x = date, y = in_transit)) + 
    labs(list(title = paste("In Transit Inventory -", f.name[i]), 
              x = "Simulation Days", y = "In Transit Inventory (kgs.)" ))
  
  temp.list[[3]] <- dat.ss + stat_smooth(mapping = aes(x = date, y = forecast_err), 
                                         geom = "line", col = "red") + 
    geom_point(aes(x = date, y = forecast_err)) + 
    labs(list(title = paste("Forecast Error -", f.name[i]), 
              x = "Simulation Days", y = "Forecast Error (kgs.)" ))
  
  temp.list[[4]] <- dat.ss + geom_point(aes(x = date, y = forecast_err), col = "red") + 
    labs(list(title = paste("Daily Inventory & Forecast Error -", f.name[i]), 
              x = "Simulation Days", y = "Material (kgs.)" )) + 
    geom_point(aes(x = date, y = daily_inv), col = "black") +
    stat_smooth(mapping = aes(x = date, y = daily_inv), geom = "line", col = "red") + 
    stat_smooth(mapping = aes(x = date, y = forecast_err), geom = "line", col = "black")
  
  temp.list[[5]] <- dat.ss + geom_histogram(mapping = aes(x = forecast_err, y = ..density..)) + 
    geom_density(aes(x = forecast_err, y = ..density..), col = "red")
  
  plots[[i]] <- temp.list
}

save.pdf(plots)
save.png(plots)



