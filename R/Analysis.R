library(ggplot2)

save.pdf <- function(l.plots) {
  f.name <- names(l.plots)
  for (i in 1:length(l.plots)) {
    temp.list <- l.plots[[i]]
    pdf(paste0("C:/Users/a421356/R-GitHub/SCMsim/Output/Graphics/", f.name[i], ".pdf"), 
        width = 11, height = 8.5, onefile = TRUE, title = f.name[i])
    for (j in 1:length(temp.list)) {
      print(temp.list[[j]])
    }
    dev.off()
  }
}

save.jpeg <- function(l.plots) {
  f.name <- names(l.plots) 
  for (i in 1:length(l.plots)) {
    temp.list <- l.plots[[i]]
    p.names <- names(temp.list)
    for (j in 1:length(temp.list)) {
      jpeg(paste0("C:/Users/a421356/R-GitHub/SCMsim/Output/Graphics/", f.name[i], " ", p.names, 
                 ".jpg"), width = 1920, height = 1080, units = "px")
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
  
  tw <- ggplot(whole.sim)
  td <- ggplot(disrupt)
  
  temp.list <- vector("list", length = 6)
  names(temp.list) <- c("daily_inv", "in_transit_inv", "disruption", "fcst_err", "act_err", "err_density")
  
  temp.list[[1]] <- tw + stat_smooth(mapping = aes(x = date, y = daily_inv), 
                                         geom = "line", col = "red") + 
    geom_point(aes(x = date, y = daily_inv)) + 
    labs(list(title = paste("Daily Inventory -", f.name[i]), 
              x = "Simulation Days", y = "Daily Inventory (kgs.)" ))
  
  temp.list[[2]] <- tw + stat_smooth(mapping = aes(x = date, y = in_transit), 
                                         geom = "line", col = "red") + 
    geom_point(aes(x = date, y = in_transit)) + 
    labs(list(title = paste("In Transit Inventory -", f.name[i]), 
              x = "Simulation Days", y = "In Transit Inventory (kgs.)" ))
  
  temp.list[[3]] <- td + stat_smooth(mapping = aes(x = date, y = daily_inv), 
                                     geom = "line", col = "red") + 
    geom_point(aes(x = date, y = daily_inv)) + 
    labs(list(title = paste("Disruption Daily Inventory -", f.name[i]), 
              x = "Simulation Days", y = "Daily Inventory (kgs.)" ))
  
  temp.list[[4]] <- tw + stat_smooth(mapping = aes(x = date, y = forecast_err), 
                                         geom = "line", col = "red") + 
    geom_point(aes(x = date, y = forecast_err)) + 
    labs(list(title = paste("Forecast Error -", f.name[i]), 
              x = "Simulation Days", y = "Forecast Error (kgs.)" ))
  
  temp.list[[5]] <- tw + geom_point(aes(x = date, y = forecast_err), col = "red") + 
    labs(list(title = paste("Daily Inventory & Forecast Error -", f.name[i]), 
              x = "Simulation Days", y = "Material (kgs.)" )) + 
    geom_point(aes(x = date, y = daily_inv), col = "black") +
    stat_smooth(mapping = aes(x = date, y = daily_inv), geom = "line", col = "red") + 
    stat_smooth(mapping = aes(x = date, y = forecast_err), geom = "line", col = "black")
  
  temp.list[[6]] <- tw + geom_histogram(mapping = aes(x = forecast_err, y = ..density..)) + 
    geom_density(aes(x = forecast_err, y = ..density..), col = "red")
  
  plots[[i]] <- temp.list
}

save.pdf(plots)
# save.jpeg(plots)



