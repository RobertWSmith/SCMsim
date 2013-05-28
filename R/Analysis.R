library(ggplot2)

options(scipen = 10)
### if need be
setwd("C:/Users/a421356/R-GitHub/SCMsim/Output")

data <- read.csv("TEST - quant 0.95.csv")
temp <- colnames(data)
temp[1] <- "date"
colnames(data) <- temp
data$date[nSim:length(data$date)] <- ((data$date[nSim:length(data$date)] %% nSim) + 1)
rm(temp)

f.name <- as.character(unique(data$factory))
plots <- vector("list", length = length(f.name))

for (i in 1:length(f.name)) {
  dat.ss <- ggplot(data[(data$factory == f.name[i]), 1:8])
  plots[[i]] <- dat.ss
  
  pdf(paste0("C:/Users/a421356/R-GitHub/SCMsim/Output/Graphics/", f.name[i], ".pdf"), 
      width = 11, height = 8.5, onefile = TRUE, title = f.name[i])
  print(dat.ss + geom_line(aes(x = date, y = daily_inv)) + 
          labs(list(title = paste("Daily Inventory -", f.name[i]),
            x = "Simulation nSim", y = "Daily Inventory (kgs.)" )))
  
  dev.off()
}


