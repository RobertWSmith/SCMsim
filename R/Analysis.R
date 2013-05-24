library(ggplot2)

options(digits = 2)
setwd("C:/Users/a421356/R-GitHub/SCM_Sim/Output")
data <- read.csv("test 4 Americana.csv")

days <- data[ ,1]
inventory <- data[ ,2]



plot(x = days, y = inventory, type = 'b')
abline(h = mean(inventory), col = "green")
abline(h = min(inventory[100:1000]), col = "red")
abline(h = 992750)

# plt <- ggplot(aes(x = days, y = inventory))

