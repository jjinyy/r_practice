unzip(zipfile = "./Data/archive.zip", exdir = "./Data")
avocado <- read.csv("D:./Data/avocado.csv", header=TRUE, sep = ",")

library("dplyr")
(x_avg <- avocado %>% group_by(region) %>% summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))
View(x_avg)

(x_avg <- avocado %>% group_by(region, year) %>% summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))
View(x_avg)

(x_avg <- avocado %>% group_by(region, year, type) %>% summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))
View(x_avg)

arrange(x_avg, desc(V_avg))

x_avg1 <- x_avg %>% filter(region != "TotalUS")
x_avg1[x_avg1$V_avg == max(x_avg1$V_avg), ]

install.packages("lubridate")
library(lubridate)

(x_avg = avocado %>% group_by(region, year, month(Date), type) %>% summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))
