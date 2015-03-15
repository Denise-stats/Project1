setwd("/Users/Denise/Desktop/Research/Data Analysis/DATA2")

# main function

source("solve_x_log.r")
source("area_xy_log.r")
source("area2_x_log.r")

table = xmax = xmin = area1 =area2 =area3 =area4 = d.area= x <- rep(NA, 42)

for(i in 1:42){ 
  data <- read.csv(paste(i, ".csv", sep=""), header=T)
  data$control <- apply(data[,-1], 1, mean)
  data <- data[order(data$drug),]
  table[i] <- i
  xmax[i] <- max(data$drug)
  xmin[i] <- min(data$drug)
  area1[i] <- area_xy_log(x=1, y=100, data)
  area2[i] <- area_xy_log(x=1, y=0, data)
  area3[i] <- area_xy_log(x=1, y=-75, data)
  area4[i] <- area2_x_log(x=1, data)
  x[i] <- solve_x_log(50, data)
  if(area4[i]>area2[i])
    d.area[i] <- area4[i]-area2[i]
  else
    d.area[i] <- 0
}
  
summarys <- as.data.frame(cbind(table, xmin, xmax, area1, area2, area3, d.area, x))


colnames(summarys) <- c("table",
                        "drug_min",
                        "drug_max",
                        "Area1", #x=1, y=100, log(drug)
                        "Area2", #x=1, y=0, log(drug)
                        "Area3", #x=1, y=-75, log(drug)
                        "d.area", #x=1, log(drug)
                        "drug_y50") #the first x coordinate when y=50


name <- read.csv("names.csv")[,1:2]
Summary <- merge(name, summarys, by="table")
write.csv(Summary, "Summary03-15.csv") 
