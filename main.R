# main function

source("area_y_log.r")
source("solve_x_log.r")
source("area_xy_log.r")

table = xmax = xmin = area1 = area2 = x = area31 = area32 <- rep(NA, 56)

for(i in 1:56){ 
  data <- read.csv(paste(i, ".csv", sep=""), header=T)
  data$control <- apply(data[,-1], 1, mean)
  data <- data[order(data$drug),]
  table[i] <- i
  xmax[i] <- max(data$drug)
  xmin[i] <- min(data$drug)
  area1[i] <- area_y_log(0, data)
  area2[i] <- area_y_log(-200, data)
  x[i] <- solve_x_log(50, data)
  area31[i] <- area_xy_log(x=0.1, y=0, data)
  area32[i] <- area_xy_log(x=0.1, y=-200, data)
}  
  
summarys <- as.data.frame(cbind(table, xmin, xmax, area1, area2, x, area31, area32))


colnames(summarys) <- c("table",
                        "drug_min",
                        "drug_max",
                        "area1", #y=0, log(drug)
                        "area2", #y< ymin
                        "drug_y50", #the first x coordinate when y=50
                        "area31", #x=0.1, y=0, log(drug)
                        "area32") #x=0.1, y<min

name <- read.csv("name.csv")[,1:2]
Summary <- merge(name, summarys, by="table")
write.csv(Summary, "Summary.csv") 
