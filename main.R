# main function
source("area_y_log.r")
source("area_y.r") 
source("solve_x.r")
source("area_xy.r")
source("area_xy_log.r")

xmax <- max(data$drug)
xmin <- min(data$drug)
y2 <- floor(min(data$control)/10)*10

area1 <- area_y_log(0, data)
area2 <- area_y(y2, data)
x <- solve_x(50, data)
area31 <- area_xy_log(x=0.1, y=0, data)
area32 <- area_xy(x=0.1, y=y2, data)

summarys <- as.data.frame(cbind(xmin, xmax, y2, area1, area2, x, area31, area32))
colnames(summarys) <- c("drug_min",
                        "drug_max",
                        "chosen_y",
                        "area1", #y=0, log(drug)
                        "area2", #y< ymin
                        "drug_y50", #the first x coordinate when y=50
                        "area31", #x=0.1, y=0, log(drug)
                        "area32") #x=0.1, y<min

#example
##using table1
head(summarys)
#  drug_min drug_max chosen_y    area1    area2  drug_y50   area31   area32
#1     0.01       10       20 52.43813 252.2282 0.2526793 33.35193 235.9167
