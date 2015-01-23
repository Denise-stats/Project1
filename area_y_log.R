## Input:
#       y       -- a numeric value of % control
#       X       -- initialized data
#
## Temple Veriables:
#       a            -- a vector of x coordinates of the intersections of y and the polylines.
#       b            -- a vector of y coordinates of the intersections of y and the polylines.
#       X2           -- the data.frame of a and b.
#
#
#＃ Output:
#       area         -- the area of the graph of curve of y=p and the polylines.
#




area_y_log <- function(y, X){
  X$ldrug <- log(X$drug)
  X <- X[,c("control","ldrug")]

  if (y<min(X$control))
    stop(paste("The minimum value of % control is", min(X$control)))
  else if (y>= max(X$control))
    stop(paste("The maximum value of % control is", max(X$control)))
  
  #calculate the coordinates of the intersection of y and the polylines. Denote it as (a,b).
  a = b <- rep(NA, 7)
  for(m in 1:6){
    if ((y<=X$control[m] & y>X$control[m+1])|(y>X$control[m] & y<=X$control[m+1])){
      a[m] <- X$ldrug[m]+((X$ldrug[m+1]-X$ldrug[m])*(y-X$control[m]))/(X$control[m+1]-X$control[m])
      b[m] <- y
    }
  }
  
  X2 <- as.data.frame(cbind(b,a))
  colnames(X2) <- c("control", "ldrug")
  X2 <- X2[complete.cases(X2),]
  X <- rbind(X,X2)
  X <- X[order(X$ldrug),]
  
  area <- 0
  for (n in 1:(length(X$ldrug)-1)){
    area1 <- 0.5*(X$ldrug[n+1]-X$ldrug[n])*(X$control[n+1]+X$control[n]-2*y)
    if (area1>0)
      area <- area1 +area
  }
  return(area)
}

#examples
area1 <- area_y_log(70, data); area1
#[1] 55.7276