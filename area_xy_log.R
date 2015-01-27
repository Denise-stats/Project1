## Input:
#       y       -- a numeric value of % control
#       X       -- initialized data
#
## Temple Veriables:
#       a            -- a vector of x coordinates of the intersections of y and the polylines.
#       b            -- a vector of y coordinates of the intersections of y and the polylines.
#       c            -- a vector of x coordinates of the intersections of x and the polylines.
#       d            -- a vector of y coordinates of the intersections of x and the polylines.
#       X2           -- the data.frame of a and b.
#
#
# Output:
#       area         -- the area of the graph of curve of y=p and the polylines.
#



area_xy_log <- function(x, y, X){
  X$ldrug <- log(X$drug)
  X <- X[,c("control","ldrug")]
  
  #calculate the coordinates of the intersection of y and the polylines. Denote it as (a,b).
  a = b <- rep(NA, 7)
  for(m in 1:6){
    if ((y<=X$control[m] & y>X$control[m+1])|(y>X$control[m] & y<=X$control[m+1])){
      a[m] <- X$ldrug[m]+((X$ldrug[m+1]-X$ldrug[m])*(y-X$control[m]))/(X$control[m+1]-X$control[m])
      b[m] <- y
    }
  }
  c = d <- rep(NA, 7)
  for(n in 1:6){
    if (x>=X$ldrug[n] & x<X$ldrug[n+1]){
      c[n] <- x
      d[n] <- X$control[n]+((x-X$ldrug[n])*(X$control[n+1]-X$control[n]))/(X$ldrug[n+1]-X$ldrug[n])
    }
  }
  
  X2 <- as.data.frame(rbind(cbind(b,a),cbind(d,c)))
  X2 <- X2[complete.cases(X2),]
  colnames(X2) <- c("control", "ldrug")
  X <- rbind(X,X2)
  X <- X[order(X$ldrug),]
  
  area <- 0
  for (n in 1:(length(X$ldrug)-1)){
    if (X$ldrug[n]>x){
      area1 <- 0.5*(X$ldrug[n+1]-X$ldrug[n])*(X$control[n+1]+X$control[n]-2*y)
      if (area1>0)
        area <- area1 +area
    } 
  }
  return(area)
}
