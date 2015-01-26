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



area_xy <- function(x, y, X){
    X <- X[,c("control","drug")]
    if (y>= max(X$control))
    stop(paste("The maximum value of % control is", max(X$control)))
    if (x<min(X$drug))
    stop(paste("The minimum value of the drug concentrates is", min(X$drug)))
    else if (x>= max(X$drug))
    stop(paste("The maximum value of the drug concentrates is", max(X$drug)))
    
    #calculate the coordinates of the intersection of y and the polylines. Denote it as (a,b).
    a = b <- rep(NA, 7)
    for(m in 1:6){
        if ((y<=X$control[m] & y>X$control[m+1])|(y>X$control[m] & y<=X$control[m+1])){
            a[m] <- X$drug[m]+((X$drug[m+1]-X$drug[m])*(y-X$control[m]))/(X$control[m+1]-X$control[m])
            b[m] <- y
        }
    }
   c = d <- rep(NA, 7)
   for(n in 1:6){
     if (x>=X$drug[n] & x<X$drug[n+1]){
       c[n] <- x
       d[n] <- X$control[n]+((x-X$drug[n])*(X$control[n+1]-X$control[n]))/(X$drug[n+1]-X$drug[n])
     }
   }
  
  X2 <- as.data.frame(rbind(cbind(b,a),cbind(d,c)))
  X2 <- X2[complete.cases(X2),]
  colnames(X2) <- c("control", "drug")
  X <- rbind(X,X2)
  X <- X[order(X$drug),]
  
  area <- 0
  for (n in 1:(length(X$drug)-1)){
    if (X$drug[n]>x){
      area1 <- 0.5*(X$drug[n+1]-X$drug[n])*(X$control[n+1]+X$control[n]-2*y)
      if (area1>0)
        area <- area1 +area
    } 
  }
  return(area)
}

area1 <- area_xy(0.055, 40, data); area1
#[1] 52.43813
