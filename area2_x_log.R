## Input:
#       x       -- a numeric value of drug dose
#       X       -- initialized data
#
## Temple Veriables:
#       c            -- a vector of x coordinates of the intersections of x and the polylines.
#       d            -- a vector of y coordinates of the intersections of x and the polylines.
#       X1           -- the data.frame of c and d.
#
#
# Output:
#       area         -- the area of the graph of curve of y=p and the polylines.




area2_x_log <- function(x, X){
  X$ldrug <- log(X$drug)
  X <- X[,c("control","ldrug")]
  
  #calculate the coordinates of the intersection of x and the polylines. Denote it as (c,d).
  c = d <- rep(NA, (length(X$control)-1))
  for(n in 1:(length(X$control)-1)){
    if (log(x)>=X$ldrug[n] & log(x)<X$ldrug[n+1]){
      c[n] <- log(x)
      d[n] <- X$control[n]+((log(x)-X$ldrug[n])*(X$control[n+1]-X$control[n]))/(X$ldrug[n+1]-X$ldrug[n])
    }
  }
  
  y <- c[!is.na(c)]
  
  X1 <- as.data.frame(cbind(d,c))
  X1 <- X1[complete.cases(X1),]
  colnames(X1) <- c("control", "ldrug")
  X <- rbind(X,X1)
  X <- X[order(X$ldrug),]
  
  X1 <- X[(X$ldrug <= log(x)),]
  y <- min(y, X1$control)
  
  #calculate the area piece by piece
  area <- 0
  for (n in 1:(length(X$ldrug)-1)){
    if (X$ldrug[n]<=log(x)){
      area1 <- 0.5*(X$ldrug[n+1]-X$ldrug[n])*(X$control[n+1]+X$control[n]-2*y)
      if (area1>0)
        area <- area1 +area
    } 
  }
  return(area)
}
