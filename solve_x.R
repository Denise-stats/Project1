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
#       v            -- a vector of the x coordinates of the intersection of y and the polylines.
#




solve_x <- function(y, X){
    X <- X[,c("control","drug")]
    if (y<min(X$control))
    stop(paste("The minimum value of % control is", min(X$control)))
    else if (y>= max(X$control))
    stop(paste("The maximum value of % control is", max(X$control)))
    
    #calculate the coordinates of the intersection of y and the polylines. Denote it as (a,b).
    a = b <- rep(NA, 7)
    for(m in 1:6){
        if ((y<=X$control[m] & y>X$control[m+1])|(y>X$control[m] & y<=X$control[m+1])){
            a[m] <- X$drug[m]+((X$drug[m+1]-X$drug[m])*(y-X$control[m]))/(X$control[m+1]-X$control[m])
            b[m] <- y
        }
    }
  
   X2 <- as.data.frame(cbind(b,a))
   colnames(X2) <- c("control", "drug")
   X2 <- X2[complete.cases(X2),]
   v <- X2$drug
   return(v[1])
}

x1 <- solve_x(90, data); x1
#[1] 0.05937155

x2 <- solve_x(40, data); x2
#[1] 0.3516076 0.9429134


