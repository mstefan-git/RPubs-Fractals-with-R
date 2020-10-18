# Replication file for: "Fractals with R"
# RPubs-link: https://rpubs.com/mstefan-rpubs/fractals
# (c) Martin Stefan, October 2020

# function to create empty canvas
emptyCanvas <- function(xlim, ylim, bg="gray20") {
  par(mar=rep(1,4), bg=bg)
  plot(1, 
       type="n", 
       bty="n",
       xlab="", ylab="", 
       xaxt="n", yaxt="n",
       xlim=xlim, ylim=ylim)
}

# function to draw a single line
drawLine <- function(line, col="white", lwd=1) {
  segments(x0=line[1], 
           y0=line[2], 
           x1=line[3], 
           y1=line[4], 
           col=col,
           lwd=lwd)
}

# wrapper around "drawLine" to draw entire objects
drawObject <- function(object, col="white", lwd=1) {
  invisible(apply(object, 1, drawLine, col=col, lwd=lwd))
}

# function to add a new line to an existing one
newLine <- function(line, angle, reduce=1) {
  
  x0 <- line[1]
  y0 <- line[2]
  x1 <- line[3]
  y1 <- line[4]
  
  dx <- unname(x1-x0)                      # change in x direction
  dy <- unname(y1-y0)                      # change in y direction
  l <- sqrt(dx^2 + dy^2)                   # length of the line
  
  theta <- atan(dy/dx) * 180 / pi          # angle between line and origin
  rad <- (angle+theta) * pi / 180          # (theta + new angle) in radians
  
  coeff <- sign(theta)*sign(dy)            # coefficient of direction
  if(coeff == 0) coeff <- -1
  
  x2 <- x0 + coeff*l*cos(rad)*reduce + dx  # new x location
  y2 <- y0 + coeff*l*sin(rad)*reduce + dy  # new y location
  return(c(x1,y1,x2,y2))
  
}

# function to run next iteration based on "ifun()"
iterate <- function(object, ifun, ...) {
  linesList <- vector("list",0)
  for(i in 1:nrow(object)) {
    old_line <- matrix(object[i,], nrow=1)
    new_line <- ifun(old_line, ...)
    linesList[[length(linesList)+1]] <- new_line
  }
  new_object <- do.call(rbind, linesList)
  return(new_object)
}