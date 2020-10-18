# Replication file for: "Fractals with R"
# RPubs-link: https://rpubs.com/mstefan-rpubs/fractals
# (c) Martin Stefan, October 2020

# iterator function: recursive tree
tree <- function(line0, angle=30, reduce=.7, randomness=0) {
  
  # angles and randomness
  angle1 <- angle+rnorm(1,0,randomness)  # left branch
  angle2 <- -angle+rnorm(1,0,randomness) # right branch
  
  # new branches
  line1 <- newLine(line0, angle=angle1, reduce=reduce)   
  line2 <- newLine(line0, angle=angle2, reduce=reduce)
  
  # store in matrix and return
  mat <- matrix(c(line1,line2), byrow=T, ncol=4)
  return(mat)
  
}