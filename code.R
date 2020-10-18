# Replication file for: "Fractals with R"
# RPubs-link: https://rpubs.com/mstefan-rpubs/fractals
# (c) Martin Stefan, October 2020

# clear workspace
rm(list = ls())

# source functions
source("functions.R")
source("ifun_koch.R")
source("ifun_tree.R")

# koch curve
fractal <- matrix(c(10,0,20,1e-9), nrow=1)
for(i in 1:6) fractal <- iterate(fractal, ifun=koch)
emptyCanvas(xlim=c(10,20), ylim=c(0,3))
drawObject(fractal)

# koch snowflake
A <- c(0,1e-9)
B <- c(3,5)
C <- c(6,0)
fractal <- matrix(c(A,B,B,C,C,A), nrow=3, byrow=T)
for(i in 1:6) fractal <- iterate(fractal, ifun=koch)
emptyCanvas(xlim=c(-2,8), ylim=c(-2,5))
drawObject(fractal)

# recursive tree (perfect)
fractal <- matrix(c(0,0,0,10), nrow=1)
emptyCanvas(xlim=c(-30,30), ylim=c(0,35))
drawObject(fractal)
for(i in 1:10) {
  fractal <- iterate(fractal, ifun=tree, angle=23)
  drawObject(fractal)
}

# recursive tree (organic)
set.seed(1234)
fractal <- matrix(c(0,0,0,10), nrow=1)
emptyCanvas(xlim=c(-30,30), ylim=c(0,35))
lwd <- 7
drawObject(fractal, lwd=lwd)
for(i in 1:12) {
  lwd <- lwd*0.75
  fractal <- iterate(fractal, ifun=tree, angle=29, randomness=9)
  drawObject(fractal, lwd=lwd)
}

# multiple recursive trees
Z <- c(0,0)
A <- c(1e-9,5)
B <- c(5,-1e-9)
fractal <- matrix(c(Z,A,Z,B,Z,-A,Z,-B), nrow=4, byrow=T)
emptyCanvas(xlim=c(-20,20), ylim=c(-20,20))
drawObject(fractal)
for(i in 1:11) {
  fractal <- iterate(fractal, ifun=tree, angle=29, reduce=.75)
  drawObject(fractal, col=i+1)
}
