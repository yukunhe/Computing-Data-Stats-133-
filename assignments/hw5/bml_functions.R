#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  matrix(sample(x=0:2, size=r*c, replace = T, prob=c(1-p,p/2,p/2)), nrow=r, ncol=c)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  red_cars = m*(m==1)
  block_red_cars = m[,c(2:ncol(m),1)] != 0
  m.redcars = m*(m!=1)+ red_cars*block_red_cars+
    (red_cars*!block_red_cars)[,c(ncol(m),1:(ncol(m)-1))]
  blue_cars = m.redcars*(m.redcars==2)
  block_blue_cars = m.redcars[c(nrow(m.redcars),1:(nrow(m.redcars)-1)), ] != 0
  m.bluecars = m.redcars*(m.redcars!=2)+blue_cars*block_blue_cars+
    (blue_cars*!block_blue_cars)[c(2:nrow(m.redcars),1), ]
  grid.new = identical(m, m.bluecars) == F
  return(list(m.bluecars, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m = bml.init(r, c, p)
  image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
  for (i in 1:1000) {
    n = bml.step(m)
    if (n[[2]] == TRUE) {
      m = n[[1]]
      image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
    } else {
      image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
      return (list(i, TRUE))
    }
  }
  return (list(i, FALSE))
}

