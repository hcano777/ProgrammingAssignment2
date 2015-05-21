## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix and cacheSolve functions are meant to work 
## together. makecacheMatrix provides accessibility control to 
## matrix object in question. cacheSolve makes use of an
## instance of makeCacheMatrix on runtime and skips the 
## inverse matrix computation if already computed.

## Usage: 

## 1. Load Source File
## > source("cachematrix.R")
## 2. assign a simple matrix to some variable (say m)
## > m <- matrix(c(1,2,3,0,1,5,5,6,0),3,3)
## 3. instantiate makeCacheMatrix to m and assign it to some 
## variable - say x
## x <- makeCacheMatrix(m)
## 4. instantiate cacheSolve(x)
##> cacheSolve(x)
##[,1] [,2] [,3]
##[1,]    1    0    5
##[2,]    2    1    6
##[3,]    3    5    0
## 5. Run cacheSolve(x). Note the console message that the 
## inverse of the matrix m is being pulled from cache.
##> cacheSolve(x)
##getting cached data 
##[,1] [,2] [,3]
##[1,] -6.0    5 -1.0
##[2,]  3.6   -3  0.8
##[3,]  1.4   -1  0.2
##> 

##--------------------------------------------------------
## makeCacheMatrix takes on a matrix function as an input
##parameter. It then provides 2 getters and 2 setters. Two
##are generic to get/set the matrix itself. The other two
##getters and setters are specialized and point to a 
##function that solves for the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  # everytime we invoke makecachematrix, we ensure that
  #cached value is cleared.
  m <- NULL
  
  # y has scope inside set. x has scope inside 
  # makeCacheMatrix. m has 'global' scope.
  set <- function(y) {
    x <<- y
    ## whenever the matrix changes, nullify the cached 
    ## value. If the matrix is being changed to the 
    ## same matrix, then we do not want to nullify 
    ## the inverse.
    m <<- NULL
  }
  #returns the matrix set in makeCacheMatrix
  get <- function() x
  # Points to a function that solves the inverse
  #of matrix and sets to global variable m
  setmatrixinverse <- function(solve) m <<- solve
  # returns the matrixinverse in cache
  getmatrixinverse <- function() m
  # stores named properties in list
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}

##--------------------------------------------------------
## cacheSolve is a function that takes on an object 
## containing the same attributes as defined in 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'. value
  #will be null if the inverse has not yet been
  #calculated.
  m <- x$getmatrixinverse()
  
  # check if global variable (environment variable) is
  # null. If it is, that means the inverse for that 
  # particular matrix has not been calculated since the 
  # matrix was set.
  if(!is.null(m)) {
    message("getting cached data")
    # return cached value that is not null.
    return(m)
  }
  
  # If I get to this point, this means that the inverse 
  # needs to be calculated for the matrix that has been
  # set
  
  # Populate the variable data with the matrix object
  data <- x$get()
  
  # Calculate inverse and set the global variable 'm'.
  m <- solve(data, ...)
  x$setmatrixinverse(m)
  # return m to console
  m
}

