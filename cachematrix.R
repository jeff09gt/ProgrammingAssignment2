## The next two functions makes the job of caching the inverse of a matrix in case this
## result already exist on memory. In case the result is not already calculated, the 
## process will calculate the inverse of an "x" matrix and have it on memory to be
## be used on future calculations

## makeCacheMatrix receives a parameter and sets it as a matrix
## with "X" creates the functions to: 
##    1) Set the value of the matrix
##    2) Be able to GET the value of the matrix
##    1) Calculates the inverse of the matrix
##    1) Get the calculated value of the inverse of the matrix


## working example with an 3 by 3 matrix:

# x = matrix(c(3,0,2,2,0,-2,0,1,1), nrow=3, byrow=TRUE)
# m = makeCacheMatrix(x)
# cacheSolve(m)

## after running the example, you will get:

##      [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0

## if the cacheSolve(m) is used again, the message will 
## shown because there is a result already calculated.

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setMatInverse <- function(inverse) mInverse <<- inverse
  getMatInverse <- function() mInverse
  list(set=set
       , get=get
       , setMatInverse=setMatInverse
       , getMatInverse=getMatInverse)
}


## cacheSolve will invoke the process to calculate the inverse of an X matrix only
## if the result isn't already calculated. In case the result already exist, will
## just load the result on cache, show a message and then show this result.

cacheSolve <- function(x, ...) {
  mInverse <- x$getMatInverse()
  if(!is.null(mInverse)) {
    message("Loading data to cache ...")
    return(mInverse)
  }
  MatData <- x$get()
  mInverse <- solve(MatData)
  x$setMatInverse(mInverse)
  mInverse
}


## 
## 
## EndLineOfCode
