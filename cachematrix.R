## Put comments here that give an overall description of what your
## functions do

## The 2 main functions created here are makeCacheMatrix and cacheSolve. The scope of these
## functions are to get the values of the matrix and to get the inverse of the same.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL  # to initilize the inverse valuse as NULL
  set<- function(y){
    x<<- y
    I<<- NULL
  }
  get<-function(){x} # to get the matrix x
  setI<- function(Inver){I<<-Inver}
  getI<-function(){I} # to get the inverse
  list(set = set, get = get, setI = setI, getI = getI)
  
}


## The function to retrieve the cache. 

cacheSolve <- function(x, ...) {
  I <- x$getI()
  if(!is.null(I)) {  # to check the value I is NULL
    message("getting cached data")
    return(I)
  }
  matrix <- x$get()
  I <- solve(matrix, ...)  # inverse matrix calculation 
  x$setI(I)
  I  ## Return a matrix that is the inverse of 'x'
}
