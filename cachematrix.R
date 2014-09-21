## Coursera R-programming Assignment week 3

## Assignment about lexical scooping
## consice explanation
## https://class.coursera.org/rprog-007/forum/thread?thread_id=707

library(MASS)

makeCacheMatrix <- function(r_matrix = matrix()) {
  
  ma <- NULL
  set <- function(y){
    r_matrix <<- y
    ma <<- NULL
    
  }
  
  get <- function() r_matrix
  
  setmatrix <- function(solve) 
    ma <<- solve
  
  getmatrix <- function() ma
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## chol2inv(chol(data)) is faster than solve algorithm, first
## computes the factors with chol() and then the inverse
## with chol2inv()

cacheSolve <- function(r_matrix, ...) {
        ## Return the inverse of 'x'
  inverse_m  <- r_matrix$getinverse()
  
  if (!is.null(inverse_m)){
    message("got cached data")
    return(inverse_m)
  }
  
  data  <- r_matrix$get()
  inverse_m  <- chol2inv(chol(data))
  r_matrix$setinverse(inverse_m)
  inverse_m
  
}
