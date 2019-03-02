## First the inverse is set to Null and is placed as a holder for future value.
## set function -> Then x is to the new vetor y and  resets the inverse, i, to Null.
## get function -> gives a vector x
## setinverse function -> sets the inverse, i to solve matrix
## getinverse function -> returns the inverse i
## list -> returns the special vector containing all the functions defined.

## makeCacheMatrix is a function to get the matrix as a object that can Cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solveMatrix) i <<- solveMatrix
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If inverse of a matrix was previously computed, then this function helps in retreiving its value without computing, hence time saving

cacheSolve <- function(x, ...) {
        
  i <<- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
    ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
