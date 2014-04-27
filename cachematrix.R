## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## function to make special matrix
m <- NULL
## set value of matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## sets the Inverse of the matrix
  setInverse <- function(solve) m <<- solve
  ## gets the Inverse of the matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()           #query the x matrix is cache         
  if(!is.null(m)) {           #if there is a cache
    message("getting cached data") 
    return(m)                #just return the cache, no computation needed
  }
  data <- x$get()             #if there's no cache
  m <- solve(data, ...)        #we actually compute them here
  x$setInverse(m)                #save the result back to x's cache
  m                           #return the result
  
}