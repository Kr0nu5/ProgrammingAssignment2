## R Programming Assignment 2: cachematrix.R

## This function will create the matrix, and, define the set & get functions 
## within it:

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve 
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## This function calculates and caches the inverse of the matrix created
## using the functions defined above

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {  ## Skip calculation if inverse is already cached
    message("getting cached data") ## Produces an output to let you know the inverse has not been calculated yet
    return(m) 
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m      ## Returns a matrix that is the inverse of 'x'
}
