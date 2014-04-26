## Fnctions for cacheing results of potentially time-consuming 
## matrix computations. 

## Create a special "matrix" object that caches its inverse
## Return a list that contains the names of 4 functions 
## that operate on that object 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Compute the inverse of a square matrix 
## If the inverse was previously calculated then retrieve it from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
