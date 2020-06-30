## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function makeCacheMatrix creates a special "matrix"

# at the end what you get is a list that contains a function for:

# set the value of the matrix

# get the value of the matrix

# set the value of the solve matrix

# get the value of the solve matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#The following function calculates the inverse of the special

#matrix created with the previous function. However, it is first

#verified if the inverse has not been calculated, if so, it gets 

#the inverse of the cache and omits the calculation. Otherwise, 

#calculates the inverse of the matrix and sets the value of the 

#inverse in the cache using the function setInverse.


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
pmatrix$get()
cacheSolve(pmatrix)
pmatrix$getInverse()
