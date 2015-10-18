# Provides functionality for calculating and caching a matrix inverxe. Use the makeCacheMatrix
# function to create a cachable matrix object that can be used with cacheSolve. Use cacheSolve
# to calculate the inverse of the cachable matrix object.

# Create a cachable matrix object to be used with cacheSolve function
#
# Args:
#   x: matrix that the inverse will be calculated from
#
# Returns:
#   Cachable matrix object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  set_inverse <- function(inverse) {
    i <<- inverse
  }
  get_inverse <- function() {
    i
  }
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


# Retrieve the inverse of a cachable matrix object
#
# Args:
#   x: cachable matrix object used to calculate the inverse
#
# Returns:
#   Inverse of the matrix in the cachable matrix object

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if (!is.null(i)) {
    i
  } else {
    m = x$get()
    i <- solve(m)
    x$set_inverse(i)
    i
  }
}
