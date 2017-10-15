#makeCacheMatrix creates a list of functions.
#The functions can cache a matrix and its inverse,
#and can also retrieve a previously-cached matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrInv <- NULL
  set <- function(y) {
    x <<- y
    matrInv <<- NULL
  }
  get <- function() x
  setSolved <- function(o) matrInv <<- o
  getSolved <- function() matrInv
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)

  
}

#cacheSolve retrieves the inverse of a matrix from the cache, if previously stored
#Else, it will calculate the inverse of a matrix and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolved()
  if(!is.null(m)) {
    message("returning pre-solved matrix")
    return(m)
  }
  prevMatr <- x$get()
  m <- solve(prevMatr, ...)
  x$setSolved(m)
  m
}