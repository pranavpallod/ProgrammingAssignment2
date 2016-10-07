## Put comments here that give an overall description of what your
## functions do
##setmat is the function that sets the value of matrix
##getmat is the function that gets the value of the matrix
##setinverse is the function that sets the the value of the inverse of the matrix
##getinverse is the function that gets the value of the inverse of the matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  setmat <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  getmat <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(setmat = setmat,
       getmat = getmat,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinverse()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  mat <- x$getmat()
  invmat <- solve(mat, ...)
  x$setinverse(invmat)
  invmat
  
}

