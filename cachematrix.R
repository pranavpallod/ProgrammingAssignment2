## Put comments here that give an overall description of what your
## functions do
##setmat is the function that sets the value of matrix
##getmat is the function that gets the value of the matrix
##setinverse is the function that sets the the value of the inverse of the matrix
##getinverse is the function that gets the value of the inverse of the matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {       #defining the function
  invmat <- NULL                                  # giving default value to the invmat  
  setmat <- function(y) {                         # defining function setmat to set the value of the matrix 
    x <<- y
    invmat <<- NULL                               # assigning value to invmat
  }
  getmat <- function() x                          # defining function getmat, which gets the value of invmat
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(setmat = setmat,
       getmat = getmat,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {                 # defining the function to get cache value 
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinverse()
  if (!is.null(invmat)) {                        # if the matrix is similar to the one for which we have calculated the inverse, then return cached value
    message("getting cached data")
    return(invmat)
  }
  mat <- x$getmat()
  invmat <- solve(mat, ...)
  x$setinverse(invmat)
  invmat
  
}

