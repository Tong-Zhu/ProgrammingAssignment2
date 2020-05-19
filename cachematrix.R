## Put comments here that give an overall description of what yourfunctions do：
##Comments:1.The first function creates a special "matrix" object that can cache its inverse.
##It creates a list containing a function to set the value of the matrix, get the value of 
##the matrix, set the value of the inverse and get the value of the inverse. 2.The second function
##computes and returns the inverse of the matrix created in the first one. If the matrix is not a 
##square invertible matrix, it will return error.

## Write a short comment describing this function:
##The function makeCacheMatrix sets the matrix x at first.Then it gets the value of a matrix, sets the 
##inverse and gets the inverse. It actually returns a list of 'set','get','setinverse' and 'getinverse'.
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 


## Write a short comment describing this function:
##There are two parts in the function cacheSolve.If it can get the inverse which has been computed
##in the first function(cache),just return the calculated value. Otherwise, it calculates the inverse
##of the matrix in 'get' function and sets this inverse in 'setinverse' function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## solve(X) returns its inverse.
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}

#Example:
x<-matrix(1:4,2,2)
makeCacheMatrix(x)
cacheSolve(makeCacheMatrix(x))

 
