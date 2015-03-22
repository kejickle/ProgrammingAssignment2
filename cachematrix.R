## The following two functions create a cached matrix so that it is easier 
## to solve for the inverse of a matrix.  
## The first function creates the cached matrix and gets and sets values of
## the inverse while the second function solves for the inverse, if it is already
## solved for then it gets that inverse value from previously being cached.

## This function creates the cached matrix 

makeCacheMatrix <- function(x = matrix()) {
  m = NULL 
  #The next few lines get and set the value of the matrix
  set = function(y){
    x <<- y 
    m <<- NULL
  }
  get <- function() x 
  #These next two lines get and set the inverse of the matrix input in the function
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m 
  list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}



## This function solves for the inverse of the matrix x that is passed into the function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #if the inverse has already been cached then the function retrieves this value and returns the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #here the function gets and solves for the inverse using the above function
  data = x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  #the function returns the inverse of the matrix as stored in the value m
  m
}
