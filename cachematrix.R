## These pair of functions are used to create a special object that stores a matrix and 
## cache´s the inverse of the matrix. 

## This function creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL 
      set <- function(y){
            x <<- y
            I <<- NULL
      }
      get <- function()x
      setinverse <- function(solve) I <<- solve 
      getinverse <- function() I
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the the special matrix. If inverse has alredy 
## been calculated then the inverse is retrived from the cache. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      I <- x$getinverse()
      if (!is.null(I)){
            message ("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data,...)
      x$setinverse(I)
      I
}
