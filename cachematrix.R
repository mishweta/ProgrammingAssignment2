## This function creates a special "matrix" object that can cache its inverse.

## The function will compute the inverse of a matrix and cache the same 

makeCacheMatrix <- function(x = matrix()) 
  {
    m <- NULL
    set <- function(y) 
      {
    x <<- y
    m <<- NULL
      }
    get <- function() x
    setinverse <- function(solve) 
    m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

  }
## Additional function written to see if the two matrices are equal per the assignment requirement
mateq <- function(x, y)
{is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)}
  
## This function will check if inverse of a matrix exists 
##in the cache and if yes, then the inverse will be published from the cache instead of 
##recomputing

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  cachemean <- function(x, ...) 
    {
    m <- x$getinverse()
    if(!is.null(m) && mateq(x,m)) 
      {
      message("getting cached data")
      return(m)
      }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
}
