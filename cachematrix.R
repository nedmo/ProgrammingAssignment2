## These functions cache the inverse of a matrix and return the
## cached version if it has been calculated to save time.

## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Set local var to null
  m <- NULL
  
  ## Create Set function to set cache values
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Create get function to return cache values 
  get <- function() x
  
  # Crete getinv/setinv to get/set cached inverse matrices 
  setinv <- function(solve) m <<- solve
  getinv <- function() m

  ## Create list containing all 4 functions 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  This function computes the inverse of the special matrix 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated then retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Check cache with above function to see if the inverse has 
  ## already been calculated
  m <- x$getinv()
  ## If cached inv exists, return it to save time
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix data
  data <- x$get()
  ## Calculate the inverse
  m <- solve(data, ...)
  ## Put the calculated inverse matrix into the cache
  x$setinv(m)
  
  ## Return a matrix that is the inverse of 'x'
  m

}
