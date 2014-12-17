## Put comments here that give an overall description of what your
## functions do

## Caches the inverse of a matrix or gets the value in the cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                  #
      x <<- y
      m <<- NULL
    }
    get <- function() x                    # gets the data in the cache
    setinv <- function(solve) m <<- solve  # function to cache the solution
    getinv <- function() m                 # function to get the matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}




## Return the inverse of the matrix from the cache if available or else calculates it
cacheSolve <- function(x) {
  m <- x$getinv()          # gets the value of the inverse
  if(is.null(m)) {         # if inverse not yet calculated
    data <- x$get()        # get matrix
    m <- solve(data)       # calculate the inverse
    x$setinv(m)            # store the inverse in x (see setinv() in makeCacheMatrix)
  }
  else {print("cache data")}  
  m                        # ... and return the matrix
}
