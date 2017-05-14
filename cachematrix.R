## Put comments here that give an overall description of what your
## functions do

# the 2 functions take advantage of the scoping rules of the R language for matrixinverse funtion 
# and how they can be manipulated to preserve state inside of an R object.

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to inverse the matrix
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list (set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
# The cacheSolve function calculates the matrx inverse of the special "vector" created with the above function. 
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and 
# sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

