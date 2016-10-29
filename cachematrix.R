# Makes a container with functions from a Vector
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# Extract cache data from a container or calculate it 
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

# Makes a container with basic functions from a Matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(solve) m <<- solve
  getCache <- function() m
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}

# Extract cache data from a container or calculate it 
cacheSolve <- function(x, ...) {
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  m
}

#Quick test
#Create a diagonal matrix
a<-diag(5,3)  
#Create an object with matrix a
x<-makeCacheMatrix(a)
#First call calculates an inverse matrix
cacheSolve(x)
#Second call returns the cached data
cacheSolve(x)