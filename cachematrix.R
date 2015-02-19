## These functions help reduce potentially time-consuming computations by caching the value of an operation
## so it can be looked up in the cache rather than re-computed.

# # creates a list of functions to manipulate the data

makeCacheMatrix <- function(x = numeric()) {          
  
  inverse <- NULL                                     # initializes the variable inverse and clears its previous values
  
  set <- function(y) {                                # creates function to set the value of the matrix in a different environment
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x                                 # creates function get to access the matrix
  
  setinverse <- function(solve) inverse <<- solve     # creates function setinverse that sets the inverse in the cache
  
  getinverse <- function() inverse                    # creates function getinverse to access the inverse                  
  
  list(set = set, get = get,                          # returns list of functions to access the cached data
       setinverse = setinverse,
       getinverse = getinverse)
}

# retrieves inverse matrix from the cache or calculates the inverse if it's not in the cache.
cacheSolve <- function(x, ...) {                      
  
  inverse <- x$getinverse()                         # gets the inverse and stores it into a variable 
  
  if(!is.null(inverse)) {                           # if the variable is NOT null (ie the value already exists), 
    message("getting cached data")                  # returns the value from the cache
    return(inverse)
  }
  data <- x$get()                                   # retrieves data with get function
  inverse <- solve(data, ...)                       # calculates the inverse
  x$setinverse(inverse)                             # stores the inverse in the cache
  inverse                                           # returns the inverse
}