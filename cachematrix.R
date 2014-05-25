## Method:
# Clone the cachematrix.R file
# Copy/Paste the example code into relevant functions
# Apply the solve function to the matrix x
# ie replace all words "mean" with "solve"
# Now. Test the functions

#R.version.string


## Write a short comment describing this function

# The function makeCacheMatrix creates an empty 'matrix', 
  #1. set the value of the matrix
  #2. get the value of the matrix
  #3. set the value of the inverse matrix
  #4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#Checks to see if The inverse of the matrix has already been calculated,
#If not, then:
#Calculates the inverse of a matrix using makeCacheMatrix, and caches it.
#If so, then:
#The cache provides the inverse matrix and the calculation is skipped.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
