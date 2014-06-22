## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix does the following:
# sets the value of a matrix, gets the value of that matrix
# sets that value of that matrix's (matricies?) inverse, then gets said inverse

makeCacheMatrix <- function(mat = matrix()) {
  
  # inver currently set to NULL, will eventually store the cached matrix inverse
  inver <- NULL
  
  set <- function(y) {
    mat <<- y
    inver <<- NULL
  }
  
  # grabs matrix value
  get <- function() mat
  
  # sets the matrix inverse
  setinver <- function(invert) inver <<- invert
  getinver <- function() inver
  
  # gets the matrix inverse
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

# cacheSolve does the following: computes the inverse of mat (matrix)
cacheSolve <- function(mat, ...) {
  inver <- mat$getinver()
  
  # If the inverse has already been calculated, return the value
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  
  # Else, calculate the uncalculated inverse
  data <- mat$get()
  inver <- invert(data, ...)
  
  # Call the caching function on the inverse
  mat$setinver(inver)
  
  # BAM!
  inver
}