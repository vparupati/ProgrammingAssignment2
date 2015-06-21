## Functions for creating and using cacheble inverted matrices 

makeCacheMatrix <- function(x = matrix()) {
  
  # validate matrix
  if (!is.matrix(x)) {
    stop("Agument is not a matrix")
  }
  
  invertedMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  
  # Functions for getting and setting cached inv. matrix value
  get <- function() x
  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) invertedMatrix <<- solve
  get.inverse <- function() invertedMatrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)

}



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
  invertedMatrix <- cacheableMatrix$get.inverse()
  # getting cached inverted matrix
  if(!is.null(invertedMatrix)) {
    return(invertedMatrix)
  }
  #create inverted matrix and cache it
  matrixToInverse <- cacheableMatrix$get()
  invertedMatrix <- solve(matrixToInverse)
  cacheableMatrix$set.inverse(invertedMatrix)
  invertedMatrix
  
}

