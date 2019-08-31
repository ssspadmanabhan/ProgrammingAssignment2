## Function Name : makeCacheMatrix
## Matrix as Input 
## The purpose of this function is to store the matrix data using get & set method

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) invers <<- matrix
  getMatrix <- function() invers
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)  
}

## Function Name : cacheSolve
## The purpose of this function is to inverse of the matrix only if it is not inversed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  dataMatrix <- x$get()
  inverseMatrix <- solve(dataMatrix)
  x$setMatrix(inverseMatrix)
  inverseMatrix
}
