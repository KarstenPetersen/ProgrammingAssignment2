## The 2 functions creates a special "matrix" object that reuses the calculated inverse matrix stored in the cache, if the matrix is not changed

## The function creates a list with functions to: 
##     set&get the value of the matrix 
##     set&get the inverse matrix
 
  makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) invers <<- inv
  getinverse <- function() invers
  
  list(set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  
}

## The function calculates the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse matrix has already been calculated (and the matrix has not changed), 
##  then the function gets the result from the cache.

cacheSolve <- function(x, ...) {
  
  invers <- x$getinverse()
  
  if(!is.null(invers)) {
      message("getting cached data")
      return(invers)
  }
  
  m <- x$get()
  invers <- solve(m, ...)
  x$setinverse(invers)
  invers
  
}


## -------------------------------------------------------------------------------------------------
## Just to test the solve funtion:
matr <- c(4,2,7,6)
dim(matr) <- c(2,2)
matr
solve(matr)
## for result see https://www.mathsisfun.com/algebra/matrix-inverse.html

# To test the created functions 
m <- makeCacheMatrix (matr)
cacheSolve(m)
cacheSolve(m)

# when a new matrix is created the cache is not reused
matr <- c(42,22,37,36)
dim(matr) <- c(2,2)
m <- makeCacheMatrix (matr)
cacheSolve(m)