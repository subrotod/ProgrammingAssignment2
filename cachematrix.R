##   *********************************************************************
##     File: cacheMatrix.R 
##     Created : 1/21/2015
##
##     This file contains two functions makeCacheMatrix and cacheSolve. 
##     makeCacheMatrix is used in conjunction with the cacheSolve function.
##     These two functions provide an efficient way to store and calculate
##     a matrix and its inverse, when the inverse of the same matrix
##     has to be calculated multiple times.
##
##     makeCacheMatrix takes a R matrix as input and returns a list object. 
##     The returned list object is used by the cacheSolve function to 
##     compute and cache the inverse, and return the inverse value.
##   *********************************************************************
##  Additional details are provided before each function definition.
##

## makeCacheMatrix 
## Description: Returns a list object with 4 function object members that support 
##              matrix and matrixinverse caching (persistent storage) and retrieval.
## 
## Sample Usage: Create and initialize a CacheMatrix using one of the two methods
##              a)  cm <- makeCacheMatrix(NULL) (Creates a NULL cached matrix)
##                  cm$set(matrix(1:4,2,2)) (The 2x2 matrix is cached) 
##              or
##              b) cm <- makeCacheMatrix(matrix(1:4,2,2)) (Creates a Cached Matrix for the 2x2 )
##
## Assumptions: The matrix passed as an argument to makeCacheMatrix is square and invertible.
##
## Arguments : x: matrix to be cached (i.e class(x) should be equal to matrix)
##
## Returns :  A list with 4 function objects that operates on the cached matrix
##            set(m) : Cache a new matrix m and resets cached inverse.
##            get() : Retrieves the cached matrix.
##            setinv(minv) Cache the matrix minv as the inverse
##            getinv() : Retrieves the cached inverse.
##
## Details:     1. When a new matrix is cached the cached inverse is set to NULL,  
##                 forcing cacheSolve to recompute and cache a new inverse.
##              2. The set() function is optimized to check if the cached matrix 
##                 has changed during the call. 
##              3. The set() function and the setinv() function uses the enclosing 
##                 environment of makeCacheMatrix and deep assignment (<<- ) to store 
##                 the matrix and its inverse.
##

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  set <- function(y) {
    
    ## Check if the cached matrix and the new matrix (x)  are different
    ## If the cached and new matrices have the same value the cached inverse 
    ## will not change. Therefore the old inverse is valid.
    
    if (!identical(x,y))
    {
      x <<- y
      # Reset the cached inverse. Forces inverse recalculation in the next cacheSolve call 
      i <<- NULL
    }
  }
  get <- function() {x}
  setinv <- function(inv) {i <<- inv}
  getinv <- function() {i}
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
} 


## cacheSolve
## Description: Computes the inverse of a matrix. The matrix and its inverse are cached, so
##              that consecutive calls to the cacheSolve function with the identical matrix
##              do not require a new inverse to be computed each time. This function is to be
##              used along with the makeCacheMatrix function as shown in the Sample Usage.
##
## Sample Usage:
##
##              cm <- makeCacheMatrix(matrix(1:4,2,2)) (Creates a 2x2 Cached Matrix)
##  
##              ## Compute the inverse for the Cached matrix
##              minv <- cacheSolve(cm)  (The 2x2 inverse will be computed and cached)
##              minv <- cacheSolve(cm)  (Cached 2x2 inverse will be returned)
##              cm$set(matrix(1:9,3,3)) (the 2x2 matrix will be replaced and the 3x3 matrix is cached)
##              minv <- cacheSolve(cm) (The 3x3 inverse will be computed and cached, replacing the 2x2 inverse)
##
## Assumptions: The matrix passed as an argument to makeCacheMatrix is square and invertible.
##
## Arguments : x: A list object returned from the makeCacheMatrix function. Used to cache the matrix and its 
##                inverse.
##             ...: Additional arguments passed to the solve function. Use ??solve for more info.
##
## Returns : The matrix inverse stored in x.
##

cacheSolve <- function(x, ...) {          ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
} 
