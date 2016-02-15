### Programming Assigment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly (there are also
## alternatives to matrix inversion that we will not discuss here). Your assignment 
## is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {   
  m <- NULL
  
  set <- function(y) {                        ## set the value of the matrix
    x <<- y                                   ## create a matrix
    m <<- NULL
  }
  
  get <- function() x                         ## get the value of the matrix
  
  set_matrix <- function(solve) m <<- solve   ## set the value of the matrix,
                                              ## return the inverse in cache
  
  getinv_matrix <- function() m               ## get the value (inverse) of 
  ## the matrix from cache
  
  list(set = set, get = get,                  ## create the "matrix"
       setmatrix = set_matrix,                
       getinv_matrix = getinv_matrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  m <- x$getinv_matrix()                      ## trying to get the matrix from cache
  
  if(!is.null(m)) {                           ## conditional: if the inverse exists 
    message("getting cached data")            ## it is displayed  from cache        
    return(m)
  }
  
  data <- x$get()                             ## if the inverse doesn't exist from cache
                                              ## create the matrix
  
  m <- solve(data, ...)                       ## solve the matrix and returns its inverse
  x$setmatrix(m)
  m
}

