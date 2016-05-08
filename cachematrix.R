## R Programming Assignment #2: Lexical Scoping

## Create a special "matrix" object that can cache its own inverse.

  makeCacheMatrix <- function(x = matrix()) {
    
    mat_inv<- NULL
    set <- function(y) {
        x <<- y
        mat_inv<<- NULL
        }
    get <- function() x
    setInverse <- function(inverse) mat_inv<<- inverse
    getInverse <- function() mat_inv
    list(set = set,get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }

## Compute the inverse of the matrix created by makeCacheMatrix.  
## If the inverse has already been calculated, it will retrieve the matrix(makeCacheMatrix) 
## If not, it will calculate the inverse of the matrix 

  cacheSolve <- function(x, ...) {
    mat_inv<- x$getInverse()
    ## First check to see if the matrix is in the cache already
    
    if (!is.null(mat_inv)) {
        message("getting data from cache")
        return(mat_inv)
    }
    ## If the matrix does not exist, create a new matrix and cache it. 
    ## Print the inverse matrix
    
      message("creating new inverse matrix")
      mat <- x$get()
      mat_inv<- solve(mat, ...)
      x$setInverse(mat_inv)
      mat_inv
  }
