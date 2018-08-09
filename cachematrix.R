## Caching the inverse of a Matrix:
## Matrix inversing is usually a costly computation and there maybe some benefit to caching the inverse 
## of a matrix rather than compute it repeatly. The following two functions that are used to create a matrix and store this matrix and caches its inverse.

## The first function will create a "matrix", in fact it is a list for storing its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## declare and init
  inve <- NULL
  
  ## set the value of the matrix
  set <- function(y){
    
     x <<- y
     inve <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of inverse
  setInverse <- function(inverse) inve <<- inverse
  
  ## get the value of inverse
  getInverse <- function() inve
  
  ## this is the special Matrix, indeed is a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The second funcation is to calculate the inverse of matrix from call of makeCacheMatrix. If the inverse has been calculated then it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a "matrix" LIST that is the inverse of 'x'

        inve <- x$getInverse()
        
        if (!is.null(inve)){
          
                message("getting cached data")
                return(inve)
        }
        
        matr <- x$get()
        
        inve <- solve(matr, ...)
        
        x$setInverse(inve)
        
        inve
}
