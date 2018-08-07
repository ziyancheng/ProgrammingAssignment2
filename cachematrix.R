## Caching the inverse of a Matrix:
## Matrix inversing is usually a costly computation and there maybe some benefit to caching the inverse 
## of a matrix rather than compute it repeatly. The following two function that are used to create a matrix and store this matrix and caches its inverse.

## This function will create a matrix for storing its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inve <- NULL
  set <- function(y){
    
     x <<- y
     inve <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inve <<- inverse
  
  getInverse <- function() inve
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function calculate the inverse of matrix from call of makeCacheMatrix. If the inverse has been calculated then it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
