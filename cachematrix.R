## Overall these functions cache the inverse of a matrix

# This function creates a special "matrix" which is really a list of four functions

makeCacheMatrix <- function(x = matrix()) {

        # x is an invertible matrix
        
        inv <- NULL
        
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
  
        get <- function() x
  
        set_inverse <- function(inverse) inv <<- inverse

        get_inverse <- function() inv
  
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  

}


## Computes the inverse of the special matrix created above or retrieves the inverse from the cache

cacheSolve <- function(x, ...) {

    inv <- x$get_inverse()

    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$set_inverse(inv)
    
    inv

}
