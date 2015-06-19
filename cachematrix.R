## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a cache of matrix inversion.  The benefit of of a function like this is to improve proformance on a large dataset by caching a intensive operation, rather than continually calculating running the co
costly computation repeatedly.


makeCacheMatrix <- function(x = matrix())

 {
      
        
        inv_matrix <- NULL
        set_matrix <- function(y) {
                
                x <<- y
                inv_matrix <<- NULL
        }
        get_matrix <- function() x
        set_inv_matrix <- function(inverse) inv_matrix <<- inverse 
        get_inv_matrix <- function() inv_matrix
        list(set_matrix=set_matrix, get_matrix=get_matrix, set_inv_matrix=set_inv_matrix,  get_inv_matrix= get_inv_matrix)
}

## The cacheSolve function calculates the inverse of the makeCacheMatrix function listed above. 

cacheSolve <- function(x, ...) {
        
        inv_matrix <- x$get_inv_matrix()
        
       
        if (!is.null(inv_matrix)){
                
                message("getting cached data")
                return(inv_inv_matrix)
        }
        
       
        data_matrix <-  x$get_inv_matrix()
        inv_matrix <- solve(data_matrix, ...)
        
       
        x$set_inv_matrix(inv_matrix)
        
        return(inv_matrix)
}


