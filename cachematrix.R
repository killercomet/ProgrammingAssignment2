## cachematrix.R makes it easy to store a cached inverse of a matrix in order to speed
## up the retrieval of the inverse of a matrix by storing it in memory after the first access.
## Example usage: 
## m <- makeCacheMatrix(matrix(runif(6250000), 2500, 2500))
## inv <- cacheSolve(m)  #This first call computes the inverse of the matrix. 
## inv2 <- cacheSolve(m) #This second call is much faster (retrieves the cached data) 

## Creates a special matrix object prepared to cache its inverse.
## Parameters:
## x: matrix. The matrix to use. 
## Returns: 
## An object prepared to store a matrix and its inverse as data and the following functions:
## -Two setters to change the matrix used and to store its inverse(set and setinverse) 
## -Two getters to retrieve the stored matrix and inverse (get and getinverse)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
         x <<- y
         inv <<- NULL
    }    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Retrieves the inverse of a function by either computing it when its not already available or by 
## returning a cached instance of it. 
## Parameters:
## x: special matrix object for which we want to compute the inverse created with makeCacheMatrix(). 
## ... : extra parameters passed to the solve() function. 
## Returns: 
## The inverse of the matrix passed as an standard R matrix.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
