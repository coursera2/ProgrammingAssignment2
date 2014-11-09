## R functions to construct a cached matrix and its inverse
## in order to avoid the computational complexity of computing an
## inverse each time it is needed
## Revision History: initial release November 2014 coursera2
## 
## makeCacheMatrix(matrix)
## constructor for matrix object with get/set and cached inverse
## returns specialMatrixObject list of function pointers
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)    
}

## cacheSolve(specialMatrixObject)
## function to compute and cache inverse of a matrix (if invertible)
## returns inverse of matrix in specialMatrixObject
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

