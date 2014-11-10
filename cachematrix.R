## R functions to construct a cached matrix and its inverse
## in order to avoid the computational complexity of computing an
## inverse each time it is needed
## Revision History:    initial release November 2014 coursera2
##                      added tests on matrix dimensions/content and a test func
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
        mydim <- dim(data)
        #stop condition here is sort of input checking det already does
        if (length(mydim)!=2 | nrow(data)!=ncol(data)) {
                stop("input must be square matrix")
        }
        mydet <- det(data)
        #Note that warnings will not be reissued when data is cached
        #warnings here are a little better than errors thrown by solve
        if (is.na(mydet) | is.nan(mydet) | is.infinite(mydet)) {
                warning("input matrix has invalid value(s)")
                i <- NA
        } else if (mydet==0) {
                warning("singular matrix has no inverse")
                i <- NA
        } else {
                i <- solve(data, ...)
        }
        x$setinv(i)                
        i
        ## Return a matrix that is the inverse of 'x'
}

# testCachedMatrix(number_of_tests, random_seed, max_matrix_dim)
# function to exercise the above functions for testing num times
# returns Boolean on whether test appears to have passed
# expected output with no args: 10 getting cached data lines & return code TRUE
testCachedMatrix <- function(num=10, seed=1, maxmdim=10) {
        set.seed(seed) # reproducible; for random try seed=unclass(Sys.time())
        rcode <- TRUE
        for (ix in 1:num) {
                mdim <- sample.int(maxmdim,1)
                mdsq <- mdim * mdim
                myx <- makeCacheMatrix() # use bare constructor
                myx$set(matrix(sample(-mdsq:mdsq,mdsq,replace=TRUE),mdim,mdim)) # use method to set
                myy <- cacheSolve(myx)
                myy <- cacheSolve(myx)   # repeat to print the cached usage line
                myx$setinv(myy)          # use method to set matrix inverse
                imatrix <- myx$get() %*% myx$getinv()
                mydet <- signif(det(imatrix),12) # my platform has 12 sig digits
                if (mydet!=1) {
                        print(imatrix)
                        print(as.character(mydet)) # my print default is 7 digs; show precision as string
                        warning("rough check for identity matrix result failed")
                        rcode <- FALSE
                }
        }
        rcode
}
