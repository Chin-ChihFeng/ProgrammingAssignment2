## Put comments here that give an overall description of what your
## functions do

## It is used to crate a matrix that can be stroed in the memery.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(cachematrix) m <<- cachematrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        # Check on the matrix is NULL either or not.
        if(!is.null(m)) {
        # Determine the matrix, whether it can be inverse.
        # If data exist and can be inverse, then put into the memery.
                
                if(det(m) != 0) { 
                message("getting cached data")
                return(m)
                }
        }
        data <- x$get()
        if(det(data) == 0) {
        print("This matrix is not invertible.")
        }else {
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        }
}
