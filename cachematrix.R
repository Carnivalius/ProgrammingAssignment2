## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    '''
    Function that creates a matrix that can cache
    its inverse, to save future computation
    '''
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(solve) m <<- solve
    getmean <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    '''
    This function returns the inverse of the inputted matrix
    utilising the makeCachematrix() function in order to minimise
    multiple computations. i.e. It caches and retrieves matrixs
    '''    
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
