## A pair of functions that when utilised together can solve the inverse
## of a matrix and also then cache this inverse to solve

##  Function that creates a matrix that can cache
##  its inverse, to save future computation by storing these
##  values as part of a list that can later be referenced

makeCacheMatrix <- function(x = matrix()) {
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


##  This function returns the inverse of the inputted matrix
##  utilising the makeCachematrix() function in order to minimise
##  multiple computations. i.e. It caches and retrieves matrixs
##  The cacheSolve() function first checks that no previously cached
##  value for that matrix exists, if so it returns it, otherwise it
##  performs the inverse calculation utilising "solve" and then caches it
##  inside the list made by makeCacheMatrix()

cacheSolve <- function(x, ...) {
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
