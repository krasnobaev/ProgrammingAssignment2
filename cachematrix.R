## makeCacheMatrix() - encapsulates access to cached and raw matrix
## cacheSolve()      - solve matrix with aware of cache

## processed and raw matrix encapsulation
## return - list of set/get functions to access matrix
##   rawm - raw matrix
makeCacheMatrix <- function(rawm = matrix()) {
    procm <- NULL

    ## raw matrix encapsulation
    set <- function(a) {
        rawm  <<- a
        procm <<- NULL
    }
    get <- function() rawm
    
    ## solved matrix encapsulation
    setProcessed <- function(b) procm <<- b
    getProcessed <- function()  procm

    list(set = set, get = get,
         setProcessed = setProcessed,
         getProcessed = getProcessed)
}

## solve matrix with aware of cache and write time matrix
## been processed
## return - matrix that is the inverse of x
##      x - list returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
    t <- proc.time()

    ## check whether matrix is processed
    m <- x$getProcessed()
    if(!is.null(m)) {
        message("getting cached data")
        message("Process Time: ", proc.time() - t)
        return(m)
    }

    ## cache miss
    m <- solve(x$get(), ...)
    x$setProcessed(m)
    message("Process Time: ", proc.time() - t)

    m
}
