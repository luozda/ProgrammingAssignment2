## The first function, "makeCacheMatrix" create a special "vector", which is really a list
## containing a set of functions:
##      set.vector: set the value of the vector
##      get.vector: get the valute of the vecor
##      set.inverse: set the inverse of a matrix
##      get.inverse: get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set.vector <- function(y) {
        x <<- y
        m <<- NULL
    }
    get.vector <- function() x
    set.inverse <- function(inverse) m <<- inverse
    get.inverse <- function() m
    
    list(set.vector  = set.vector,  get.vector  = get.vector,
         set.inverse = set.inverse, get.inverse = get.inverse)
}


## The following calculates the mean of the special "vector" created with the above function.
## It first checks to see if the inverse has already been done. if so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the "set.inverse" function.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$get.inverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get.vector()
    m <- solve(data, ...)
    x$set.inverse(m)
    m
}
