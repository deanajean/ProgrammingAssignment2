
#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
    set <- function(y) {
    x <<- y
    inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean 
## from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets
## the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        # check if the inverse exists, if so return cached value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    # if the inverse doesn't exist, get it
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv    
}

## Test Results
## > m <- matrix(c(6,1,7,6,4,5,9,0,8),3,3)
##> a <- makeCacheMatrix(m)
##> cacheSolve(a)
##          [,1]        [,2]       [,3]
##[1,] -0.5079365  0.04761905  0.5714286
##[2,]  0.1269841  0.23809524 -0.1428571
##[3,]  0.3650794 -0.19047619 -0.2857143

## repeat
##> cacheSolve(a)
##getting cached data
##           [,1]        [,2]       [,3]
##[1,] -0.5079365  0.04761905  0.5714286
##[2,]  0.1269841  0.23809524 -0.1428571
##[3,]  0.3650794 -0.19047619 -0.2857143
