# makeCacheMatrix creates a list, which is containing a function to:
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The following function returns the inverse of the matrix. 
# Firstly, it tries to retrieve the existing inverse matrix.
# Then, and checks if the inverse has already been computed. 
# If so, it gets the result, returns the inverse matrix and skips the computation.
# If not, it computes the inverse, sets the value in the cache via setInverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
