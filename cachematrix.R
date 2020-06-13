## Put comments here that give an overall description of what your
## functions do
# The set of two functions are to save the time & resources for spare
# computations of inverse matrix by caching previously computed inverse of
# a matrix rather than compute it repeatedly.
# It is assumed that the matrix supplied is always invertible.

## Write a short comment describing this function
# The makeCacheMatrix function creates a special "matrix" object
# that can cache its inverse
# Basically it is a list containing a function to
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse matrix
        # 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# The cacheSolve function calculates the inverse matrix of the special "matrix"
# returned by makeCacheMatrix function above.
        # If the inverse has already been calculated (and the matrix has not
        # changed), then the cacheSolve should retrieve the inverse from the cache
        # Otherwise, it calculates the inverse matrix on the updated data and
        # sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# check the results
m <- matrix(rnorm(25), 5, 5)
m
solve(m)
m.upd <- makeCacheMatrix(m)
cacheSolve(m.upd)
