## For a big matrix, it may take too long to compute the inverse.
## If the contents of a matrix are not changing, it may make sense
## to cache the value of the inverse so that when we need it again,
## it can be looked up in the cache rather than recomputed. 
## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse:
## 
## makeCacheMatrix: This function creates a special "matrix" object
##                  that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
##             returned by makeCacheMatrix above. If the inverse has
##             already been calculated (and the matrix has not changed),
##             then cacheSolve should retrieve the inverse from the cache.


## Given a matrix x, function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. This special "matrix" object is really a list
## containing the following functions:
## 
##     set(y): set x to a given matrix y
##     get():  get the matrix x
##     setinverse(v): set the inverse i of matrix x to v
##     getinverse(): get the inverse i of matrix x
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(v) i <<- v
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special object created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the matrix and sets
## the inverse of the matrix in the cache via the setinverse function.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}



## Some testing
##
## x <- matrix(c(1, 0, 0, 0, 2, 0, 0, 0, 4), c(3, 3))
## xv <- makeCacheMatrix(x)
## cacheSolve(xv)
## cacheSolve(xv)
## cacheSolve(xv)

