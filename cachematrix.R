## 
## makeCacheMatrix and cacheSolve
## Together, these functions create and access an object that serves
## as a wrapper for a matrix, allowing the inverse of the matrix to be 
## cached in the object, to prevent the need for its recalculation.
## Sarah Clatterbuck Soper, 2014.08.22

## makeCacheMatrix
## -Takes an invertible matrix as an argument
## -Returns a cache object that consists of a list of four 
##  functions that access the matrix and
##  read and write a cached matrix inversion

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverted) inverse <<- inverted
        getinverse <- function() inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve
## -takes cache object created by makeCacheMatrix as an argument
## -queries the object returned by makeCacheMatrix to determine if the
##  matrix inverse is cached.  If it is, returns the cache
## -otherwise, calculates the inverse and stores it in the object
## -returns the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Fetching cached matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}