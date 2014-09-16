## The script below return the inverse of a matrix from cache and in case it 
## doesn't exit computes it at put it cache


## The `makeCacheMatrix` function creates a special "matrix", which in fact is
## a list containing a function to:
##      1.  set the matrix
##      2.  get the matrix
##      3.  set the inverse of the matrix
##      4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##  Return a matrix that is the inverse of 'x'
##  The `cacheSolve` function calculates the inverse of the special "matrix"
##  in case that it hasn't already been calculated and adds to cache via the 
## `setinverse` function. Otherwise, it get`s the inverted matrix from the
## cache and skips the computation.

cacheSolve <- function(x, ...) {
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