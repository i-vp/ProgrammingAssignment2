## These 2 functions cache the inverse of a matrix

## This function creates a special "matrix" object that can chache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
              s <- NULL
              set <- function(y) {
                x <<- y
                s <<- NULL
              }
              get <- function() x
              setsolve <- function(solve) s <<- solve
              getsolve <- function() s
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix function above

cacheSolve <- function(x, ...) {
            s <- x$getsolve()
            if(!is.null(s)) {
              return(s)
            }
            data <-x$get()
            s <- solve(data, ...)
            x$setsolve(s)
            s
        ## Return a matrix that is the inverse of 'x'
}
