## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.
# This function creates an especial object class Matrix which cache his inverse.
#And save the object  in makeCacheMatrix. 

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
#This function, cacheSolve compute the output of the later function, and if is necesary compute the inverse of the Matrix.
# If this inverse already was calculated, only bring the object. 
#Is a function which tries of make easier this tough process.

cacheSolve <- function(x, ...)  {
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
# Sorry about my English, I'm trying my best.