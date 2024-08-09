## Put comments here that give an overall description of what your
## functions do
#Overall these functions will be able to calculate and store the inverse of a 
# matrix within an object so that the inverse matrix will not have to be
# calculated every time it is needed

## Write a short comment describing this function
# This function takes in a matrix and creates a new object that can have its inverse cached
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# Computes the inverse of the object type returned by makeCacheMatrix, or retrieves
# the inverse if it has already been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
                #message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
