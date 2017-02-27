## Special Matrix type that can store its inverse
## Run cachesolve to solve for inverse
## Checks for inverse before solving

## Modeled after the example in the assignment
## set and get inverse
## set and get value of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## checks to see if there is an inverse stored within "matrix"
## solves for and returns inverse afterward if not cached

cacheSolve <- function(x, ...) {
        
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
