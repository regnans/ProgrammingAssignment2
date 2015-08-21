## These two functions create a matrix and calculate it's inverse. 
## The matrix inverse is cached and can be retrieved.

## create a matrix and the ability to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    k <- NULL
    set<- function(y) {
        x<<-y
        k <<- NULL
    }
    get <- function() x
    setinv <- function(solve) k <<- solve
    getinv <- function () k
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes inverse of matrix, or retrieves inverse 
## from cache if already calculated. Returns matrix. 

cacheSolve <- function(x, ...) {
        k <- x$getinv()
        if(!is.null(k)) {
            message("getting cached matrix")
            return(k)
        }
        matrix <- x$get()
        k <- solve(matrix)
        x$setinv(k)
        k
}
