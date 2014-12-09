makeCacheMatrix <- function(x = matrix()) {
    
    inv_X <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    setinvmatr <- function(inv) inv_X <<- inv
    getinvmatr <- function() inv_X
    list(set = set, get = get, setinvmatr = setinvmatr, getinvmatr = getinvmatr)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinvmatr()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$setinvmatr(inv_x)
    inv_x
}