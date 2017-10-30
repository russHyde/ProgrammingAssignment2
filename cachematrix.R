## Put comments here that give an overall description of what your
## functions do

#' makeCacheMatrix - Construct a matrix that caches it's inverse upon
#' calculation
#'
#' @param        x             A numerical matrix.
#' @return       A list containing functions `set`, `get`, `setinv` and `getinv`
#' 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

#' cacheSolve - Obtain the inverse-matrix for a given cacheMatrix
#'
#' @param        x             A cacheMatrix as constructed using
#'   `makeCacheMatrix`
#' @return       The matrix-inverse corresponding to the input matrix.
#' 
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
