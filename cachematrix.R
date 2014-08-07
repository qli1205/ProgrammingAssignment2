## Put comments here that give an overall description of what your
## functions do
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsol <- function(sol) m <<- sol
    getsol <- function() m
    list(set = set, get = get,
         setsol = setsol,
         getsol = getsol)
}


## Write a short comment describing this function
## The function calculates the inverse of the matrix created by the function
## makeCacheMatrix. It checks whether the inverse has already been calculated,
## it gets the results from the cache and skips the calculation. Otherwise, it
## calculates the inverse of the matrix and sets the value of the inverse in the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsol()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsol(m)
    m
}
