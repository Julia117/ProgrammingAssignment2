## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrix_cache <- NULL
        set <- function(y) {
                x <<- y
                matrix_cache <<- NULL
        }
        get <- function() x
        setmean <- function(inverse) matrix_cache <<- inverse
        getmean <- function() matrix_cache 
        
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_cache <- x$getmean()
        if(!is.null(matrix_cache)) {
                message("getting cached data")
                return(matrix_cache)
        }
        data <- x$get()
        matrix_cache <- solve(data, ...)
        x$setmean(matrix_cache)
        matrix_cache
}
