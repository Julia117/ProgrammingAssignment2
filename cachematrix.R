## Next two functions allow us to calculate the inverse of a matrix 
## and save it to a special matrix not to comptute it twice

## The following function allows to save the inverse of a matrix to matrix_cache
## and the to get it from cache

makeCacheMatrix <- function(x = matrix()) {
        matrix_cache <- NULL
        set <- function(y) {
                x <<- y
                matrix_cache <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_cache <<- inverse
        getinverse <- function() matrix_cache 
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function first checks if the reverse has already been calculated,
## if not --- it gets the matrix_cache matrix and saves the inverse there

cacheSolve <- function(x, ...) {
        matrix_cache <- x$getinverse()
        if(!is.null(matrix_cache)) {
                message("getting cached data")
                return(matrix_cache)
        }
        data <- x$get()
        matrix_cache <- solve(data, ...)
        x$setinverse(matrix_cache)
        matrix_cache
}
