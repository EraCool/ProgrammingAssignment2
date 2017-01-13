
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        in_verse <- NULL
        set <- function(y) {
                x <<- y
                in_verse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) in_verse <<- inverse
        getinverse <- function() in_verse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}





# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieves the inverse from the cache

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        in_verse <- x$getinverse()
        if(!is.null(in_verse)) {
                message("getting cached data.")
                return(in_verse)
        }
        data <- x$get()
        in_verse <- solve(data)
        x$setinverse(in_verse)
        in_verse
}
