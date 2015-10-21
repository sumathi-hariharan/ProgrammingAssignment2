# Matrix inversion is a tedious function and its rather simple 
# to cache the inverse of a matrix instead of computing it repeatedly. The
# following are the two functions  used to cache the inverse of a matrix.
# makeCacheMatrix creates a list to set and get the values of matrix and also for inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# function to return inverse of the matrix. It checks for inverse and gets the output.
# if inverse is not performed, then setinverse function is used to compute the values and set in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
#Output:
x = rbind(c(1, -1/4), c(-1/4, 1))
> m = makeCacheMatrix(x)
> m$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00

> cacheSolve(m)
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667
