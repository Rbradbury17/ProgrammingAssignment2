## makeCacheMatrix and cacheSolve are two functions which allow the inverse of a matrix to be stored
## once calculated so that it doesn't need to be calculated multiple times which saves time.

## The function makeCacheMatrix takes a matrix 'x' and returns a list of the following functions:
## 'set' changes the stored matrix and resets the inverse.
## 'get' returns the stored matrix.
## 'setinverse' stores the value of the input into the variable 'inv'
## 'getinverse' returns the value of 'inv'
## The latter two functions are used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve takes the value of getinverse (of x) from the previous function and
## checks whether the inverse of x has already been calculated (and set). If it has, it
## just returns the value of this inverse along with the message "getting cached data".
## If not, it calculates the inverse of the matrix using the 'solve' function and stores this
## via the function setinverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

