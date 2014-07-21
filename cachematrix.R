## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function 'makeCacheMatrix' creates a special "matrix", which is
## really a list containing 4 subfunctions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inM <- NULL
    set <- function(y){
        x <<- y
        inM <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inM <<- solve
    getInverse <- function() inM
    list(set=set,get=get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix"
## generated with the above function. It first checks to see if the
## inverse matrix has already been calculated. If so, it 'get's the
## inverse matrix from the cache and skips the calculation. Otherwise,
## it calculates the inverse matrix and 'set's the inverse matrix by
## 'setinverse'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inM <- x$getInverse
    if(!is.null(inM)) {
        message("getting cached data")
        return inM
    }
    matrix1 <- x$get()
    inM <- solve(matrix1,...)
    x$setInverse(inM)
    inM
}
