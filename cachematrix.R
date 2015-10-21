
## These two functions are constructed to store a matrix and cache its mean.

## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse:
## There are 4 functions in "makeCacheMatrix":
## 1. set(): set the value of the matrix
## 2. get(): get the value of the matrix
## 3. setinverse(): set the value of the inverse matrix
## 4. getinverse(): get the value of the inverse matrix
 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## The "cacheSolve" function calculates the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
