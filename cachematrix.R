## This is the updated codes for Week 3 of Course 2 R Programming of Data Sciences Coursera Class
## The assignment is to write 2 functions to calculate the inverse of a matrix in a more efficient
## way by caching the inverse of a matrix

## This function creates a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## The following function calculates the inverse of the special list created with the 
## above function. It first checks to see if the inverse has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
