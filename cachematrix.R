## makeCacheMatrix(x) : This function creates an object that stores the value of 
## the matrix and cache its inverse.
## cacheSolve(x1): This function will compute the inverse of the matrix.It takes 
## the argument that returned by the above function.If the inverse has already 
## been calculated (and the matrix has not changed) then this function returns 
## the inverse from cache.

## This function does four different operations and returned in the form of list
## ,the operations are set and get the value of matrix and set and get the value
## of the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvers <- function(invers) inv <<- invers
        getinvers <- function() inv
        list(set=set,get=get,setinvers=setinvers,getinvers=getinvers)
}


## This function calculates the inverse of the matrix created with the above 
## function. If the inverse has already been calculted then it gets the inverse
## from cache.Otherwise, it will compute the inverse of the matrix and sets the 
## value in the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinvers()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinvers(inv)
        inv
}
