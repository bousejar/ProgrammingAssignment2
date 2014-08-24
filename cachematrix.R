##Overall description: makeCacheMatrix(X) makes "matrix" object that can cache value of its inverse
##                     if we call cacheSolve() with such an object we get its inverse. 
##                                                --If the inverse has already been calculated (and the matrix has not changed), then the inverse is retrieved from the cache.

## makeCacheMatrix (x= matrix())
## Function creates "Matrix"object that can cache value of its inverse 
## Object "X<-makeCacheMatrix(A)" can be called directly by commands:
##              X$get()                - returns original matrix 'A'
##              X$getInverse()         - returns inverse of original matrix
##              X$set(Y)               - changes the original matrix by matrix 'Y'
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (y) {  ## 
                x <<- y
                inverse <<- NULL
        }
        get <- function() x     ## return the value of original matrix
        setInverse <- function(x) inverse <<- solve(x)  ## Private function to calculate inverse of matrix
        getInverse <- function(){
                ## If doesn't exist, calculate inverse matrix (setInverse()), otherwise get cached data
                if (is.null(inverse)) { setInverse(x)} else {message("getting cached data")}
                inverse}
        list(set = set, get = get, getInverse= getInverse)
}


##cacheSolve <- function(x, ...)
##returns inverse matrix - if already calculated in the past for given matrix, cached data is given.
##Decision whether calculate or use cached inverse matrix was moved to the function makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        inverse        
}
