## Put comments here that give an overall description of what your
## functions do

##inverse of matrix is calcualted using the solve function (expensive calculation) in the cacheSolve ONLY IF 
## the inverse is NOT already cached

## to test 
## mat <- matrix(1:4, 2,2)
## t<- makeCacheMatrix(mat)
## cacheSolve(t) ## prints only the inverse
## cacheSolve(t) ## second time should print the  message "getting cached data" while printing the inverse


## Write a short comment describing this function
## cacheSolve uses the output of this function makeCacheMatrix
## Essentially a basic function with setters and getters that accesses the parent environment's inv through "<<-"


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## makeCacheMatrix is passed to this function cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) #solve is used to get the inverse of a square matrix
    x$setinverse(inv)
    inv
}
