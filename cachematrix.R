## makeCacheMatrix : Create a special Matrix object witch cache its inverse
## cacheSolve : Computes the inverse of the special "matrix" returned by makeCacheMatrix

## Create a special Matrix object witch cache its inverse
## Return a list containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        setInverse <- function(solveMatrix) inverse <<- solveMatrix
        ## get the value of the inverse
        getInverse <- function() inverse
        ## Return a list containing the functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                ## Return the cached data (matrix that is the inverse of 'x')
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse     
}