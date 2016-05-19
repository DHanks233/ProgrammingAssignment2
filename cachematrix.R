## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## Pre: initialMatrix parameter is a square & invertible matrix
## Post: None
## Parameter: initialMatrix - A square & invertible matrix
## Returns: A list that contains the functions which allows the
##	    saved state of the funciton environment to accessed
## Description: This function takes a matrix, creates functions
##		to get, set, set the inverse and get the inverse
##		of that matrix, which it then stores in a list that
##		is returned. The list can be used by cacheSolve()
##		to access the saved state of this function's environment

makeCacheMatrix <- function(initialMatrix = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                initialMatrix <<- y
                inverseMatrix <<- NULL
        }
        get <- function() initialMatrix
        setinverse <- function(solve) inverseMatrix <<- solve
        getinverse <- function() inverseMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
## Pre: cacheMatrix is an object created with the makeCacheMatrix
##	function (a list of functions)
## Post: None
## Parameter: cacheMatrix - An object created by makeCacheMatrix
## Returns: A matrix that is the inverse of the original matrix used
##	    as a parameter in makeCacheMatrix
## Description: This function uses the saved state of the makeCacheMatrix
##		environment to read an inverse matrix. inverseMatrix is
##		becomes part of the saved state of this environment so
##		it can be accessed later as a quick cache.

cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- cacheMatrix$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- cacheMatrix$get()
        inverseMatrix <- solve(data, ...)
        cacheMatrix$setinverse(inverseMatrix)
        inverseMatrix
}