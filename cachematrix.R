## Put comments here that give an overall description of what your
## functions do

## The two functions makeCacheMatrix and cacheSolve together make use of 
## the lexical scoping rules to store pre-computed objcects in a sort of
## cache, within a function. 

## The makeCacheMatrix function creates a special kind of matrix that has
## four associated functions called get, set, getInverse and setInverse
## The difference from a standard matrix is that this matrix will typically be 
## used to store large matrices, whose inverse needs to calculated often.
## To save on computation, the inverse of this matrix will be calculated using the 
## function cacheSolve, which will calculate the inverse only once - i.e. the first time 
## it is called, and will store the copmuted inverse in this special matrix object.

makeCacheMatrix <- function(x = matrix()) {
	my_inverse <- NULL
        set <- function(y) {
                x <<- y
                my_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) my_inverse <<- solve
        getInverse <- function() my_inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## For the special matrix object created using the makeCacheMatrix function,
## the inverse will be calculate using this function cacheSolve. The difference 
## from a standard solve function is that the cacheSolve will first check if a pre-computed
## inverse already exists inside the special matrix object and return it, otherwise will 
## calculate the inverse (using solve) and the store it in the special matrix object for 
## all future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the 
        ## special matrix 'x' that has been created using the makeCacheMatrix function
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)	# Step1. Calculate inverse
        x$setInverse(inverse)	#Step2. Store it in x for future use
        inverse	#Step3. Return the inverse for current use
}
