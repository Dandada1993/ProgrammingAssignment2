library(matrixcalc) #library is needed to get the is.square.matrix function

#In absence of the matrixcalc library the following function can be used to determine if a matrix is square.
#It checks that the matrix is a 2 dimensional matrix and that the number of rows and columns are equal.

issquare <- function(x = matrix())
{
    dimensions <- dim(x)
    if (length(dimensions) == 2 & (dimensions[1] == dimensions[2]))
    {
        TRUE
    }
    else
    {
        FALSE
    }
}

#This function returns an object that wraps a matrix and provides the following functions:

#Get: Return the matrix
#Set: Changes the matrix and reinitialised the inverse to NULL
#GetInverse: Get the cached inverse of the matrix or NULL
#SetInverse: Sets the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix created by the makeCacheMatrix function
## If returns the cache inverse, if the cached inverse is NULL, it calculates the inverse
## store it in x and returns it to the caller if and only if the matrix stored is square.
## The function is.square.matrix in the matrixcalc library is utilised. In the absence of 
## that libary the above issquare function can be used.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i))
    {
        message("getting inverse from cache")
        return( i)
    }
    m = x$get()
    i <- if(is.square.matrix(m)) #i <- if(issquare(m))
    {
        solve(m)
    }
    else
    {
        stop("Matrix is not square")
    }
    x$setinverse(i)
    i
}
