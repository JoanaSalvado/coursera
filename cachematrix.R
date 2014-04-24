
## The first function makeCacheMatrix creates special "matrix" object that 
## can cache its inverse
## The second function, cacheSolve,  returns an inverse matrix of 
## a given matrix


## makeCacheMatrix creates a list of four functions:  
## set: puts the given matrix in some other environment than the workspace
## get: returns the given matrix
## setInv: puts the inverse in that environment
## getInv: returns the inverse of the given matrix
## The point of this function is to avoid using the memory directly,
## caching the information how to build the given matrix and it inverse


makeCacheMatrix <- function(x = matrix()) {
        
        M <- NULL
        
        
        set <- function(y) {
                x <<- y
                M <- NULL
        }
        
        get <- function() x
        
        setInv <- function(solve) M<<-solve
        getInv <- function () M
        
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)

}



## This function actually calculates the inverse of the given matrix
## using the function created on makeCacheMatrix.
## If the matrix M (the "cached" matrix) is not null it will then calculate
## the inverse of the given matrix

cacheSolve <- function(x, ...) {
        
        M <- x$getInv()
        
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        
        getX <- x$get()
        M<-solve(getX, ...)
        x$setInv(M)
        M
        
}
