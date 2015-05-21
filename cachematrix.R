## Overall: setup two functions that:
##  a) create a special "matrix" object that can cache its inverse
##  b) IF inverse has already been calc'ed, retrieve from cache
##  c) ELSE compute the inverse of the "matrix" 


## FUNCTION 1 ----------------
## @makeCacheMatrix: return "list' with functions to:
##  a) set & get the matrix
##  b) set & get the inverse of said matrix 
##  ...list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
## @x - a 'square invertible matrix'
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    seti <- function(inverse) i <<- inverse
    geti <- function() i
    list (set=set, get=get, seti=seti, geti=geti)
}


## Function 2 ----------------
## @cacheSolve: return inverse of matrix (x) - or, retrieve from cache

cacheSolve <- function(x, ...) {
    ## @x - output of makeCacheMatrix() ... takes "x"
    i <- x$geti()
    
    ## if the inverse has already been calculated
    if(!is.null(i)){
        # get it from the cache and skips the computation
        message("getting cached data")
        return(i)
    }    
    
    ## else: calc the inverse
    mat.data <- x$get()
    i <- solve(mat.data, ...)
   
    ## sets the value of the inverse in the cache via the seti function
    x$seti(i)
    
    return(i)
}
    
##END ----------------    