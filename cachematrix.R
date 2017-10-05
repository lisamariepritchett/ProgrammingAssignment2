# Two functions makeCacheMatrix and cacheInverse work together to cache the inverse of a matrix
# makeCacheMatrix returns a named list to set and get the input matrix and its inverse
# cacheInverse either returns the cached inverse if it exists or calculates the Inverse and Caches it.


#makeCacheMatrix returns a named list to get and set the value of x and its inverse.
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    
    # create get and set functions
    set <- function(new_matrix) {   
        x <<- new_matrix
        inv <<- NULL
    }
    get <- function() x             
    setinv <- function(inverse) {   
        inv <<- inverse
    }
    getinv <- function() inv        
    
    #make named list
    list(set=set, get=get, setinv = setinv, getinv=getinv) 
}



# cacheInverse works with makeCacheMatrix t0 return the inverse of a matrix 
# by getting it from cache or calculating it.
cacheInverse <- function(x, ...){
    #get inverse from the named x-list
    inv <- x$getinv()
    
    # if inverse had already been cached use it
    if(!is.null(inv)) {
        message("Getting cached Inverse")
        return(inv)
    }
    
    #otherwise, get the data, calculate its inverse, cache the value and return
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
