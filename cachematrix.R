# Two functions makeCacheMatrix and cacheInverse work together to cache the inverse of a matrix
# makeCacheMatrix returns a named list to set and get the input matrix and its inverse
# cacheInverse either returns the cached inverse if it exists or calculates the Inverse and Caches it.


#makeCacheMatrix returns a named list to get and set the value of x and its inverse.
makeCacheMatrix <- function(x = matrix()){
    
    # initalize inverse
    inv <- NULL                     
    
    # make set function to set x and reset the inverse to NULL
    set <- function(new_matrix) {   
        x <<- new_matrix
        inv <<- NULL
    }
    
    #make function to retrun x
    get <- function() x             
    
    #make setinv function to update inverse value
    setinv <- function(inverse) {   
        inv <<- inverse
    }
    
    #make getinv to return value of inverse
    getinv <- function() inv        
    
    #make named list
    list(set=set, get=get, setinv = setinv, getinv=getinv) 
}

data <- matrix(runif(9,5,10),3,3)
x <- makeCacheMatrix(data)



#make cacheInverse, a function that works with makeCacheMatrix ti return 
#the inverse of a matrix by getting it from cache or calculating it.
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
