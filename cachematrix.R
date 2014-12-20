

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
        x <<-ls- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function opens the matrix, stores it in cashe memory, and computes a mean

cachesolve <- function(x, ...) {  #x is an object from makeVector
    i <- x$getsolve() #when called, first gets mean of x (from makecvvector)
    
    #if is cached value, then...
    if(!is.null(i)) {
        message("getting cached data")
        return(i)   #and then returns the mean
    }
    
    #if no cached data, gets the original fector (is x$getmean() is NULL)
    data <- x$get()
    # calculate mean
    i <- solve(data, ...)
    #set mean value back in object
    x$setsolve(i)
    #also return the mean, duh. 
    i
}