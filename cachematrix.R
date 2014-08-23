## The first function takes a matrix as it's argument and contains a list of functions
## that will cache objects that can be accessed by the second function to calculate
## and cache the inverse of the matrix in the first argument or pull the inverse
## from cache if it has previously been calculated

## This function is a list of functions that will be accessed by cachceSolve 
##  along with a matrix as it's argument

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                       ## will store the inverse of the matrix and is reset to NULL when makeCacheMatrix is called 
        get <- function() x             ## function to return the original matrix              
        setinverse <- function(inverse) i <<- inverse   ## set the inverse of the matrix to cache on the first call from cacheSolve
        getinverse <- function() i                      ## gets the inverse of the matrix from cache when called by cacheSolve
        list(get = get, setinverse = setinverse, getinverse = getinverse) ## returned to cachSolve as arguments so the functions can be accessed by cacheSolve
}

## cacheSolve checks if inverse of matrix is in cache and returns if true, calculates inversre if false

cacheSolve <- function(x, ...) {
        i <- x$getinverse()     #gets inverse if cached, will be NULL if not
        if(!is.null(i)) {       #checks if inverse is cached
                message("getting cached data")  #displays message that inverse is in cache
                return(i)                       #returns inverse from cash, ends function
        }
        data <- x$get()         #if inverse is not in cache, gets matrix from cache
        i <- solve(data, ...)   #calculates inverse of matrix
        x$setinverse(i)         ## sets inverse into cache
        i               ## Return a matrix that is the inverse of 'x'
}
