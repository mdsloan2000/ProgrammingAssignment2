## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## MDS - Comments - The following function accepts a matrix.  It also provides
## creates a list that contains a function to set and get the value of the 
## matrix, and solve the mean.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                    ## creates set function
                x <<- y                         ## modifies x in upper env 
                m <<- NULL                      ## modifies m in upper env 
        }
        get <- function() x                        ## define get function
        setsolve <- function(solve) m <<- solve    ## define setsolve 
        getsolve <- function() m                   ## define getsolve 
        list(set = set, get = get,                 ##returns list of functions
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

##  This function caclulates the source of the matrix returned above.  If the 
##  value contains the inverse calculation, it get's the cached value (and
##  uses the message function to post the notice in red notice in red).  
##  Note that I heaviliy leveraged the functionality of the example to create
##  the version below (after making sure I understood how it worked).
##  I also looked to ensure that the function returned an error for a singular
##  entry per the source function and it does.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()                       ##  Gets M
        if(!is.null(m)) {                       ##  If M has non-null then get cached data
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         ##  If M is NULL then it calculates solve
        m <- solve(data, ...)                   ##  And stores m
        x$setsolve(m)                           ##  Caches m so when re-run, it is not calc'd
        m                                       ##  Returns m - the inverse of matrix.
        
}
