## This function caches a time-consuming computation.
## It contains two parts: the first part creates a matrix, 
## including the caching option; the second part computes 
## the inverse of the matrix while checking the cache.


## Creates matrix as a 'special list' with 4 elements
## (set, get, setInverse, getInverse) and cache in new
## environment. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                               # set cache to zero
        set <- function(y) {                                    # assign cache to new environment
                x <<- y
                m <<- NULL
        }
        get <- function() x                                     # get matrix
        setInverse <- function(solve) m <<- solve               # use solve for inverse, assign to cache
        getInverse <- function() m                              # get cache 
        list(set = set, get = get,  
              setInverse = setInverse, getInverse = getInverse) # make list
}

## Computes inverse of matrix only if it isn't stored yet
## in cache m.  

cacheSolve <- function(x, ...){
        m <- x$getInverse()                             # query cache
        if(!is.null(m)) {                               # if not empty
                message("getting cached data")          # print message
                return(m)                               # no computation needed, return cache
        }
        data <- x$get()                                 # if empty, get matrix
        m <- solve(data, ...)                           # calculate inverse
        x$setInverse(m)                                 # save to cache
        m                                               # return inverse matrix
}
