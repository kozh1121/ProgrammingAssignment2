## Function creates matrix list contaning functions for matrix inversion
makeCacheMatrix <- function(m = matrix()){
        inv <- NULL
        ## function sets the value of the matrix to the variable m in the 
        ## shared environment (cache)
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        
        ## function returns the matrix that was set to m
        get <- function() m
        
        ## function assigns inverts the matrix and assignes inverted matrix to the
        ## variable inv in cache
        setInv <- function(solve)  inv <<- solve(m)
        
        ## function returns invertes matrix stored in the variable m 
        getInv <- function() inv
        
        ## getting the result list together
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## Function return inverted maxtrix from cache or calculates it 
## if chached value is empty 
cacheSolve <- function(m, ...){
        inv <- m$getInv()
        
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data)
        m$setInv(inv)
        inv
}