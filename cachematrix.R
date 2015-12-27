# makeCacheMatrix step. 
makeCacheMatrix <- function(x = matrix()) {
        cm <- NULL # cached matrix
        
        # get & set matrix
        set <- function(y) { x <<- y; cm <- NULL }
        get <- function() { x }
        
        # get & set cached matrix
        setInv <- function(inv) { cm <<- inv }
        getInv <- function() { cm }
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# cacheSolve step. 
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (! is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) # solve matrix inverse
        x$setInv(inv)
        inv
}