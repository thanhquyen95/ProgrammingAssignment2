## Create a function that can get a matrix's inverse

## Functions to set/get value, set/get matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## cached inverse of matrix
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    getinv <- function() inv
    setinv <- function(inverse) inv <<- inverse
    list(get=get,set=set,getinv=getinv,setinv=setinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <-x$getinv()
    if(!is.null(inv)){
        message("inverse is cached")
        return(inv)
    }
    m <- x$get()
    inv<- solve(m,...)
    x$setinv(inv)
    return(inv)
}
