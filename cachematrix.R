## makeCacheMatrix: creates a matrix, and caches the inverse.
## cacheSolve: computes the inverse of the special matrix
## returned by the makeCacheMatrix function.

## initialize set,get,setinv,getinv, caches the matrix

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set<-function(y){
        x<<-y
        s<-NULL
    }
    get<-function() x
    setinv<-function(inv) s<<-inv
    getinv<-function() s
    list(set=set, get=get,
         getinv = getinv,
         setinv = setinv)
}


## returns inverse of matrix 
## if inverse already exist in cache, skip calculation

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)){
        print("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setinv(s)
    s
}

