## makeCacheMatrix creates a special "matrix" object that also contains a
## cache of the inverse matrix. cacheSolve checks the object created by 
## makeCacheMatrix to see if a there is a cached matrix and if not, 
## calculates the inverse of the matrix.

## creates "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    v<-NULL
    set<-function(y){
        x<<- y
        v<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) v<<-inverse
    getinverse <- function()v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks if there is a cached inverse, if not calculates the inverse of 
## the matrix

cacheSolve <- function(x, ...) {
    v<- x$getinverse()
    if(!is.null(v)){
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data)
    x$setinverse(v)
    v
}
