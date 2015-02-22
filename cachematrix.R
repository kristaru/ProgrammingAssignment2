## Following functions are written to cache the inverse of matrix
## for saving time and resources, because computing of matrix inversion 
## can be long process. 

## Write a short comment describing this function
## makeCacheMatrix consists of the following steps:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix 
##get the value of the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## Function "cacheSolve" first of all will check if inversion 
##was already has been computed, if not, then it will compute it, otherwise 
## it will skip.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
