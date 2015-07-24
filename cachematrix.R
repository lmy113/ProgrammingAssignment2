## These two functions make and cache the inverse of a matrix object


## This function creates a matrix using set and get that is cached.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<- y
    m<<- NULL
  }
  get<-function() x
  setmatrix<-function(inverse) m<<- inverse
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix, getmatrix=getmatrix)
}

## This function takes and caches the inverse of the special matrix.  
## If it exists inverse not recalculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
