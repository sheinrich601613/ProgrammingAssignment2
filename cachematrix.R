## Two functions that together cache the inverse of a matrix.


## Creates a special "matrix" object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- NULL
  set <- function(y) {
    x <<- y
    inverse_mat <<- NULL
  }
  get <- function(){
    x
  } 
  setinv <- function(inv){
    inverse_mat <<- inv
  } 
  getinv <- function() {
    inverse_mat
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## computes inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_mat<-x$getinv()
  if(!is.null(inverse_mat)){
    message("getting cached data")
    return(inverse_mat)
  }
  mat<-x$get()
  inverse_mat<-solve(mat,...)
  x$setinv(inverse_mat)
  inverse_mat
}

