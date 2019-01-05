## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

