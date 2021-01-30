## Lionel Courteau
## Coursera R Programming Week 3: Programming Assignment 2: Lexical Scoping

## The purpose of this project is to write a pair of functions that cache the inverse
## of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  set_Inverse <- function(inverse) j <<- inverse
  get_Inverse <- function() j
  list(set = set, get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  j <- x$get_Inverse()
  if(!is.null(j)) {
    message("getting inversed matrix")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$set_Inverse(j)
  j
}
