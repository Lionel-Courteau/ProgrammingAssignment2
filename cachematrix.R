## Coursera R Programming Week 3: Programming Assignment 2: Lexical Scoping

## The purpose of this project is to write a pair of functions that cache the inverse
## of a matrix.



## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	j <- NULL
	set <- function(y){
		x <<- y
		j <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) j <<- inverse
	getInverse <- function() inverse
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
        if (!is.null(j)) {
            message("getting cached data")
            return(i)
        }
        mat <- x$get()
        j <- solve(mat, ...)
        x$setInverse(j)
        j
}
