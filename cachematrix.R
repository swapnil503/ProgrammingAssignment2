### Assignment: Caching the Inverse of a Matrix
	

	##Below is a pair of functions that cache the inverse of a matrix such 
	##that when needed again, it can be looked up in the cache rather 
	##than recomputed.
	## 'makeCacheMatrix': This function creates a special "matrix" object that can cache its inverse.
	

	makeCacheMatrix <- function(x = matrix()) {
	        inv <- NULL
	        set <- function(y) {
	          x <<- y
	          inv <<- NULL
	        }
	        get <- function() x
	        setInverse <- function(inverse) inv <<- inverse
	        getInverse <- function() inv
	        list(set = set,
	             get = get,
	             setInverse = setInverse,
	             getInverse = getInverse)
	      }
	

	

	

	## 'cacheSolve' computes the inverse of the special
	##"matrix" returned by 'makeCacheMatrix' above. If the inverse has
	##already been calculated (and the matrix has not changed), then
	##'cacheSolve' should retrieve the inverse from the cache.
	

	cacheSolve <- function(x, ...) {
	        ## Return a matrix that is the inverse of 'x'
	        inv <- x$getInverse()
	        if (!is.null(inv)) {
	          message("getting cached data")
	          return(inv)
	        }
	        invertible_matrix <- x$get()
	        inv <- solve(invertible_matrix, ...)
	        x$setInverse(inv)
	        inv
	}
	

	# Testing the functions
	

	# > getwd()
	# [1] "/Users/Jane/Documents/GitHub/ProgrammingAssignment2"
	# > setwd("~/Documents/GitHub/ProgrammingAssignment2")
	# > source("~/Documents/GitHub/ProgrammingAssignment2/cachematrix.R")
	# > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
	# > my_matrix$get()
	# [,1] [,2]
	# [1,]    1    3
	# [2,]    2    4
	# > my_matrix$getInverse()
	# NULL
	# > cacheSolve(my_matrix)
	# [,1] [,2]
	# [1,]   -2  1.5
	# [2,]    1 -0.5
	# > my_matrix$getInverse()
	# [,1] [,2]
	# [1,]   -2  1.5
	# [2,]    1 -0.5

