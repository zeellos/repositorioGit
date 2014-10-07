makeCacheMatrix <- function(x = matrix()) {
	# Created by: zeellos
	# Date: 22/08/2014
	# Modified: 
	# Description:
	# This function creates a special "matrix" object that 
	# can cache its inverse.
	#  1. set the value of the matrix
    #  2. get the value of the matrix
    #  3. set the value of the inverse matrix
    #  4. get the value of the inverse matrix
	
	invMatrix <- NULL
	
	set <- function(y){
		x <<- y
		invMatrix <<- NULL
	}
	get <- function() x
	setInvMatrix <- function(invMat) invMatrix <<- invMat
	getInvMatrix <- function() invMatrix
	list( set = set,
		  get = get,
		  setInvMatrix = setInvMatrix,
		  getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        	invMatrix <- x$getInvMatrix()
			if (!is.null(invMatrix)){
				message("getting cache data")
				return(invMatrix)
			}
			data <- x$get()
			invMatrix <- solve(data)
			x$setInvMatrix(invMatrix)
			invMatrix
}
