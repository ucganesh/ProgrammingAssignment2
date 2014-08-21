## This set of functions caches inverse of a matrix and provides access to the
## cached data and another one get cached copy on inverse of a matrix. If cached
## data is not available then computes the inverse and caches the data for future
## use

## Provides functions to store retrieve original data and cached value

makeCacheMatrix <- function(x = matrix()) {
		##Initialize a variable to cache inverse matrix
        inverseMat <- NULL
		
		##Creating a subfunction  to change matrix
        set <- function(maty) {
				##Super assign the new matrix to input matrix,
				## Super assignment is done if the scope of the variable to
				## modify is outside the scope of current context. i.e. matx
				## is available as part of the function makeCacheMatrix and not
				## available in set function.
                matx <<- maty 
				
				## remove cached information as the input matrix has changed
                inverseMat <<- NULL
        }
		
		## Subfunction to retrieve input matrix
        get <- function() matx
		
		## Subfunction to set inversed matrix. The argument should be inverse
		## of matrix matx provided while calling makeCacheMatrix
        setInverseMatrix <- function(inversedMatrix) {
			inverseMat<<- inversedMatrix
        }
		
		## Subfunction to get the inverse matrix
		getInverseMatrix <- function() inverseMat
		
		## Returns list of subfunctions
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}


## Returns inverse of a matrix, if the inverse is cached then provides cached data
## else computes inverse and stores the inverse for future use.

cacheSolve <- function(x, ...) {
		## Retrieving current value stored in x
        inv <- x$getInverseMatrix()
		
		## Check if value is not null and return cached data
        if(!is.null(inv)) {
                message("getting cached data of inverse matrix")
				## Return a matrix that is the inverse of 'x'
                return(inv)
        }
		
		## this is reached if there is no cached value so we would calcuate
		## inverse matrix and store it in x
		## Get Input Matrix
        data <- x$get()
        
		## Calculate inverse matrix and set the value in x to cache
		inv <- solve(data)
		x$setInverseMatrix(inv)
        
		## Return a matrix that is the inverse of 'x'
		inv
        
}
