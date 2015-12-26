######################################################################
# The code below is part of the assessment for the course R programming
######################################################################

# There are two functions that are used to create a special object
# that stores a numeric matrix and caches its inverse.

# The first function, makeMatrix creates a special "matrix", which is 
# really a list containing a function to:

# set the value of the matrix
# get the value of the matrix
# set the matrix inverse and store in cache
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        cached_matrix <- NULL

        # setting the value of the matrix
        set <- function(y) {
                x <<- y
                cached_matrix <<- NULL
        }

        # getting the value of the matrix
        get <- function() x
        
        # inverting the matrix and storing it in cache
        setMatrix <- function(inverse) cached_matrix <<- inverse
        
        # getting the inverted matrix from cache
        getInverse <- function() cached_matrix

        # return the created functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}





# The following function calculates the inverse matrix previously stored with the above function. 
# 1st checks to see if the inverted matrix has already been calculated.
# If so, gets the inverse matrix from the cache and skips the computation. 
# Otherwise, calculates the inverse matrix in the cache via the setMatrix function.

cacheSolve <- function(x, ...) {
        # get the inverse of the matrix stored in cache
		cached_matrix <- x$getInverse()

        # checks if it has already been calculated
        if (!is.null(cached_matrix)) {
                # if so, returns the cached matrix
                return(cached_matrix)
        }

        # otherwise, creates the matrix
        cached_matrix <- solve(x$get())
		
		# calculates inverse
        x$setMatrix(cached_matrix)

        # return cached inverse matrix
        return (cached_matrix)
}
