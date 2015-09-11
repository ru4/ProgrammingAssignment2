## This function calculate the inverse of a matrix and cache it for further use.

## create the spacial Matrix with setter and getter for Matrix data and setter, getter for inverse of Matrix data
## setinv() caches the Matrix in "m" variable in its parent env. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)

}


## return the inverse of the spacial matrix from cache if its already there or calculates and cache it via setinv() then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
