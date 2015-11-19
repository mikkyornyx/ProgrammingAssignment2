## makeCacheMatrix function takes matrix as an argument and CacheSolve function calculates
## the inverse of that matrix. If inverse matrix has already been calculated and matrix value hasn't
## changed then it will take it from cache with no calculation
 
makeCacheMatrix <- function(x = matrix()) {
	## store the cached value, init to NULL
	cached_value <- NULL
	## create matrix
	set <- function(y) {
	  x <<- y
		cached_value <<- NULL
	}
	## get matrix value
	get <- function() x
	## invert matrix value and store in cache
	set_matrix <- function(inverse) cached_value <<- inverse
	## get inverted matrix from cache
	get_inv_matrix <- function() cached_value
	## load functions to R env
	list(set = set, get = get, set_matrix = set_matrix, get_inv_matrix = get_inv_matrix)
}


## cacheSolve calculates makeCacheMatrix inversed value
## if it is possible to find that value from cache, it is
## retrieved, if not, it is calculated and cached

cacheSolve <- function(x, ...) {
        ## try to find cached value
	cached_value <- x$get_inv_matrix()
	## return inverted value if it is in cache
	if (!is.null(cached_value)) {
	  message("cached value: ")
		return(cached_value) 
	}
	## if there is no cached value, create new matrix
	new_matrix <- x$get()
	## invert matrix and set it in cache
	cached_value <- solve(new_matrix)
	x$set_matrix(cached_value)
	## return cached value
	return(cached_value)
}
