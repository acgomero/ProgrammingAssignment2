## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## This function creates a special "matrix" object
## that can cache its inverse.
## The function sets the value of the matrix
## It then gets the value of the matrix
## Followed by setting the value of the inverse
## Lastly, it gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        show_matrix <- NULL
        set <- function(store_matrix) {
                x <<- store_matrix 
                show_matrix <<- NULL 
        }
        get <- function() x
        set_inverse <- function(solve) show_matrix <<- solve
        get_inverse <- function() show_matrix
        list(set = set, get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}


## Write a short comment describing this function
## This next function calculates the inverse of the matrix created with the above.
## First it checks to see if the inverse has already been calculated.
## If the inverse has been calculated it gets it from the catch and skips the computation.
## If no, it calculates the inverse and sets the value of the inverse in the cache with the set_inverse function.

cacheSolve <- function(x, ...) {
        show_matrix <- x$get_inverse()
        if(!is.null(show_matrix)) {
                message("getting cached matrix")
                return(show_matrix)
        }
        data <- x$get()
        show_matrix <- solve(data, ...)
        x$set_inverse(show_matrix)
        show_matrix
        ## Return a matrix that is the inverse of 'x'
}
