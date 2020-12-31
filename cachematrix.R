## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {     ##Create makeCacheMatrix as the parent function of matrix x
        inv <- NULL                             ##to put the initial inverse matrix as Null
        set <- function(y) {                    ##function set() with argument y
                x <<- y                         ##y's environment is locked to follow x's environment
                inv <<- NULL                    ##to clear any prior content of matrix inv in function cacheSolve()
        }
        get <- function() x                     ##create function get() to get matrix x
        setInverse <- function(inverse) inv <<- inverse      ##create function setInverse(inverse) and to lock the
                                                             ##inverse's environment to follow inv's environment
        getInverse <- function() inv            ##create function getInverse() to get matrix inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)           ##to put the result of function set, get, setInverse and getInverse
                                                ##in a list, so can be called under name x$set, x$get, etc.

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                ##Create cacheSolve with argument x and ellipsis
        inv <- x$getInverse()                   ##to retrieve inverse matrix inv from makeCacheMatrix
        if(!is.null(inv)) {                     ##to check whether the inverse matrix inv is Null
                message("getting cached data")  ##to display the message if inv is not Null
                return(inv)                     ##to return inv if inv is not Null
        }
        mat <- x$get()                          ##to get input matrix
        inv <- solve(mat, ...)                  ##to calculate inv
        x$setInverse(inv)                       ##to use the setInverse() function on the input object to set 
                                                ##the inverse in the input object,
        inv                                     ##Return a matrix that is the inverse of 'x'
}


my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()