## Similar to the examples makeVector() and cachemean, here are two function
## that implemenent a cached calculation of the inverse of a matrix.
## makeCacheMatrix() creates the matrix
## cacheSolve() calculates and stores the inverse matrix in cache,  
## or retrevies from cache, if alredy calculated. Must use matrix created by makeCacheMatrix()
##
## example of use:
##
## >m <- matrix(c(1,2,3,4),2,2)
## cm <- makeCacheMatrix(m)
## >cm$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## >cacheSolve(cm)
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
## >cacheSolve(cm)
## getting cached data
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

## makeCachedMatrix() create a data structure to store both a matrix and its inverse and provide a list
## of functions to interface with both the matrix and its inverse
## input: a matrix x (default empty matrix)
## processing:  sets an inital value for the inverse of the matrix inv, to NULL.
## output: a list of "getter" and "setter" functions for both the matrix and its inverse.
##       set(): sets and resets the matrix x to the function call input y, and initializes 
##              the inv to NULL. 
##       get(): return the matrix X
##       setinv(): sets the value of the matrix inv to the function call input inverse
##       getinv(): get the value of the inv
## Note, the due to R lexical scoping rules, the list functions have access to the environment 
## in which they were defined, and specifically to the variables inv and x. 
## 


makeCacheMatrix <- function(x = matrix()) {
        ## creats and initializes a caching structure for the matrix x, and its inverse inv
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
        
}


## This function returns the inverse of the input matrix, obtained either by calculation, or by retreiveal 
## from cache.
## input: a structure created by makeCacheMatrix
## processing: if a cached value of the inverse (matrix inv) exists (i.e. not NULL), return it. 
## Otherwise calculate the inverse using the built it solve method. The parameterse to the solve methods 
## can be input the cacheSolve function, and are passed to the solve function
## output: inv, a matrix with the same dimension as the input matrix x (if x has no inverse, the solve
## functinon will error out with suitable message)
## Note that the makeCacheMatrix function initializes inv to NULL, so a first call to the cacheSolve function
## will attempt to calcualte inv using the solve method, and will store in the cache. Subsequent calls 
## to cacheSolve will then retreive the inv from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <-x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
