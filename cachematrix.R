## These two functions used to create a special object that stores a matrix 
## and cache's its inverse

## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  ##stores the cached value
        cache <- NULL           ## initializes cache to NULL
        set <- function(y) {    ## creates the matrix in the working enviroment 
                x <<- y
                cache <<- NULL
        }
        get <- function() x     ## get the value of the matrix
        setinverse <- function(inverse) cache <<- inverse ## invert the matrix and store in the cache variable
        getinverse <- function() cache ## gets the inverted matrix from the cache variable
        list(set = set, get = get,   ## returns the created functions to the working environment
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {  
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getinverse() ##attempts to get the inverse of the matrix stored in the cache
        ## returns the inverted matrix from cache if it exists
        ## else creates the matrix in the working environment
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache) ## displays matrix in the console
        }
        ## the rest of this function gets ignored if the inverted matrix was in the cache
        matrix <- x$get() ## creates matrix since it does not exist
        cache <- solve(matrix, ...) ## re-calculates inverse of matrix if it wasn't in cache
        x$setinverse(cache) ## puts inverted matrix in cache
        cache
}
