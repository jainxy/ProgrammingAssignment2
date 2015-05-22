## Overall description: These functions implement caching of computationally 
## expensive operation of matrix inversion. The computational complexity of 
## general NxN matrix is O(N^3).
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## The 'makeCacheMatrix' function converts the matrix object into 
## "special vector" which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y) {
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


## The 'cacheSolve' function takes "special vector" created by 'makeCacheMatrix'
## function as argument. It checks if its inverse have been calculated. If it is,
## then inverse from cache is simply returned, otherwise the inverse of 
## corresponding matrix of the argument is calulated and set the value of inverse
## in the cache. It also returns the same.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("retrieving cached data ...")
                return(inv)
        }        
        data <- x$get()
        inv <-solve(data)
        x$setinv(inv)
        inv
}
