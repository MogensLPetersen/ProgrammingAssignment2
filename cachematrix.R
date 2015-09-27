## Programming Assignment 2

## The first function, makeCacheMatrix, creates a special 
## "matrix" and sets/gets matrix, sets/gets matrix inverse:

makeCacheMatrix <- function(x = matrix()) { 
    
    matinverse <- NULL
    set <- function(y) {
        x <<- y
        matinverse <<- NULL
    }
    get <- function() x
    ## make matrix in lexical scoping.
    setinverse <- function(inverse) matinverse <<- inverse
    getinverse <- function() matinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The second function calculates the inverse of the special 
## "matrix" created with the previous function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the 
##data and sets the value of the inverse in the cache
## via the setinverse function. Using "solve" to find matrix inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    
    matinverse <- x$getinverse()
    if(!is.null(matinverse)) {           ## if not inverse in cache
        message("getting cached data")
        return(matinverse)               
    }
    data <- x$get()
    matinverse <- solve(data, ...)
    x$setinverse(matinverse)
    matinverse
} 

## Instructor made
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## Instructor made
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
