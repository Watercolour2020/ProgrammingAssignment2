## these functions demonstrate take advantage of the scoping rules of the R language and 
## how they can be manipulated to preserve state inside of an R object.   
## H Stiff 27 Feb 2016


## function makeCacheMatrix - creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   #returns a list of four functions related to the above
        
     #  m <- data.frame() 
        setmatrix <- function(y) {    #setmatrix is a function 
                x <<- y               #that accepts a new matrix from user
                m <<- data.frame()    #which requires that the inverse matrix 'cache' be cleared
        }
        getmatrix  <- function() x    #getmatrix is a function that returns the stored matrix
        setinverse <- function(solve) m <<- solve(x) #function that inverts the passed matrix and stores it in the matrix cache
        getinverse <- function() m    #function that returns the stored matrix inversion from the cache
        
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

## function makeCacheMatrix - computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        m <- x$getinverse()    #retrieve inverted matrix if it exists in 'cache' m
        
        if(nrow(m)>0) {        #if matrix has data, then...
                message("Getting cached matrix inversion...")
                return(m)      #display matrix data from 'cache'
        }
        
        data <- x$getmatrix()  #otherwise retrieve original matrix
        m <- solve(data, ...)  #and invert it
        x$setinverse(m)        #and dump inverted matrix into cache m
        
        m                      #dont forget to return inverted matrix
}
