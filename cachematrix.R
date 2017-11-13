## R Programming Assignment 2 - chachematrix.R
## Author : samuel Quiroga
## Date: Nov 13, 2017

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {# first intialize two objects x and s 
        s = NULL
        set<-function(y) {
                x<<- y #use the <<- form of assignment vector to assign y to x
                s<<- NULL
        }
        get<-function() x # retreives x from parent environment
        setsolve <- function(solve) s<<- solve #uses <<- to assign s in parent environment
        getsolve <- function() s #define getter for s, jsut like we did for x
        
        # the last part assigns functions as elements within a list, 
        # also each element is named so we can used $ extact operator 
        # to acccess functions by name
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve() #attempts to retreive mean of object passed in the argument
        if(!is.null(s)) { #then checks to see if result is NOT null, if so, then return s
                message("getting cached data")
                return(s)
        }
        data <- x$get() # if its NULL, then cacheSolve gets the matrix from input object
        s <- solve(data, ...) # solve the inverse of matrix
        x$setsolve(s) # uses setsolve function to set solve on input object
        s
        ## Return a matrix that is the inverse of 'x'
}
