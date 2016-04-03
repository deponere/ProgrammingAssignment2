## Programming Assignment 2 for R Programming on Coursera 
## Assignment: Caching the Inverse of a Matrix
##
## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: generates a cacheable matrix and defined the 
## operators to manipulate the stored matrix

####################### Code #####################################
## Write a short comment describing this function
##
## makeCacheMatrix, takes a matrix and add the function get,set,getmat and 
## setmat to get,set the matrix, setmat and getmat to extract the already
## calculated invers of the matrix
## matPuffer is internal variable to store the chache of the generated result
## x is the argument
## y is the variable to fullfill the set function

makeCacheMatrix <- function(x = matrix()) {
        matPuffer <- NULL
        set <- function(y) {
                x <<- y
                matPuffer <<- NULL
        }
        get <- function() x
        setmat <- function(s) matPuffer <<- s
        getmat <- function() matPuffer
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
}

## Write a short comment describing this function
##
## cacheSolve take a before generated matrix (made with makeCacheMatrix)
## checks if there was already a inversion of the matrix calculated
## and returns the chache of calculates the matrix for the first time..
## m1 internal variable to deal with the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m1 <- x$getmat()
        if (!is.null(m1)) {
                message("getting cached inverse - matrix")
                return(m1)
        }
        data <- x$get()
        m1 <- solve(data, ...)
        x$setmat(m1)
        m
}
########################## End Code ################################
