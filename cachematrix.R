#------------------------------------
# Purpose:       To speed up the process of matrix inversion by calculating 
#                an invertable matrix and then caching a copy of it for  
#                further use. Program will get/set a raw data matrix and 
#                get/set the matrix's inverse.
#
#  Usage:        Assuming the working directory is set properly.
#                Example at the command line:
#                > source("matrixcache.R")
#                > amatrix <- matrix(c(1,45,-23,55,-12,9,64,21,18), nrow=3,ncol=3)           
#                > mat <- makeCacheMatrix(amatrix)
#                
#                To set the inverse
#                > cacheSolve(mat)

#                To call the inverse
#                > mat$getinverse()

#                To get original matrix
#                > mat$get()

#                To set a new matrix
#                > mat$set(matrix)  #where matrix is a new inverted matrix
#
# Assumptions:   The matrix is invertable.
#------------------------------------

#------------------------------------
# Purpose:       Define a list of functions to:
#                1) get/set a matrix into/from memory.
#                2) get/set the inverse of the matrix into memory.
#                Lexical scoping is used provide a mini "environment"
#                to store the matrix and its inverse into memory.
# 
#                
# Functions:     1) set(x) - sets/caches a raw matrix where 'x' is 
#                   an invertable matrix.
#                2) get() - returns the raw matrix.
#                3) setinverse(new_inverse) - sets/caches the inverted matrix
#                   into memory where "new_inverse" is the newly created 
#                   inverted matrix.
#                4) getinverse() - returns the cached inverse.
#
# Returns:       A list with functions to get/set an invertable matrix.
#                and to get/set the matrix's inverse.
#------------------------------------
makeCacheMatrix <- function(matrix = matrix()) {
        
        #new matrix provided as function parameter
        #so set inverse to null.
        inverse_matrix <- NULL
    
        #function to "store" raw matrix.
        #set inverse equal to null
        set <- function(y) {
                m <<- y
                inverse_matrix <<- NULL
        }
 
        #return the raw matrix
        get <- function() m
        
        #function to "store" inverse matrix
        setinverse <- function(new_inverse) inverse_matrix <<- new_inverse
        
        #returns the inverted matrix from memory
        getinverse <- function() inverse_matrix
        
        #list whose elements are the functions
        #defined above
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}
#------------------------------------
# Purpose:      To retrieve the inverse of a matrix from memory - ie: cached
#               If the inversion does not yet exist, the function
#               will calculate and store it. This function depends on
#               makeCacheMatrix in order to execute.
#
# Arguments:    m - a list created from the function makeCacheMatrix
#                 
# Returns:      An inverted matrix.
#------------------------------------
cacheSolve <- function(m, ...) {
    
    #call inverted matrix
    #if (!identical(m, ))
    inverse <- m$getinverse()
    
    #if inverted matrix exists
    #return it to caller
    if(!is.null(inverse)) {
        message("getting cached matrix")
        return(inverse)
    }
    
    #otherwise, get "raw" matrix so it can be inverted 
    # and cached for future use.    
    data <- m$get()
    
    #calculate inverse     
    inverse <- solve(data, ...)
    
    #cache the inverse and return it to calling function
    m$setinverse(inverse)
    inverse
}
#end of program