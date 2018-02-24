## Put comments here that give an overall description 
## of what your functions do
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly 

## Coursera - R Programming Course
## Programming Assignment 2: Lexical Scoping
## A pair of functions that cache the inverse of a matrix.
## Author: nada-sh

## For this assignment, assume that the matrix supplied is always invertible.

## Write a short comment describing this function
# makeCacheMatrix: 
# ----------------
# This function creates a special "matrix" object that can
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    # Define input to function as a matrix
    
    inv_matrix <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv_matrix <<- NULL
        # <<- operator which can be used to assign a value 
        # to an object in an environment that is different 
        # from the current environment.
    }
    
    get <- function() x
    
    set_InverseMatrix <- function(inverse) inv_matrix <<- inverse
    get_InverseMatrix <- function() inv_matrix
    
    list( set = set, 
          get = get,
          set_InverseMatrix = set_InverseMatrix,
          get_InverseMatrix = get_InverseMatrix)
}


## Write a short comment describing this function
# cacheSolve:
# -----------
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    # Define input to function as a matrix with ellipsis 
    # (any number of named or unnamed arguments)
    
    inv_matrix <- x$get_InverseMatrix()
    
    # Check if the data is already cashed
    if(!is.null(inv_matrix))
    {
        # If the data is already cashed
        # Show message and get cashed data
        message("Getting cashed data")
        return(inv_matrix)
    }
    
    
    matrix_data <- x$get()
    # The inverse of a square matrix can be done with the solve function.
    inv_matrix <- solve(matrix_data, ...)
    # Set inv_matrix with the value calculated using Solve function
    # to get the inverse of a square matrix
    x$set_InverseMatrix(inv_matrix)
    inv_matrix
}

# #### My Tests ####
# ## 1. ## Using 2x2 matrix, generated from random integers between 1 & 10
# m1 <- makeCacheMatrix(matrix(sample(1:10, 4, replace = FALSE), 2, 2))
# ## 1.a) Get the matrix formed
# m1$get()
# # > m1$get()
# # [,1] [,2]
# # [1,]    8    6
# # [2,]    3    4
# ## 1.b) Get the inverse to the matrix
# m1$get_InverseMatrix()
# # > m1$get_InverseMatrix()
# # NULL
# ## 1.c) Calculate the inverse to the matrix for the first time
# cacheSolve(m1)
# # > cacheSolve(m1)
# # [,1]       [,2]
# # [1,]  0.2857143 -0.4285714
# # [2,] -0.2142857  0.5714286
# ## 1.d) Re-run the solve function to get cashed data
# ## instead of calculating from the beginning
# cacheSolve(m1)
# # > cacheSolve(m1)
# # Getting cashed data
# # [,1]       [,2]
# # [1,]  0.2857143 -0.4285714
# # [2,] -0.2142857  0.5714286
# ## 1.e) Checking Get Inverse Matrix function after data cashed
# m1$get_InverseMatrix()
# # > m1$get_InverseMatrix()
# # [,1]       [,2]
# # [1,]  0.2857143 -0.4285714
# # [2,] -0.2142857  0.5714286
# ## 1.f) Set the matrix m1 with new values (11, 12, 13, 14)
# m1$set(matrix(c(11,12,13,14),2,2))
# ## 1.g) Get the edited matrix after set
# m1$get()
# # > m1$get()
# # [,1] [,2]
# # [1,]   11   13
# # [2,]   12   14
# ## Repeat previous steps for the new matrix to
# ## make sure that correct Inverse is cashed
# ## 1.h) Try to get Inverse cashed matrix
# m1$get_InverseMatrix()
# # > m1$get_InverseMatrix()
# # NULL
# ## 1.i) Calculate the inverse of the new matrix
# casheSolve(m1)
# # > cacheSolve(m1)
# # [,1] [,2]
# # [1,]   -7  6.5
# # [2,]    6 -5.5
# ## 1.j) Get the cashed Inverse of the new matrix by recalling cacheSolve function
# casheSolve(m1)
# # > cacheSolve(m1)
# # Getting cashed data
# # [,1] [,2]
# # [1,]   -7  6.5
# # [2,]    6 -5.5
# ## 1.k) Try to get Inverse cashed matrix with Get Inverse Matrix
# m1$get_InverseMatrix()
# # > m1$get_InverseMatrix()
# # [,1] [,2]
# # [1,]   -7  6.5
# # [2,]    6 -5.5
# # ---- #
# ## 2. ## Using 2x2 matrix with 1,2,3 & 4 as values
# m2 <- makeCacheMatrix(matrix(1:4, 2, 2))
# ## 2.a) Get the matrix formed
# m2$get()
# # > m2$get()
# # [,1] [,2]
# # [1,]    1    3
# # [2,]    2    4
# ## 2.b) Get the inverse to the matrix
# m2$get_InverseMatrix()
# # > m2$get_InverseMatrix()
# # NULL
# ## 2.c) Calculate the inverse to the matrix for the first time
# cacheSolve(m2)
# # > cacheSolve(m2)
# # [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5
# ## 2.d) Re-run the solve function to get cashed data
# ## instead of calculating from the beginning
# cacheSolve(m2)
# # Getting cashed data
# # [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5
# ## 2.e) Checking Get Inverse Matrix function after data cashed
# m2$get_InverseMatrix()
# # > m2$get_InverseMatrix()
# # [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5
# ## 2.f) Set the matrix m1 with new values (100, 200, 300, 400)
# m2$set(matrix(c(100,200,300,400),2,2))
# ## 2.g) Get the edited matrix after set
# m2$get()
# # > m2$get()
# # [,1] [,2]
# # [1,]  100  300
# # [2,]  200  400