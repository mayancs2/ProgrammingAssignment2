###############################################################################
# Matrix inversion with caching
# Caching based on makeVector/cachemean by rdpeng
# Copyright (C) 2015  C.S.
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
###############################################################################

###############################################################################
# Cache a valid non-empty square matrix and 
# return a list with named members (functions) to set/get matrix and 
# setinverse/getinverse of matrix
# 
# Name - makeCacheMatrix
# Arguments - non-null, non-empty, square matrix
# Returns list of methods to change it (set), retrieve it (get), 
#     store inverse (setinverse), retrieve inverse (getinverse)
#
# Usage Example:
#       See Usage example for cacheSolve, also testCacheMatrix
#
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
        # Check validity of matrix
        isMatrixValid(x)
        
        # set inverse matrix to null/uncalculated and so uncached
        inverse_x <- NULL
        
        # Method to Cache matrix        
        set <- function(y) {
                # Check validity of matrix
                isMatrixValid(y)
                
                # set matrix data
                x <<- y
                
                # reset inverse to null/not calculated and so uncached
                inverse_x <<- NULL
        }
        
        # Method to get cached matrix
        get <- function() x
        
        # Method to Cache inverse of matrix        
        setinverse <- function(inverse) inverse_x <<- inverse
        
        # Method to get cached inverse of matrix        
        getinverse <- function() inverse_x
        
        # create and return list of methods with indicated member names 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

###############################################################################
# Calculate the inverse of the stored matrix, cache it and return it. 
# Directly return inverse if already present in cache.
# Assumes matrix has inverse
#
# Name - cacheSolve
# Arguments - list returned by makeCacheMatrix
# Returns - inverse of cached matrix
#
# Usage Example:
#      aMatrix <- matrix(c(3,1,0,2), nrow=2, ncol=2)
#      matrixCache <- makeCacheMatrix(aMatrix)
#      aMatrixInverse <- cacheSolve(matrixCache)
#
#      aMatrix
#      [,1] [,2]
#      [1,]    3    0
#      [2,]    1    2
#
#      aMatrixInverse
#      [,1] [,2]
#      [1,]  0.3333  0.0
#      [2,] -0.1667  0.5
#
#      aMatrix %*% aMatrixInverse (or aMatrixInverse %*% aMatrix)
#      [,1] [,2]
#      [1,]    1    0
#      [2,]    0    1
#
#      Also see testCachedMatrixInversion
#
###############################################################################
cacheSolve <- function(x, ...) {
        
        # get and check if inverse is already calculated and cached
        inverse_x <- x$getinverse()
        if(!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        }
        
        # get data (matrix)
        data <- x$get()
        
        # make identity matrix I of the same dimension as the matrix
        # rows already = cols and > 0, I is the diagonal rows x rows matrix
        identity <- diag(nrow(data))
        
        # matrix %*% inverse(matrix) = I, solve returns inverse
        inverse_x <- solve(data, identity, ...)
        x$setinverse(inverse_x)
        inverse_x
}

###############################################################################
# Check if input is non-null non-empty valid square matrix
# Stops program if matrix is not valid
# 
# Arguments - matrix
#
###############################################################################
isMatrixValid <- function(x) {
        # Check if valid non-null non-empty square matrix is specified        
        if (is.null(x) || !is.matrix(x) || nrow(x) == 0 || nrow(x) != ncol(x)) {
                stop("Invalid or empty or non-square matrix specified")
        }
}

###############################################################################
# Check if 2 matrices are equal
# invalid arguments fail this test
# 
# Arguments - first matrix, second matrix
# Returns - TRUE on success, FALSE on failure
#
###############################################################################
areMatricesEqual <- function(x, y) {
        # Check if valid non-null non-empty square matrix is specified        
        if (is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)) {
                TRUE
        } else {
                FALSE
        }
}


###############################################################################
# Test makeCacheMatrix and cacheSolve
# 
# Name - testCacheMatrix
# Arguments - None
# Returns - TRUE if successful, FALSE if failed
#
###############################################################################
testCachedMatrixInversion <- function() {
        
        ######################
        # Test main operations
        ######################
        
        aMatrix <- matrix(c(3,1,0,2), nrow=2, ncol=2)
        matrixCache <- makeCacheMatrix(aMatrix)
        
        # First inverse calculation, must NOT display 
        # "getting cached data message"
        message("Must not display \"getting cached data\" next")
        aMatrixInverse <- cacheSolve(matrixCache)
        
        # check matrix %*% inverse_matrix = Identity matrix
        if (areMatricesEqual(aMatrix %*% aMatrixInverse,  diag(nrow(aMatrix)))) {
                message("testCacheMatrix passed")
        } else {
                message("testCacheMatrix failed")
                return (FALSE)
        }
        
        # Repeat invocation, must display "getting cached data" message
        message("Must display \"getting cached data\" next")
        aMatrixInverse <- cacheSolve(matrixCache) 
        
        
        
        
        
        ######################
        # Test with a refresh 
        # of the matrix
        ######################
        
        # Make new matrix
        aMatrix <- matrix(c(3,1,1,0,1,1,0,0,2), nrow=3, ncol=3)
        
        # Verify set/get        
        matrixCache$set(aMatrix)
        
        if (areMatricesEqual(aMatrix, matrixCache$get())) {                
                message("testCacheMatrix passed set/get")
        } else {
                message("testCacheMatrix failed set/get")
                return (FALSE)
        }        
        
        # First inverse calculation after new matrix set, must NOT display 
        # "getting cached data message"
        message("Must not display \"getting cached data\" next")
        aMatrixInverse <- cacheSolve(matrixCache)  
        
        # check matrix %*% inverse_matrix = Identity matrix        
        if (areMatricesEqual(aMatrix %*% aMatrixInverse,  diag(nrow(aMatrix)))) {
                message("testCacheMatrix passed (matrix refreshed)")
        } else {
                message("testCacheMatrix failed (matrix refreshed)")
                return (FALSE)
        }
        
        TRUE
}
