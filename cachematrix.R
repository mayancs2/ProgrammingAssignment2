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
# Store a valid non-empty square matrix and 
# Arguments - non-null, non-empty, square matrix
# Returns list of methods to change it (set), retrieve it (get), 
#     store inverse (setinverse), retrieve inverse (getinverse)
#
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
        # Check if valid square matrix is specified        
        if (is.null(x) || !is.matrix(x) || nrow(x) == 0 || nrow(x) != ncol(x)) {      
                stop("Invalid or empty or non-square matrix specified")
        }
        m <- NULL
        
        # Method to Cache matrix        
        set <- function(y) {
                # Check if valid square matrix is specified
                if (is.null(y) || !is.matrix(y) || nrow(y) != ncol(y)) { 
                        stop("Invalid or non-square matrix specified")
                }                
                x <<- y
                m <<- NULL
        }
        
        # Method to get cached matrix
        get <- function() x
        
        # Method to Cache inverse of matrix        
        setinverse <- function(inverse) m <<- inverse
        
        # Method to get cached inverse of matrix        
        getinverse <- function() m
        
        # create list of methods with indicated member names 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

###############################################################################
# Calculate the inverse of the stored matrix, cache it and return it. 
# Directly return if already present in cache.
# Assumes matrix has inverse
# Arguments - list returned by makeCacheMatrix
# Returns inverse of cached matrix
# 
###############################################################################
cacheSolve <- function(x, ...) {
        
        # get and check if inverse is already calculated and cached
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # get data
        data <- x$get()

        # make identity matrix I
        # rows already = cols and > 0
        rows <- nrow(data)
        # identity <- matrix(rep(0, rows*rows), nrow=rows, ncol=rows)
        # lapply(seq_len(rows), function(i) identity[i,i] <<- 1)
        identity <- diag(rows)
        
        # inverse(data) %*% data = I, solve returns inverse
        m <- solve(data, identity, ...)
        x$setinverse(m)
        m
}

