## cachematrix.R
#
# This module provides functions which allow a "cached matrix" to be
# created which memoizes (caches the computation of) the inverse of
# the matrix.  Thus, repeated calls to "cacheSolve" return the same
# computed inverse unless and until the matrix stored therein is
# changed, upon which time, the inverse will be recomputed.
#
# The name "cacheSolve" (presumably) comes from "solve", which is the
# usual means to directly compute the inverse of a matrix in R.
#
# This implementation closely resembles the example code in the
# assignment, obviously.

## makeCacheMatrix(m)
#
# m: matrix
#
# returns: object(list) with methods to manipulate the object's state
#
# makeCacheMatrix is a constructor/factory which creates an object
# with various methods that allow one to cache a matrix in in and to
# retrieve that matrix's inverse.  I'm going to use the same
# conventions for method names as the vector example.

makeCacheMatrix <- function(cachedMatrix = matrix()) {
    cachedInversion <- NULL

    set <- function(newMatrix) {
        cachedMatrix <<- newMatrix
        cachedInversion <<- NULL
    }

    get <- function() cachedMatrix

    setInversion <- function(inversion) cachedInversion <<- inversion
    getInversion <- function() cachedInversion

    list(set = set, get = get,
         setInversion = setInversion, getInversion = getInversion)
}


## cacheSolve(m)
#
# m: matrix
#
# returns: the inverse of the matrix
#
# cacheSolve produces the equivalent answer as "solve", but the
# computation of the inverse will happen only once for a specific
# matrix.  (I.e., if the same matrix object, as returned by
# "makeCacheMatrix", is passed into this function repeatedly, the
# actual invocation of "solve" will only happen once.)

cacheSolve <- function(m, ...) {

    i <- m$getInversion()
    if (!is.null(i)) {
        # It is unclear if the following message (a la the example
        # code) is intended to be included in the submitted homework
        # assignment.  Thus, I will leave it visible.
        message("getting cached data")
        return(i)
    }

    rawMatrix <- m$get()
    i <- solve(rawMatrix, ...)
    m$setInversion(i)
    i
}
