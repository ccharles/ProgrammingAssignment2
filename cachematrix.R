#
# This package defines "cache matrices", matrix-like objects that wrap around
# a native R matrix and can cache their computed inverse.
#
# The cacheSolve() function defined below should be used in place of solve()
# when working with cache matrices.
#
# ---------------------------------------------------------------------------
#
# Functions in this package are documented with comments *inside the
# function definition*, as suggested in Google's R style guide:
#
#     https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
#


makeCacheMatrix <- function(x = matrix()) {
    # Create a cache matrix, wrapping around the passed R matrix.
    #
    # Args:
    #   x: The native R matrix that should be wrapped.
    #
    # Returns:
    #   A list of functions that can be used to manipulate the cache matrix:
    #   get(), set(), getInverse(), and setInverse().

    cachedInverse <- NULL

    set <- function(y) {
        # Set the R matrix wrapped by this object.
        #
        # Args:
        #   y: The new R matrix that should be wrapped.
        #
        # Returns:
        #   NULL
        x <<- y
        cachedInverse <<- NULL
    }

    get <- function() {
        # Get the R matrix wrapped by this object.
        #
        # Args:
        #   None
        #
        # Returns:
        #   The native R matrix wrapped by this object.
        x
    }

    setInverse <- function(inverse) {
        # Set this object's cached inverse.
        #
        # Args:
        #   The inverse to cache.
        #
        # Returns:
        #   The inverse matrix.
        cachedInverse <<- inverse
    }

    getInverse <- function() {
        # Get this object's cached inverse.
        #
        # Args:
        #   None
        #
        # Returns:
        #   The cached inverse matrix, or NULL.
        cachedInverse
    }

    # Return references to the four functions defined above so they can be
    # called externally.
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    # Get the inverse of a cache matrix, using the cached value if available,
    # and computing and caching it if not.
    #
    # Args:
    #   x: The cache matrix whose inverse should be computed.
    #   ...: Extra arguments to be passed directly into solve().
    #
    # Returns:
    #   The inverse matrix of x. Raises an error if the inverse does not
    #   exist.

    # Get the cached inverse...
    inverse <- x$getInverse()

    # ...and return it if it's not null.
    if (!is.null(inverse)) {
        message("Using cached data")
        return(inverse)
    }

    # If we get this far there was no cached inverse, so let's compute it
    # using the regular solve() function...
    mtrx <- x$get()
    inverse <- solve(mtrx, ...)

    # ...and cache it for next time.
    x$setInverse(inverse)

    inverse
}
