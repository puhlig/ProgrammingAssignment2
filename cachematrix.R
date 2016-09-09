## R Programming : Week 3 Peer Graded Assignment
## Using Lexical Scoping to create a computation cache
##
## Honor code statement:
## *** this has near-duplicates of the example code provided in problem description ***
##
## makeCacheMatrix -    wraps a matrix in a list of functions to set, get, setInv, getInv,
##                      which set or get the matrix, and set or get the inverse matrix. The
##                      cache is maintained with free variables at the environment level
##                      of makeCacheMatrix.
##

makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL
      set <- function(y) {
            x <<- y           # whenever the cached matrix is setin the free var x ...
            invMat <<- NULL   # the free var invMat must be reset to NULL 
      }
      get <- function() x                             # return the original matrix stored in x
      setInv <- function(invIn) invMat <<- invIn      # store given inverse in free var invMat
      getInv <- function() invMat                     # return the known inverse matrix: invMat
      list(set = set, get = get,
                 setInv = setInv, 
                 getInv = getInv)   # return the CacheMatrix list, that wraps the stored matrix
}


## cacheSolve -   calculates the Inverse of the "wrapped"
##                matrix produced by makeCacheMatrix.  Check first to see if 
##                inverse has been cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      invMat <- x$getInv()          # get inverse contents from cache

      if(is.null(invMat)) {        # IF cache returned Null, then invMat is not in cache
            message("using SOLVE to find inverse matrix")
            originalMat <- x$get()        # recover the original matrix     
            invMat <- solve(originalMat)  # find the inverse matrix using solve
            x$setInv(invMat)              # cache the now-known inverse matrix for future use
      } else {                      # ELSE the cache has the inverse
            message("using cached inverse matrix")
      }
      return(invMat)          # return invMat and stop further processing
}
