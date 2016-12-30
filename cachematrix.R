##  This two function allow to cache the inverse of a matrix.
##  If the inverse of a matrix is already stored additional computation are 
##  avoided.

## makeCacheMatrix is a function that take as an input a matrix and return a
## list of function. This list will be used as an input for the function 
## cacheSolve.
## The four functions in the list are:
##      set.mat: it allows to set a new matrix that will be used to compute the 
##              inverse in the second function. Once the makeCacheMatrix is called
##              set.mat can be used to set a new matrix without calling makeCacheMatrix
##              again
##      get.mat: it allows to return the matrix stored, for which we want to
##              compute the inverse
##      set.inv_mat: a function that set the inverse of the matrix x in the 
##              variable inv
##      get.inv_mat: a function that retrieve the inverse of the matrix x from 
##              the variable inv
##
## The logic is to create a object with makeCacheMatrix which environment 
## contains the two variables x and inv (respectively devoted for the matrix and
## its inverse). By using the function in the list created by makeCacheMatrix
## we are able to act (in this case get and set) on the variables in the 
## makeCacheMatrix environment, i.e. x and inv.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL    ## initialize a variable inv to store the inverse
        
        set.mat <- function(y) {  
                x <<- y           ## y is a new matrix for which we want to compute the inverse
                inv <<- NULL      ## if a new matrix is set the inverse must be set again to null
        }
        get.mat <- function() x
        set.inv_mat <- function(inv_mat) inv <<- inv_mat
        get.inv_mat <- function() inv
        
        list(set.mat = set.mat, get.mat = get.mat,
             set.inv_mat = set.inv_mat,
             get.inv_mat = get.inv_mat)
        
}


## cacheSolve is a function that takes as an input the list created with 
## makeCacheMatrix and compute its inverse if it is not stored already in the
## variable inv in the environment of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        
        # This is a check to avoid to use cacheSolve if befor makeCacheMatrix was not called
        if (class(names(x))=="NULL") {
                return ("First you have to use makeCacheMatrix function to use cacheSolve!")
        } else if (names(x)[[1]]!="set.mat") {
                return ("First you have to use makeCacheMatrix function to use cacheSolve!")
        }
        
        inv <- x$get.inv_mat()    # Try to get the inverse matrix from inv variable defined in makeCacheMatrix
        if(!is.null(inv)) {       # If the inverse is present a message is displayed and the inv is returned without computation
                message("getting cached data")
                return(inv)
        }
        
        mat <- x$get.mat()
        inv <- solve(mat, ...)      # If inv is not retrieved the inverse is computed
        x$set.inv_mat(inv)          # After computation the inverse matrix is set in the makeCacheMatrix environment
        
}