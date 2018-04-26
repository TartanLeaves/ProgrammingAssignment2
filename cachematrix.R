###############################################################################
###                                                                         ###
###    Peer-graded Assignment: Programming Assignment 2: Lexical Scoping    ###
###                                                                         ###
###############################################################################



## 1) A Function that takes a sqaure invertible matrix as argument and that:
        # - returns the matrix itself via get.x
        # - calculates the inverse of the matrix (without output) via calc.inv
        # - returns the inverse of the matrix, if it has been calculated, via get.inv

    makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        get.x <- function() x
        calc.inv <- function() inv<<-solve(x)
        get.inv <- function() inv
        
        list(
            get.x = get.x,
            calc.inv = calc.inv,
            get.inv = get.inv
            )
    }


## 2) A Function that calculates the inverse of a matrix, if it has not been stored in cache before

    cacheSolve <- function(x, ...) {
        
        # Get the inverse of the function from before:
        # (It is NULL if it has not been calculated)
        inv <- x$get.inv()
        
        if (!is.null(inv)) {
            message("getting cached data")
            return(Matrix$get.inv())
        }
        
        mat<-Matrix$get.x()
        inv<-solve(mat, ...)
        inv
        
    }
