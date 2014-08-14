## The following functions take a given matrix and create a "matrix object"
## allowing the inverse matrix to be computed and cache accessible.
## Input matrix into makeCacheMatrix
##      - output is a list of functions (use names on output to see list)
## the output of makeCacheMatrix is passed to cacheSolve
##      - the inverse of the matrix is either computed or simply
##        returned without computing if it has already been computed.           

## makeCacheMatrix outputs a list to
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #inverse unassigned
        set <- function(y){ #set function
                x <<- y #assign from outside this functions environment
                inv <<- NULL
        }
        get <- function(){ #function to get the matrix
                x
        }
        setinv <- function(sol){ # function to set the inverse
                inv <<- sol # assign from outside function environment
        }
        getinv <- function(){ # function to get the inverse
                inv
        }
        #list out properties/functions
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve
## Grabs the inverse matrix from the cache if it exists
## if not it computes the inverse using the solve function.
## Note: This function assumes the input matrix is invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){ # if inv is not null, i.e has a value
                message("grabbing cached inverse") #inform user
                return(inv) # return the cached inverse
        }
        else{ # compute the inverse
                message("computing inverse")
                mat <- x$get() # get the matrix
                inv <- solve(mat, ...) # use solve to get inverse
                x$setinv(inv) # set the new inverse
                return(inv) # return the computed inverse
        }
}