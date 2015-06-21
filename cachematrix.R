## makeCacheMatrix function store value of input matrix and inverse matrix.

## cacheSolve function computes the inverse of the "input matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

## x  : input matrix
## im : inverse maxtrix
## solve(): function in R that computing inverse of a square matrix
## For example, if A is a square invertible matrix, then solve(A) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
          x <<- y
          im <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(a) im <<- a
        getInverseMatrix <- function() im
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## First, cache will check 'im' see whether 'im' is NULL(empty) value 

## IF 'im' is NOT a NULL value, 
## print message "getting cached inverse matrix" and then return 'im'

## IF 'im' is a NULL value, 
## print message "getting new inverse matrix" and then
## retrieve input matrix with get() thus assign 'im' with the result of solve()

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
        im <- x$getInverseMatrix()
        if(!is.null(im)) {
          message("getting cached inverse matrix")
          return(im)
        }
        message("getting new inverse matrix")
        data <- x$get()
        im <- solve(data)
        x$setInverseMatrix(im)
        im
}

## Screenshot of few example outputs has been attached,
## please check https://github.com/Char1120/ProgrammingAssignment2/Example_Output


