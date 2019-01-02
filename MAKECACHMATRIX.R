# The code below can be run with the command lines
###     m<-matrix(c(1:4),nrow = 2, ncol = 2)
###     inv<-runfunctions(m)

# makeCacheMatrix() is based on the makeVector() example 
# provided with this assignment.  Accordingly, the function 
# gets or sets a matrix or its inverse

makeCacheMatrix <- function(myMatrix = matrix()) {
    # since a new matrix is being constructed, set its
    # to inverse to NULL since it has not been calculated
    myInverse<-NULL
    # define a function to store the matrix
    setmatrix<-function(y) {  
        myMatrix<<-y
        # new data
        myInverse<<-NULL
    }
    # return the current matrix
    getmatrix<-function() myMatrix
    # set the result of the call to solve
    setinverse<-function(z) myInverse<<-z
    # get the current solution (inverse)
    getinverse<-function() myInverse
    # return function 'pointers'
    list(setmatrix  = setmatrix,
         getmatrix  = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function checks to see if there is already a solution.
# If a solution exists, it is returned.
# If there is no solution, one is calculated and returned.
cacheMatrixSolve <- function(myMatrix, ...) {
    # Get whatever is stored
    m<-myMatrix$getinverse()
    # if the inverse already exists, return it
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    # if the inverse does not exist, calculate it
    message("creating new inverse")
    data<-myMatrix$getmatrix()
    myInverse<-solve(data)
    myMatrix$setinverse(myInverse)
}

# This function just calls those above in a logical manner
runfunctions<-function(myMatrix) {
    # create the catch
    m1<-makeCacheMatrix(myMatrix)
    # calculate inverse
    i1<-cacheMatrixSolve(m1)
    # to demonstrate this works, try inverting again
    # and check that stored data is returned and no new 
    # calculations occur.
    i2<-cacheMatrixSolve(m1)
    i2
}
