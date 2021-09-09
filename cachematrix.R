## These functions take an argument x, a matrix, and return its inverse. The first function takes x 
## as an argument and outputs a list that we save in the global environment. The second function take 
## the list as an argument and retrieves elements from the list to either return or calculate the 
## inverse of the matrix entered in the first function.

## Function 1
## Creates a list with the following elements: setmatrix(), getmatrix(), setinverse(), getinverse(). 
## It also creates x and an empty matrix, mx. setmatrix() sets the value of x and mx. The rest of 
## the function defines the other elements in the list. getmatrix = x, setinverse = sets mx to inverse 
## not yet defined, getinverse = mx. It also names these elements to be easier to subset in the 
## following function. This fuction is only half complete. It takes another fuction to complete it.

makeCacheMatrix <- function(x = matrix()) {
  mx<- NULL
  setmatrix<- function(y) {
    x<<-y
    mx<<-NULL
  }
  getmatrix<- function() x
  setinverse<-function(inverse) mx<<-inverse
  getinverse<- function() mx
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
}


## This function completes the fuction above. We enter the list saved from above as an argument, d. 
## It returns the matrix inverse as defined from above if it exists, or if the value is NULL, 
## it calculates the inverse using solve(), sets it to mx, inputs its value into the list from 
## function 1, and returns it.

cacheSolve <- function(d, ...) {
    mx <- d$getinverse()
    if(!is.null(mx)) {
      message("getting cached data")
      return(mx)
    }
    data <- d$getmatrix()
    mx <- solve(data, ...)
    d$setinverse(mx)
    mx
}
