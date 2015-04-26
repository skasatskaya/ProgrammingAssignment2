##With these two functions you can compute inverted matrix out of any invertible matrix, from a long list, for example.
##Caching of matrix and its inverted matrix (inverse variable) helps in saving time.

## makeCacheMatrix takes a matrix (an invertible one) as an argument and returns a list of functions. 
## Therefore we can use makeCacheMatrix(x)$ to address result of one of 4 functions which take place in makeCacheMatrix.
## makeCacheMatrix stores matrix and inverted matrix in inverse variable.

makeCacheMatrix<- function(x=matrix()){ 
    inverse<-NULL
    set <- function(y) {
        x <<- y ## this step we need to put our new matrix next to set function 
        inverse <<- NULL ## at the same time we need to make zero of previous solved matrix in case we've changed matrix
    }
    get <- function() x ## this function returns matrix, which is stored currently in x
    getinverse<-function() inverse
    setinverse<-function(inverse){inverse<<- solve}
    list(set=set, get=get, getinverse=getinverse,setinverse=setinverse);
}


## cacheSolve function return inverted matrix. We should use it with makeCacheMatrix as an argument 
## and a matrix should be used as a nested argument of makeCacheMatrix.

cacheSolve<- function(x, ...){
    inverse <-x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    else
        data<-x$get()
    inverse<-solve(data, ...)
    x$setinverse(inverse)
    inverse
}
        ## Return a matrix that is the inverse of 'x'
