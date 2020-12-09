## The functions defined below are used to compute and store the inverse of the
## invertible matrix in cache if the inverse of the matrix doesn't exist

## The function defined below is used to make cached matrix with sub functions:
## set: To set the cached matrix
## get: To get the cached matrix
## setinverse: To set the inverse of the cached matrix in the cache
## getinverse: To get the inverse of the cached matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get<-function() x
  getinverse<-function() inverse
  setinverse<-function(i) inverse<-i
  
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)

}


## This function is used to check whether the inverse of the cached matrix
## is stored in the cache or not
## If the inverse of matrix exist in cache, the inverse of matrix is returned

cacheSolve <- function(x, ...) {
        ## Get the inverse of the matrix from cache
        inverse<-x$getinverse()
        
        ## Check if inverse of the matrix exists in cache
        if(!is.null(inverse)){
          print('Getting cached data')
          return(inverse)
        }
        
        ## If inverse of matrix is not stored in cache, get the cached matrix
        data<-x$get()
        ## Calculate the inverse of the cached matrix
        if(nrow(data)==ncol(data)&& det(data)!=0){
          inverse<-solve(data)
        }
        ## Store inverse of matrix in the cache
        x$setinverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse
        
}
