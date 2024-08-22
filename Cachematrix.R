## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix consists of set,get,set.inv,get.inv
##Library(MASS) is used to calculate inverse for non squared as well as sqaure matrices


library(MASS)
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL           #inverse as null
  set <- function(y) {
             x <<- y
             inv <<- NULL
                     }
 
  
   get <- function() x        #function to get matrix x, replacing mean with inverse
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){ 
                       inver <-ginv(x)
                       inver%*%x         #function to obtain inverse of the matrix
  }
  
 
   list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}





## Write a short comment describing this function
## This is used to get the cache data


cacheSolve <- function(x, ...)  ## cache data
  {
    inv <- x$getinv()
    if(!is.null(inv)) {
      
      message("getting cached data")
      return(inv)                        #returns inverse value
    
    }
    
    data <- x$get()
    inv <- solve(data, ...)            #calc inverse value
    x$setinv(inv)
    inv            ## Return a matrix that is the inverse of 'x'
}
