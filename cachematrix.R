## Assumption: All supplied matrices are invertible.
## I religiously followed the example of cachemean.
## To run individual functions, simply select the 
## part of the code and click Run on top right of R-Studio. 

## Let us first construct a square invertible matrix :
## Example :
## x <- matrix(c(1,3,10,-5,24,17,51,-7,13), nrow=3, ncol=3)
## x
##      [,1] [,2] [,3]
## [1,]    1   -5   51
## [2,]    3   24   -7
## [3,]   10   17   13
## fun1 <- makeCacheMatrix(x)
## Printing out fun1 (class: "list"), since the last
## statement in the function asks for a list of the function definitions,
## we get an output as follows:
## fun1
## $set
## function(y) {
##   x <<- y
##   i <<- NULL
## }
## <bytecode: 0x7fa78e0f1890>
##   <environment: 0x7fa7903ca230>
##  
## $get
## function() x
## <bytecode: 0x7fa78e144a58>
##  <environment: 0x7fa7903ca230>
##  
## $setinverse
## function(inverse) i <<- inverse
## <bytecode: 0x7fa78e1b69e8>
##  <environment: 0x7fa7903ca230>
##  
## $getinverse
## function() i
## <bytecode: 0x7fa78e20dac0>
##  <environment: 0x7fa7903ca230>

makeCacheMatrix <- function(x = matrix()) {
## Initialize the inverse to NULL  
       i <- NULL 
## Set function; sets the input matrix; inverse is still set to NULL       
       set <- function(y) {
         x <<- y
         i <<- NULL
       }
## Get function; gets the input matrix       
       get <- function() x
## Set function for inverse matrix       
       setinverse <- function(inverse) i <<- inverse
## Get function for inverse matrix       
       getinverse <- function() i
## Collect the set and get functions of the input and inverse matrices
## into a list:       
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## The cacheSolve function computes the inverse matrix
## and caches the result.
##
## Output example with above matrix:
## fun2 <- cacheSolve(fun1)
## fun2
##             [,1]         [,2]         [,3]
## [1,] -0.04975182 -0.107583978  0.137250375
## [2,]  0.01258225  0.057370426 -0.018469352
## [3,]  0.02181692  0.007734041 -0.004501905
##
## If I re-compute this inverse, I get the message:"getting cached inverse":
## fun3 <- cacheSolve(fun1)
## fun3
## getting cached inverse
##             [,1]         [,2]         [,3]
## [1,] -0.04975182 -0.107583978  0.137250375
## [2,]  0.01258225  0.057370426 -0.018469352
## [3,]  0.02181692  0.007734041 -0.004501905

cacheSolve <- function(x, ...) {
## Obtain the current status of the inverse matrix  
        i <- x$getinverse()
## If it is cached, it retreives the cached inverse        
        if(!is.null(i)){
          message("getting cached inverse")
          return(i)
        }
## If not, it gets the input matrix        
        data <- x$get()
## and computes it's inverse with the in-built solve() function        
        i <- solve(data,...)
## This output is then cached for later use.        
        x$setinverse(i)
## The inverse matrix is printed out.        
        i
}
