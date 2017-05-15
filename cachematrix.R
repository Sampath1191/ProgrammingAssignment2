## A function to calculate Matrix and another function to calculate and retrieve the inverse of matrix 
##returned by the makeCacheMatrix function
## makeCacheMatrix calculates the matrix
##CacheSolve retrieves the inverse of the matrix created from makeCacheMatrix

## Function for creating the Matrix type for cache

makeCacheMatrix <- function(x = matrix()) {
                         inv<-NULL
                         set<-function(mat){
                                   x<<-mat
                                   inv<<-NULL
                        }
                    get<-function(){x}
                    setinverse<-function(solve){inv<<-solve}
                    getinverse<-function(){inv}
                    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Function for solving the inverse of matrix from the above function

cacheSolve <- function(x, ...) {
                        inv<- x$getinverse()
                        if(!is.null(inv)) {
                                message("getting cached data")
                                return(inv)
                                }
                data <- x$get()
                inv<- solve(data, ...)
                x$setinverse(inv)
                inv
}
