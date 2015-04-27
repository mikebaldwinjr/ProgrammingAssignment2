


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##First you give a value to a matrix x
##Then, You pass the value of matrix x into the makeCacheMatrix Function
##The function then creates an inverse of the supplied matrix
##the function then gets the inverse and loads to memory

makeCacheMatrix <- function(x = matrix())
{
  inv<-NULL
  set<-function(y)
  {
  x<<-y
  inv<<-NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
} 


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##if inv is not null, then this function returns data from the cache 
##Otherwise it will calculate the inverse and return it's value

cacheSolve <- function(x, ...)
{
  inv <- x$getinverse()
  if(!is.null(inv))  
  {
  message("Retrieve data from the cache.")
  return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
