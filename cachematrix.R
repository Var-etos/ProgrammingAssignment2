


## makeCacheMatrix is a function that takes arguement 'x' where 'x' is a matrix

makeCacheMatrix <- function(x = matrix()) {

  # Object inv is NULL
  inv <- NULL
  
  # 'set' is a function that takes arguement y
    set <- function(y) {
  # y is x therefore y is a matrix
      x <<- y
  # Object inv is NULL
      inv <<- NULL
    }
  # 'get' is a function that return 'x' the original matrix that was use as an arguement in the makeCacheMatrix   
    get <- function() x
  # 'setinv' is a function that take arguement solveinverse where 'solveinverse' is a matrix 'inv'
    setinv <- function(solvedinverse) inv<<- solvedinverse
  # 'getinv' is a function that returns the matrix 'inv' 
    getinv <- function() inv
  # return a list is return with the 4 above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  

}


## cacheSolve is a function with arguement x where in our case x is a list of 4 functions
cacheSolve <- function(x, ...) {
  # 'inv' retrieves the cached inverse and if its not null it return this cached inverse as an 'inv' matrix
  inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
    return(inv)
    }
  # Otherwise the data are set to the 'get' function which returns the original x matrix
    data <- x$get()
  # New 'inv' object solves for the inverse of the data('x' matrix)
    inv<- solve(data, ...)
  # 'setinv' is passed on the new 'inv' inverted matrix
    x$setinv(inv)
  # we return 'inv' inverted matrix 
    inv
  }


## Creating matrix, creating list of functions x1 based on matrix x, solving for x1
x<-matrix(rnorm(10000),100,100)
x1<-makeCacheMatrix(x)
cacheSolve(x1)















