##The function makeCacheMatrix creates a "matrix object" that can be called after with cachesolve. Cachesolve, searches for the cache and 
##if it doesnt exist it just solves it and sets the value for future operations

## Creates a list with a matrix and an space for its inverse in cache to be called after

makeCacheMatrix <- function(x = matrix()){
  inv=NULL
  ##THE PROBLEM STARTS HERE, getdara, setinv and getinv end up being the functions instead of the data proper
  setdata<-function(y)          {    #Creates a vector that has the data of x and m  
    x<<-y                            #And sends those values to another enviroment
    inv<<-NULL
    }
  getdata<-function () x                           
  setinv <- function(solve) inv <<- solve   
  getinv <- function() inv
  list(setdata = setdata, getdata = getdata, setinv = setinv,
       getinv = getinv)  #  We create a list to get them in cacheSolve
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinv #Gets the value of inv if it exists
    if(!is.null(inv)){
      message("Getting inverse matrix from cache") #And tells you so
      return(inv)
    }
    inv<-solve(x$getdata) #Produce the inverse and set it as a cached value
    x$getinv(inv) #Sets inv in the matrix
    inv #Prints the value
  }



d<-c(2,3)
e<-c(4,3)
z<-data.frame(d, e)
solve(z)
D<-makeCacheMatrix(z)
P<-cacheSolve(D)
solve(z)==P







