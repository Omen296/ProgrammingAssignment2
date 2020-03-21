##The function makeCacheMatrix creates a "matrix object" that can be called after with cachesolve. Cachesolve, searches for the cache and 
##if it doesnt exist it just solves it and sets the value for future operations

## Creates a list with different functions that will serve to recall the values from the other enviroment if they exist

makeCacheMatrix<-function(x=matrix()) {
  m<-NULL             #sets m as NULL on this enviroment to make it so it can used on the rest of the function
  get<-function() x   #stores the actual matrix in a way that can be recalled by the next function
  setsolve<-function(solve) m<<-solve #produces the function that will "receive" the value in the other function
  getsolve<-function() m  #stores the inverted value to be checked in the next function
  list(get=get,           #the "matrix"
       setsolve=setsolve,
       getsolve=getsolve)
}

cacheSolve <- function(x, ...) {
  m<-x$getsolve()   #gets the value of m
  if(!is.null(m)) {   #and if there is a prior solution, it tells you so
    message("Retrieving inverse matrix from cache")  #The message
    m #It returns the value of m from cache
  }
  else{           #The else is not really necessary but it makes everything clearer   
  data<-x$get()   #recovers the data
  m<-solve(data) #solves m
  x$setsolve(m) #sets the value for future recallings
  m #provides the output
  }
}





