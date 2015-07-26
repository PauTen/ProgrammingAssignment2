######Assignment 2: Lexical Scooping############
#In this code, the R Scooping rules are used to optimize the calculation of a matrix inverse#
#
#makeCacheMatrix gives as output a list of 4 functions: set and get the value of the matrix, set the inverse
#of the matrix and get the inverse of the matrix. When applied to an input, it stores the matrix and
#the inverse in the cache.
makeCacheMatrix<- function(x=matrix()){
  m <- NULL
  set <- function(y){
    x<<-y 
    m<<-NULL
  }
  get <-function() x
  setinverse <-function(solve) m<<-solve
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
} 
#
#cacheSolve: checks if the inverse is stored in the cache and in case it isn't, gets the inverse.
cacheSolve <- function(x,...){
  m <-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}