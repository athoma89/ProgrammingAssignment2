## functions to make and solve the inverse of a matrix

## creates a cache matrix that stores the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m<<-solve
  getinv<-function() m
  list(set=set,get=get, ##creates list with the matrix and its inverse
       setinv=setinv,
       getinv=getinv)
}


## checks to see if matrix has been solved, and if not, solves matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){ ##if m is not empty, pulls answer from makeCache
      message("getting cached data")
      return(m) ##return previously solved answer
  }
  data<-x$get() ##gets matrix from earlier list
  m<-solve(data,...) ##solves matrix
  x$setinv(m) ##sets solution for future use
  m
}
