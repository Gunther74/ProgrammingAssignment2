#########################################################################
#                                                                       #
#   How to use?                                                         #
#   [obj] <- MakeCacheMatrix ([inverable matrix])                       #
#   CacheSolve ([obj])                                                  #
#                                                                       #
#   e.g.                                                                #
#   mat <- MakeCacheMatrix (matrix(c(1,2,3,4,5,6,7,8,10),nrow=3,ncol=3) #
#   CacheSolve (mat)                                                    #
#                                                                       #
#   Whenever the same matrix has been inverted twice or more,           #
#   the message "Getting Cached data" should appear                     #
#                                                                       #
#                                                                       #
#########################################################################


####

#  MakeCacheMatrix creates a matrix object object which can inverse a matrix

####

MakeCacheMatrix <- function (x=matrix()){ # x =matrix
  m <- NULL
  set  <- function (y) {
    x <<- y
    m <<- NULL  
  }
  get  <- function()x
  setinverse <- function(solve) m<<-solve
  getinverse  <- function() m
  list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

####

#  CacheSolve computes the inverse of an matrix given thet object created with MakeCacheMatrix.
#  It caches the matrix and its inverse to avoid recalculation of the inverse.

####

CacheSolve  <- function(x,...){
  m <- x$getinverse()
  if (!is.null(m)){message ("Getting Cached data")
  return (m)}
  data <- x$get()
  m <- solve (data,...)
  x$setinverse(m)
  m
} 
