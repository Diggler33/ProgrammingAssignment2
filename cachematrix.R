

###################################################################
# Assignment 3: Caching the Inverse of a Matrix
###################################################################

## The following code is comprised of two functions

## Function 1: makecachematrix
## This function takes a matrix as an input parameter and caches it.

## Function 2: cachesolve
## This function uses the solve() function to inverse the cached 
## matrix created in 'makecachematrix' following a check that it 
## hasn't already been inversed. In this case it will return the
## already cached matrix

###################################################################


## Function 1: Caches a matrix
makecachematrix <- function(mymatrix = matrix()){
  
  m <-NULL 
  
  set <- function(y){
    mymatrix<<- y
    m <<- NULL
  }
  
  get <- function() mymatrix
  
  setinverse <- function(inverse)m <<- inverse
  
  getinverse <- function()m
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse )
  
}

## Function 2: Inverses cached matrix
cachesolve <- function(mymatrix,...) {
  
  m<-mymatrix$getinverse()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- mymatrix$get()
  m <-solve(data,...)
  mymatrix$setinverse(m)
  m
  
}

###################################################################
# Code used to test functions
###################################################################

# Create a Matrix
my.matrix <- matrix(c(1,0,3,2,2,4,3,2,1),ncol = 3)

# Cache Matrix
cache.matrix <- makecachematrix(my.matrix)

#Test 1: Should Cache the matrix and then inverse
cachesolve(cache.matrix)

#Test 2: Should use already inversed matrix that was cached & return message "getting cached matrix"
cachesolve(cache.matrix)

