##The makeCacheMatrix function adds the getters and setters for the majeCacheMatrix object, setting the variables to the parent environment
##making them available for calling the cached values
#1.  Set the inversematrix variable (im) to null as each call creates a new x value
#2. the set function with the (y) variable which is not defined is used to set the x matrix using a variable in the parent environment (y)
#3. the matrix variable (x) is added to the parent environment where the functions are defined using the value of (y).  The (x) will be used later to calculate the inverse matrix
#4. The inverse matrix variable (im) is set to null as a new value will be calculated using the updated x value
#5. A call to get() function simply returns the variable(x) to be used in the inversed matrix calculation in the cacheSolve function
#6. A call to setinverse() function is used to set the inverse variable variable (im) once it has been calculated in the cacheSolve function
#7. A call to getInverse() returns the inverse matrix value or returns null.  When null is returned to the call from cacheSolve function, a new inverse matrix is calculated.
#8. Returns a list of available function calls

makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL #(1) 
  
  set <- function(y){  #(2)
    x <<- y  #(3)
    im <<- NULL #(4)
    
  }
  get <- function() x  #(5)
  setinverse <- function(inversematrix) im <<- inversematrix  #(6)
  getinverse  <- function() im (#8)
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinvers) #(8)

}


## The cacheSolve function returns a matrix that is the inverse of 'x' by taking the following actions:
#1. the call to cacheSolve accepts a makeCacheMatrix object of x
#2. makeCacheMatrix.getinverse is called using x$getinverse
#3. if the cached value of im is not null (!null) 
#4. the we see the message 'getting cached data, im is returned.  Finished
#5. if x$getinverse returns null then we move to the next section of the code
#6. get the x variable as it was set with the call to x$set where y was defined in a global environment
#7. Using the data variable which is a square matrix solve for the inverse
#8. call the makeCacheMatrix.setinverse( x$setinverse(inversematrix)) passing in the results of solve
#9. return the value of the solve formula  (you could also call x$getinverse as it was just set)

cacheSolve <- function(x, ...) { #(1)

  im <- x$getinverse() #(2)
  if(!is.null(im)){ #(3)
    message("getting cached data") #(4)
    return(im)  #(4)
  }
  
  #(5)
  data <- x$get() #(6)
  im <- solve(data)%*%data #(7)
  x$setinverse(im) #(8)
  im #(9)
  
}
