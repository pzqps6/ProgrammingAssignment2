#-------------------------------------------------------------------------------
# This function creates the inverse matrix object and cahches the same 
# It handles the getter and setter functionalities of this inverse matrix
# NOTE: SUPPLIED MATRIX SHOULD BE INVERTIBLE. NO CHECK DONE IN THIS FUNCTION
#-------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
       # set the matrix_inverse variable to NULL 
       mi <- NULL
       
       # set function for matrix. Also reset matrix_inverse
       set <- function ( y )
       {
              x <<- y
              mi <<- NULL
       }
       
       # get the matrix set (not the inverse )
       get <- function () x
       
       # set the matrix_inverse. No computation done. Only value set
       setinv <- function ( solve ) mi <<- solve
       
       # get the inverse matrix
       getinv <- function () mi
       
       # set the list of all the environment valus. This is not needed
       list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#-------------------------------------------------------------------------------
# This function computes the inverse of the matrix if it could not retreive from
# the makeCacheMatrix function     
# NOTE: SUPPLIED MATRIX SHOULD BE INVERTIBLE. NO CHECK DONE IN THIS FUNCTION
#-------------------------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
       # get the catched data
       m <- x$getinv()
       
       # check if chached data is null
       if ( !is.null (m) )
       {
              message ("getting cached data")
              return (m) #return catched data
       }
       
       #get data 
       m <- x$get()
       
       # get the inverse
       mi <- solve (m)
       
       # store the inverted matrix
       x$setinv( mi )
       mi
}
