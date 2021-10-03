## Put comments here that give an overall description of what your
## functions d

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
   set <- function(b){     #set matrix
     x <<-b #alterando variaveis globais
     inversa <<- NULL #inverse property
   }
   get <- function(){x} #get matrix
   setInverse <- function(inverse){inversa <<- inverse}
   getInverse <- function(){inversa}
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #list of methods
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversa <- x$getInverse()
    if(!is.null(inversa)){
       message('gettting data')
       return(inversa)
  }
  
  mtx <- x$get()
  inversa <- solve(mtx, ...) #matrix multiplication - calculate inverse
  x$setInverse(inversa)
  inversa                 
  }
