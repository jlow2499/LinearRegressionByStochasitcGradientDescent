mse.cost <- function(y,yhat,n){
	sum(1/2*(y - yhat) ^ 2) / n
})
mse.gradient <- function(y,yhat){
yhat - y
}

l1.cost <- function(y,yhat,n){
	abs(sum((y - yhat) ))/n
}

l1.gradient <- function(y,yhat){
	(yhat-y)/abs(yhat-y)
}

l2.cost <- function(y,yhat,n){
	sum((y - yhat) ^ 2)/n
}

l2.gradient <- function(y,yhat){
	2 * (yhat-y)
}

gradientDesc <- function(x, y, learn_rate, conv_threshold, max_iter,cost_function) {
  
	if(identical(cost_function,mse.cost)){
		gradient_fun <- mse.gradient
	}else if(identical(cost_function,l1.cost)){
		gradient_fun <- l1.gradient
	}else{
		gradient_fun <- l2.gradient
	}
	
	m <- 0
  c <- 0
  n <- nrow(x)
  yhat <- m * x + c
  cost <- cost_function(y,yhat,n)
  converged <- F
  i <- 1
 	gradient.info <<- data.frame()
  
  while(converged == F) {
    ## Implement the gradient descent algorithm
  	gradient_m <- (sum((gradient_fun(y,yhat)) * x))
  	gradient_c <- (sum(gradient_fun(y,yhat)))
    m_new <- m - learn_rate * ((1 / n) * gradient_m)
    c_new <- c - learn_rate * ((1 / n) * gradient_c)
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    cost_new <- cost_function(y,yhat,n)
    if(cost - cost_new <= conv_threshold) {

      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    
    if(i %% 10000 == 0){
    gradient.info <<- rbind(gradient.info,data.frame(cost_new,m,c))
    }
    
    i = i + 1
    
    if(i > max_iter) { 

      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
 	names(gradient.info) <<- c('Cost','Slope','Intercept')

}


# Run the function 
x <- as.matrix(A[,2])
y <- B

 gradientDesc(x, y, 0.0001, 0.01,  2500000,cost_function=l1.cost)
 
 
 deriv(~Y*log(cosh(X/Y)),'X')
 
 
 gradient Y * (sinh(X/Y) * (1/Y)/(cosh(X/Y)))
