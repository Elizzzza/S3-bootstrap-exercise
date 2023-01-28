## S3 method bootstrap
## Constructor functions to make sure the objects is class: stratified
stratified <- function(y, strata) {
        if (!is.numeric(y)) stop("'y' must be numeric")
        if (!is.factor(strata)) stop("'strata' must be a factor")
        
        if (length(y) != length(strata)) stop("'y' and 'strata' must have equal length")
        
        structure(list(y=y, strata=strata), class = "stratified") 
}

## Create a generic bootstrap function
bootstrap <- function(object, ...) UseMethod("bootstrap")

## Create a method for numeric vectors
bootstrap.numeric <- function(object, nboot, stat){
        if ( !is( object, "numeric") ) 
                stop( "bootstrap.numeric requires an object of class 'numeric'" )
        if ( nboot < 1 | is.infinite(nboot) ) 
                stop( "'nboot' should be a positive integer" )
        
        n <- length(object)
        
        boot_samp <- replicate(nboot, sample(object, size=n, replace=TRUE))
        
        colnames(boot_samp) <- paste("b", 1:nboot, sep="")
        
        boot_stat <- apply(boot_samp, 2, stat)
        
        return(boot_stat)
}

## Create a method for stratified vectors
bootstrap.stratified <- function(object, nboot, stat){  
        if ( !is( object, "stratified") ) 
                stop( "bootstrap.stratified requires an object of class 'stratified'" )
        if ( nboot < 1 | is.infinite(nboot) ) 
                stop( "'nboot' should be a positive integer" )
        
        tapply(object$y, object$strata, bootstrap.numeric, nboot, stat)
}


x <- rnorm(5)
x_mean <- bootstrap(x, 100, mean)
x_median <- bootstrap(x, 100, median)
x_sd <- bootstrap(x, 100, sd)

x_str <- stratified(y = c(rnorm(5), rnorm(5, 3)), 
                    strata = factor(rep(c("a","b"), each=5)) ) 

x_str_mean <- bootstrap(x_str, 100, mean)
x_str_median <- bootstrap(x_str, 100, median)
x_str_sd <- bootstrap(x_str, 100, sd)

## Create histograms 
par(mfrow=c(1,3)) 
hist(x_mean)
hist(x_median)
hist(x_sd)

par(mfrow=c(2,3)) 
hist(x_str_mean[[1]], xlab= "x_str_mean for a")
hist(x_str_median[[1]], xlab= "x_str_median for a")
hist(x_str_sd[[1]], xlab= "x_str_sd for a")
hist(x_str_mean[[2]], xlab= "x_str_mean for b")
hist(x_str_median[[2]], xlab= "x_str_median for b")
hist(x_str_sd[[2]], xlab= "x_str_sd for b")

## Generalize the methods to the case of an argument that take additional arguments
moment <- function(x, k)
{
        (1/length(x))*sum((x-mean(x))ˆk)
}

## Create a generic bootstrap function
bootstrap <- function(object, ...) UseMethod("bootstrap")
## Create a method for numeric vectors
bootstrap.numeric <- function(object, nboot, stat, k){
        if ( !is( object, "numeric") ) 
                stop( "bootstrap.numeric requires an object of class 'numeric'" )
        if ( nboot < 1 | is.infinite(nboot) ) 
                stop( "'nboot' should be a positive integer" )
        
        n <- length(object)
        
        boot_samp <- replicate(nboot, sample(object, size=n, replace=TRUE))
        
        colnames(boot_samp) <- paste("b", 1:nboot, sep="")
        
        boot_stat <- apply(boot_samp, 2, stat)
        
        stat <- function(x, ...){
                moment(...)
        }
        
        moment <- function(x, k)
        {
                (1/length(x))*sum((x-mean(x))^k)
        }
        return(boot_stat)
}


bootstrap.stratified <- function(object, nboot, stat){  
        if ( !is( object, "stratified") ) 
                stop( "bootstrap.stratified requires an object of class 'stratified'" )
        if ( nboot < 1 | is.infinite(nboot) ) 
                stop( "'nboot' should be a positive integer" )
        
        tapply(object$y, object$strata, bootstrap.numeric, nboot, stat)
}


