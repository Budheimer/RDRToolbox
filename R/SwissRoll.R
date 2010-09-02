## This function computes the Swiss Roll dataset.
##
## input:	
##	N: number of samples (default 2.000)
##	Height: controls the spreading of the samples in the second dimension (default 30)
##
## output:	
##	a plot of the Swiss Roll and all N samples as a Nx3-matrix

SwissRoll <- function(N=2000, Height=30, Plot=FALSE){

    ## build manifold
    p = (3 * pi / 2) * (1 + 2 * runif(N, 0, 1));  
    y = Height * runif(N, 0 , 1);
    samples = cbind(p * cos(p), y, p * sin(p));

    ## plot and return samples
    if(Plot){
        ## load rgl for three dimensional plots
        # if(!require(rgl))
        #    stop("package rgl required for three dimensional plots")
    }else{
        	plot3d(samples, xlab="x", ylab="y", zlab="z");
    }
    return(samples)

}
