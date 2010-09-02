## Simulator for gene expression data.
## Gene expression values are normally distributed values with zero mean and correlations given 
## by a configurable blockdiagonal matrix. Half of the samples contain differential gene expression values.
##
## It generates two classes: 
## label 1: samples with differentially expressed genes (first half of the samples).
## label -1: samples without differentially expressed genes (second half of the samples).
##
## input:	
##	samples: 	number of samples (default: 50)
##	genes:		number of gene expression values per sample (default: 10.000)
##	diffgenes:	number of differential genes (default: 200)
##	blocksize:	size of each block in the blockdiagonal correlation matrix (default: 50)
##	cov1:		covariance within the blocks in the covariance matrix (default: 0.2)
##	cov2:		covariance outside the blocks in the covariance matrix (default: 0)
##	diff:		difference between the random gene expression values and the differential gene expression values (default: 0.6) 
##	diffsamples:	number of samples containing differential genes (default: half of the given samples)
##
## output:
##	list containing a (samples x features)-matrix with the simulated gene expression values and a vector with labels (1,-1) for the two classes


generateData <- function(samples=50, genes=10000, diffgenes=200, blocksize=50, cov1=0.2, cov2=0, diff=0.6, diffsamples){

	#catch missing or bad input	
    #        if(!require(MASS))
    #            stop("package MASS required for data simulation")

	if(genes <= 0 | samples <= 0 | diffgenes <= 0 | blocksize <= 0){
		stop("input value(s) <= 0; aborting")
	}
	if(diffgenes > genes){
		warning("more differential genes than genes; setting value to ", round(genes/2))
		diffGenes = genes;		
	}
	if(blocksize > genes){
		warning("blocksize for the covariance matrix too high; setting value to ", round(genes/10))
		diffGenes = genes;		
	}
	if(missing(diffsamples)){
		diffsamples = samples / 2
	}else
	if(diffsamples > samples | diffsamples < 1){
		diffsamples = samples / 2
		warning("diffsamples is either higher than the total number of samples or smaller than 1; setting diffsamples to ", diffsamples)
	}

	## Create labels: 
	## First half is of class 1 (1), second half of class 2 (-1)
	labels = c(rep(1,diffsamples), rep(-1,(samples - diffsamples)))

	## Build correlation matrix:
	## It is a blockdiagonal matrix with values cov1 within the blocks, cov2 outside and 1 on the main diagonal
	sigma = matrix(cov2,genes,genes)
	for(i in seq(1,floor(genes/blocksize)))
		sigma[ ((i-1)*blocksize+1):(i*blocksize),((i-1)*blocksize+1):(i*blocksize)  ] = cov1
	diag(sigma) = 1

	## Create the base data:
	#Normally distributed values with zero mean and given covariances
	data = mvrnorm(samples,rep(0,genes),sigma)

	## Manipulate random data:
	## For the first half of samples, raise a given number of differential genes by a given value (0.6 by default)	
	some_diffExprs = rep(diff,diffgenes)
	some_diffGenes = sample(genes,diffgenes)
	data[1:diffsamples,some_diffGenes] = data[1:diffsamples,some_diffGenes] + c(some_diffExprs)

	return(list(data,labels))		

}
