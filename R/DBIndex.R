## This function computes the Davis-Bouldin-Index for cluster validation purposes.
## To compute a clusters' compactness, this version uses the Euclidean distance to 
## determine the mean distances between the samples and the cluster centers.
## Furthermore, the distance of two clusters is given by the distance of their centers.  
## 
## input:	
##	data: NxD matrix (N sample, D features)
##	labels: a vector of class labels
##
## output:	
##	the Davis-Bouldin-Index

DBIndex <- function(data, labels){

    #catch missing or bad input
    if(missing(data))
	stop("data argument missing")
    if(missing(labels))
	stop("label argument missing")
    if(nrow(data) != length(labels))
	stop("number of samples and number of labels inconsistent")

    dim = ncol(data)

    ## separate data into a cluster for each class
    labels = as.factor(labels)
    classes = levels(labels)
    numClusters = length(classes)
    clusters = list()
    clusterSizes = vector(length=numClusters)
    for(i in 1:numClusters){
        clusters[[i]] = data[which(labels == classes[i]), ]
	clusterSizes[i] = nrow(clusters[[i]])
    }

    ## compute cluster centers and mean Euclidean distances between all samples and their center
    clusterCenters = matrix(0, dim, numClusters)
    meanDist = vector(length=numClusters)
    for(i in 1:numClusters){
    	clusterCenters[ , i] = 1 / clusterSizes[i] * colSums(clusters[[i]])
	meanDist[i] = 1 / clusterSizes[i] * sum(sqrt(colSums((t(clusters[[i]]) - clusterCenters[ , i])^2)))
    }

    ## compute relation between the compactness of two clusters and their distance (i.e. distance of their centers)
    R = matrix(0, numClusters, numClusters)
    for(i in 1:numClusters)
	R[i, ] = (meanDist[i] + meanDist) / sqrt(sum((clusterCenters[ , i] - clusterCenters)^2))
    diag(R) = NaN

    ## select maximum relation for each cluster
    R_max = apply(R, 2, max, na.rm=TRUE)

    ## compute DB-Index: mean of all selected maxima
    return( 1 / numClusters * sum(R_max) )

}
