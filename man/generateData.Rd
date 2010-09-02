\name{generateData}
\alias{generateData}
\title{Simulator for gene expression data}
\description{
A simulator for gene expression data, whose values are normally
distributed values with zero mean. The covariances are given by a
configurable block-diagonal matrix.
By default, half of the samples contain differential gene expression values (see parameter \code{diffsamples}).
}
\usage{
generateData(samples=50, genes=10000, diffgenes=200, blocksize=50, cov1=0.2, cov2=0, diff=0.6, diffsamples)
}
\arguments{
  \item{samples}{
	number of samples
}
  \item{genes}{
	number of gene expression values per sample
}
  \item{diffgenes}{
	number of differential genes for class 1
}
  \item{blocksize}{
	size of each block in the blockdiagonal correlation matrix
}
  \item{cov1}{
	covariance within the blocks in the correlation matrix
}
  \item{cov2}{
	covariance between the blocks in the correlation matrix
}
  \item{diff}{
	difference between the random gene expression values and the differential gene expression values
}
  \item{diffsamples}{
	number of samples containing differential gene expression values compared to the rest (if missing, this parameter is set to half of the total number of samples)
}
}
\details{
The simulator generates two labeled classes:\cr
label 1: samples with differentially expressed genes. \cr
label -1: samples without differentially expressed genes. 
}
\value{
	'generateData' returns a list containing:
	\item{data}{a (samples x features)-matrix with the simulated gene expression values}
	\item{labels}{a vector with labels (1,-1) for the two classes}
}
\author{
	Christoph Bartenhagen
}
\examples{
## generate a dataset with 20 samples and 1.000 gene expression values
d = generateData(samples=20, genes=1000, diffgenes=100, blocksize=10)
data = d[[1]]
labels = d[[2]]
}
