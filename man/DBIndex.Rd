\name{DBIndex}
\alias{DBIndex}
\title{
	Davis-Bouldin-Index
}
\description{
	Computes the Davis-Bouldin-Index for cluster validation purposes.
}
\usage{
DBIndex(data, labels)
}
\arguments{
  \item{data}{
	N x D matrix (N samples, D features)
	}
  \item{labels}{
	a vector of class labels
	}
}
\details{
	To compute a clusters' compactness, this version uses the Euclidean distance to
	determine the mean distances between the samples and the cluster centers.
	Furthermore, the distance of two clusters is given by the distance of their centers.
}
\value{
	'DBIndex' returns the Davis-Bouldin cluster index, a numeric value.
}
\author{
	Christoph Bartenhagen
}
\examples{
## DB-Index of a 50 dimensional dataset with 20 samples separated into two classes
d = generateData(samples=20, genes=50, diffgenes=10, blocksize=5)
DBIndex (data=d[[1]], labels=d[[2]])

}
