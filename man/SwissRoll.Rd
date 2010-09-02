\name{SwissRoll}
\alias{SwissRoll}
\title{
	The Swiss Roll dataset
}
\description{
	Computes and plots the Swiss Roll dataset of a given size and height.
	It uses the library "rgl" for rotatable 3D scatterplots.
}
\usage{
SwissRoll(N = 2000, Height = 30, Plot=FALSE)
}
\arguments{
  \item{N}{
	number of samples
	}
  \item{Height}{
	controls the spreading of the samples in the second dimension
	}
  \item{Plot}{
	a boolean specifying whether to plot the Swiss Roll dataset or not
	}
}
\value{
	'SwissRoll' returns all N samples as a Nx3-matrix
}
\author{
	Christoph Bartenhagen
}
\examples{
## compute and plot a Swiss Roll dataset with 1.000 samples
data=SwissRoll(N = 1000, Plot=TRUE)
}
