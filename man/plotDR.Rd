\name{plotDR}
\alias{plotDR}
\title{
	Plotting tool for two and three dimensional data
}
\description{
	Creates two and three dimensional plots of (labeled) data. It uses the library "rgl" for rotatable 3D scatterplots.
}
\usage{
plotDR(data, labels, axesLabels=c("x","y","z"), legend=FALSE, text, col, pch, ...)
}
\arguments{
  \item{data}{
	matrix with values to be plotted (rows correspond to samples, columns to features)
}
  \item{labels}{
	vector containing labels of the classes within the data (optional)
}
  \item{axesLabels}{
	vector containing labels for the axes of the plot
}
  \item{legend}{
	logical value whether to automatically insert a legend into the plot 
}
  \item{text}{
	vector with (short) labels for each point (optional)
}
  \item{col}{
	character vector of colours for each class (optional); see \code{colors()} to display a list of available colours
}
  \item{pch}{
	character or integer value specifying the symbol when plotting points (see \code{?par} or \code{?points} for more info)
}
  \item{...}{
	other common R plot parameters like for example \code{las}, \code{cex} or \code{font} to further customize the plot (see \code{?par} for 
	possible arguments); some parameters may only work with two dimensional plots
}
}
\details{
	It colours the data points according to given class labels (max. six classes when using default colours).
	A legend will be printed in the R console by default (for three dimensional plots, a legend is not supported).
}
\author{
	Christoph Bartenhagen
}
\examples{
## plot a two dimensional LLE embedding of a 1.000 dimensional dataset
d = generateData(samples=20, genes=1000, diffgenes=100, blocksize=10)
d_low = LLE(data=d[[1]], dim=2, k=5)
plotDR(data=d_low, labels=d[[2]])

## plot a three dimensional LLE embedding of a 1.000 dimensional dataset
d = generateData(samples=20, genes=1000, diffgenes=100, blocksize=10)
d_low = LLE(data=d[[1]], dim=3, k=5)
plotDR(data=d_low, labels=d[[2]])

## plot a two dimensional LLE embedding of a 1.000 dimensional dataset
## add axis labels, a legend and plot a text for each sample
d = generateData(samples=20, genes=1000, diffgenes=100, blocksize=10)
d_low = LLE(data=d[[1]], dim=2, k=5)
text = letters[1:20]
plotDR(data=d_low, labels=d[[2]], axesLabels=c("first component", "second component"), text=text, legend=TRUE)
## manually add a legend to the plot
plotDR(data=d_low, labels=d[[2]], axesLabels=c("first component", "second component"), text=text)
legend("topright", legend=c("class 1","class 2"), col=c("black", "red"), pch=1)

}
