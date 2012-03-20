## Plotting tool for two and three dimensional data.
## It colours the data points according to given class labels (max. six classes).

## input:
##	data:		matrix with values to be plotted (rows correspond to samples, columns to features)	
##	labels:		vector containing labels of the classes within the data
##	axesLabels:	vector containing labels for the axes of the plot
##	text:		vector with (short) labels for each point
##	legend:		boolean whether to include a legend into the plot (printed into the console otherwise)
##
## output:
##	no data output, just a nice plot and its legend


plotDR = function(data, labels, axesLabels=c("x","y","z"), legend=FALSE, text, col, pch, ...){

	## catch missing or bad input
	if(missing(data))
		stop("data argument missing: aborting")
	if(missing(labels))
		labels = "all"
	if(missing(pch))
		pch = 19

	## preparations, e.g. factoring the labels to get the different classes
	defaultCols = c("#000000",rainbow(40)[c(1,27,15,6,21)])
	defaultColNames = c("black","red","blue","green","yellow","cyan")
	if(missing(col)){
		col = defaultCols
		colour_names = defaultColNames
	}else
		colour_names = col

	labels = as.factor(labels)
	classes = levels(labels)	
	num_classes = length(classes)
	data_dim = dim(data)[2]
	data_points = dim(data)[1]

	## check dimensionality of the data argument (too low/high?)
	if(is.null(data_dim))
		stop("dimension of given data is too low: aborting")
	if(data_dim > 3){
		warning("dimension of given data is too high (", data_dim, "); used Isomap to reduce it to 2 dimensions")
		data = Isomap(data,2,min(10,data_points-1))
		data_dim = 2
	}

	## load rgl for three dimensional plots
	if(data_dim == 3){
        #	if(!require(rgl))
        #		stop("package rgl required for three dimensional plots")
		if(legend == TRUE)
			warning("legend not supported for three dimensional plots")
	}

	## check if given colours suffice the given number of classes
	if(length(col) < num_classes){
		warning("more classes than colours specified; using default colouring")
		col = defaultCols
		colour_names = defaultColNames
	}
	## check number of labels and number of classes within the labels (only up to six are supported for reasons of a good (coloured) visualization)
	if(length(labels) != data_points & labels[1] != "all"){
		warning("number of labels not consistent with the number of data points; labels were not respected")
		labels= "all"
		classes = "all"	
		num_classes = 1
	}
	else
	if(length(col) <=  6 & num_classes > 6){
		warning("Too many classes (", length(labels),") for distinct colouring; labels were not respected")
		labels = "all"
		classes = "all"	
		num_classes = 1
	}

	## check if given axes labels are correct	
	if(length(axesLabels) < data_dim){
		warning("number of axes labels smaller than the dimensionality of the data; using default labels")
		axesLabels = c("x","y","z")
	}

	## prepare colours for different classes
	plot_colours = rep("#000000",length(labels))
	for(i in 1:num_classes){
		plot_colours[which(labels == classes[i])] = col[i]
	}

	## plot data (2 or 3 dimensional, with or without text labels at each point)
	## two dimensional plot
	if(data_dim == 2){
		if(missing(text))
			plot(data,col=plot_colours,xlab=axesLabels[1],ylab=axesLabels[2], pch=pch)
		else{
			plot(data,type="n",xlab=axesLabels[1],ylab=axesLabels[2], ...)
			text(data,text,col=plot_colours, ...)
		}
		if(legend == TRUE & labels[1] != "all")
			legend("topright", legend=classes, col=col, pch=pch)
	}
	## three dimensional plot (using the library rgl)
	else
		if(missing(text))
			plot3d(data,type="s",size=1,col=plot_colours,xlab=axesLabels[1],ylab=axesLabels[2],zlab=axesLabels[3], ...)
		else{
			plot3d(data,type="n",xlab=axesLabels[1],ylab=axesLabels[2],zlab=axesLabels[3], ...)
			text3d(data,texts=text,color=plot_colours, ...)
		}

	## display a legend for the plot (just in console; 3D plot not capable of displaying a legend)
	if(data_dim == 3 | legend == FALSE){
		message("legend:")
		legend = data.frame(class=classes, colour=colour_names[1:num_classes])
		show(legend)
	}
}
