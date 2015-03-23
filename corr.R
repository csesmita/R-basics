corr <- function(directory, threshold = 0) {
	data <- complete(directory)
	ids_thresh <- c()
	i <- 1
	while( i<= nrow(data)) {
		if (data[[i, "nobs"]] > threshold) {
			ids_thresh <- c(ids_thresh, data[[i, "id"]])		
		}
		i <- i + 1
	}
	corr_data <- c()
	
	if (length(ids_thresh) > 0) {
		i<- 1
		while( i <= length(ids_thresh)) {
	      	nchars = nchar(as.character(ids_thresh[[i]]))
			if (nchars < 3) {
				name <- prepend_chars(as.character(ids_thresh[[i]]), 3-nchars)
			} else {
				name = as.character(ids_thresh[[i]])
			}
			name <- paste0(name, ".csv", sep='', collapse='')
			name <- construct_filename(directory, name)
		
			this_data <- read.csv(name)
			this_data <- this_data[complete.cases(this_data),]

			# Find out the correlation
			this_cor <- cor(this_data["nitrate"], this_data["sulfate"])
			this_cor <- round(this_cor,5)
			corr_data <- c(corr_data, this_cor)
			i <- i + 1	
		} 
	} else {
		corr_data <- as.numeric(corr_data)
	}
	corr_data
}