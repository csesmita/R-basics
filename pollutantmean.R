prepend_chars <- function(mystr, append_len) {
	j <- 0
	concat <- ""
	while (j < append_len) {
		concat <- paste0("0", concat, sep='', collapse='')
		j <- j + 1
	}
      paste0(concat,mystr, sep='', collapse='')
}

construct_filename <- function(directory, filename) {
	paste(directory, filename, sep='/', collapse='')
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
	i <- 1
	data <- c() 
      while(i <= length(id)) {
            nchars = nchar(as.character(id[[i]]))
		if (nchars < 3) {
			name <- prepend_chars(as.character(id[[i]]), 3-nchars)
		} else {
			name = as.character(id[[i]])
		}
		name <- paste0(name, ".csv", sep='', collapse='')
		name <- construct_filename(directory, name)
		
		this_data <- read.csv(name)
		
		# Only take this pollutant
		this_data <- this_data[[pollutant]]
		this_data <- this_data[!is.na(this_data)]
		
		#Append the data to the existing data
		data <- c(data, this_data)
		i <- i + 1
	}
	# Obtain the mean
	
	data <- mean(data)
	round(data,3)
}