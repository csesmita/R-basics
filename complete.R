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

complete <- function(directory, id = 1:332) {
	i <- 1
	all_num <- c() 
	all_id <- c()
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
		complete <- complete.cases(this_data)
		num <- length(complete[complete == TRUE])
		all_num = c(all_num, num)
		all_id = c(all_id, id[[i]])
		i <- i + 1
	}
	data <- cbind("id" = all_id, "nobs" = all_num)
	as.data.frame(data)
}