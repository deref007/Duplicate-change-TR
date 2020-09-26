
require(caret)
require(e1071)
require(Hmisc)
require(rms)

# "activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany"

projects <- c("activemq", "hcommon", "pig","camel","geronimo","hcommon")
projects<-c("openjpa","derby")
szz_labels <- c("buggy_AG", "buggy_RA")


root_path <- "C://Users//deref//Desktop//classification//data/"
result_fn <- paste(root_path, "drop_redun.txt", sep="")
cat("", file=result_fn)

collinearity_fn <- paste(root_path, "jit_collinearity.csv", sep="")
collinearity_features <- read.csv(collinearity_fn)

for (p in projects){
	fn <- paste(c(root_path, "raw_data/", p, ".csv"), collapse="")
	data <- read.csv(fn)
	#fn2 <- paste(c(root_path, "data_csvs2/", p, ".csv"), collapse = "")
	#raw_data <- read.csv(fn2)
	#data <- data[-which(raw_data$la+raw_data$ld > 10000 | raw_data$nf > 100), ]

	p_sub <- sub('-', '.', p)
	filter_features <- as.vector(collinearity_features[p_sub][,1])
	filter_features <- append(filter_features, szz_labels)
	filter_features <- append(filter_features, c("commit_id", "is_merge"))

	var_names <- names(data)
	var_names1 <- var_names[!var_names %in% filter_features]

	print(var_names1)

	filter_data <- data[var_names1]

	var_names_str <- paste(c(var_names1), collapse="+")
	redun_form <- as.formula(paste("~", var_names_str, sep=""))
	print(redun(redun_form, data=filter_data, nk=0))
}


