library(irr)

#"activemq", "camel", "derby", "geronimo", "hadoop-common", "hbase", "mahout", "openjpa", "pig", "tuscany"
projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")
#"buggy_B1", "buggy_B2", "buggy_AG", "buggy_MA", "buggy_RA", "buggy_RA_Filter"
szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA")

compare_label <- "buggy_RA"

total <- 0

result_matrix <- NULL

root_path <- "C://Study/szz_investigation/"

for (p in projects){
	print(p)
	fn <- paste(c(root_path, "data_csvs/", p, ".csv"), collapse="")
	fn2 <- paste(c(root_path, "data_csvs2/", p, ".csv"), collapse = "")
	print(paste("filename: ", fn, sep=""))
	data <- read.csv(fn)
	raw_data <- read.csv(fn2)

	data <- data[-which(raw_data$la+raw_data$ld > 10000 | raw_data$nf > 100), ]
	raw_data <- raw_data[-which(raw_data$la+raw_data$ld > 10000 | raw_data$nf > 100), ]

	changes <- nrow(data)
	print(changes)

	kappas <- c()

	for (szz_label in szz_labels){
		print(szz_label)
		if (szz_label != compare_label){
			x <- data[szz_label][,1]
			y <- data[compare_label][,1]
			temp_data <- data.frame(x,y)
			ck <- kappa2(temp_data)
			kappas <- append(kappas, ck$value)
		}
	}
	
	if (is.null(result_matrix)){
		result_matrix <- matrix(kappas, nrow=1)
	}
	else {
		result_matrix <- rbind(result_matrix, matrix(kappas, nrow=1))
	}

	total <- changes + total
}

result_frame <- data.frame(result_matrix)
names(result_frame) <- szz_labels
row.names(result_frame) <- projects
write.csv(result_frame, paste(c(root_path, "kappa.csv"), collapse=""))

print(total)