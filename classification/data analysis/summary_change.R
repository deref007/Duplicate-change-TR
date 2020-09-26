projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")
szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA")

root_path <- "E://Study/szz_investigation/"

change_num <- 0

result_matrix <- NULL

compare_label <- "buggy_RA"

for (p in projects){
	data <- read.csv(paste(c(root_path, "data_csvs/", p, ".csv"), collapse=""))
	# fn2 <- paste(c(root_path, "data_csvs2/", p, ".csv"), collapse = "")
	# raw_data <- read.csv(fn2)
	# data <- data[-which(raw_data$la+raw_data$ld > 10000 | raw_data$nf > 100), ]
	result <- c()

	real_buggy <- which(data[compare_label][,1] == "buggy")
	real_buggy_num <- length(real_buggy)
	real_clean <- which(data[compare_label][,1] == "clean")
	real_clean_num <- length(real_clean)

	for (szz_label in szz_labels){
		indices <- which(data[szz_label][,1] == "buggy")

		num <- length(indices)
		fp <- which(data[szz_label][,1]=="buggy" & data[compare_label][,1]=="clean")
		fp_num <- length(fp)
		fn <- which(data[szz_label][,1]=="clean" & data[compare_label][,1]=="buggy")
		fn_num <- length(fn)
		#options(digits=2)
		
		this_buggy_num <- length(which(data[,szz_label] == "buggy"))

		buggy_rate <- num / nrow(data)

		fpr <- round(fp_num / real_clean_num * 100)
		print(fpr)
		fnr <- round(fn_num/real_buggy_num * 100)
		print(fnr)
		buggy_rate <- round(buggy_rate * 100)
		print(buggy_rate)
		
		mislabeled_num <- fp_num + fn_num
		mislabel_rate <- round(mislabeled_num / nrow(data) * 100)
		print(mislabel_rate)
		
		fpr_str <- paste(as.character(fpr), "%", sep="")
		fnr_str <- paste(as.character(fnr), "%", sep="")
		mislabel_str <- paste(as.character(mislabel_rate), "%", sep="")
		buggy_rate_str <- paste(c("(", as.character(buggy_rate), "%", ")"), collapse="")
		#result <- append(result, c(num, buggy_rate, fp_num, fpr_str, fn_num, fnr_str, mislabeled_num, mislabel_str, fp_num / this_buggy_num))
		result<- append(result, c(mislabeled_num, fp_num, fn_num))
	}

	buggy_rate <- real_buggy_num/ nrow(data)
	buggy_rate <- round(buggy_rate * 100)
	print(buggy_rate)
	buggy_rate_str <- paste(c("(", as.character(buggy_rate), "%", ")"), collapse="")
	result <- append(result, c(real_buggy_num, buggy_rate_str))

	if (is.null(result_matrix)) {
		result_matrix <- matrix(result, nrow=1)
	}
	else {
		result_matrix <- rbind(result_matrix, matrix(result, nrow=1))
	}
}

result_frame <- data.frame(result_matrix)
row.names(result_frame) <- projects

xtable(result_frame)