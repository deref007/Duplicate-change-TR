
library(vcd)

projects <- c("derby", "openjpa")
projects<-c("derby","openjpa")
szz_labels <- c("buggy_AG", "buggy_RA")


root_path <- "C://Users//deref//Desktop//classification//data/"

result_fn <- paste(root_path, "_collinearity.txt", sep="")
cat("", file=result_fn)

for (p in projects){
	print(p)
	# p_szz <- paste(p, st, sep="-")
	# fn <- paste(p_szz, "csv", sep='.')

	fn <- paste(c(root_path, "raw_data/", p, ".csv"), collapse="")
	data <- read.csv(fn)
	# fn2 <- paste(c(root_path, "data_csvs2/", p, ".csv"), collapse = "")
	# raw_data <- read.csv(fn2)
	# data <- data[-which(raw_data$la+raw_data$ld > 10000 | raw_data$nf > 100), ]
	var_names <- names(data)
	var_names1 <- var_names[!var_names %in% c(szz_labels, "commit_id", "is_merge")]
	# correlations <- cor(data[var_names1], data[var_names1], method="spearman")
	correlations <- cor(data[var_names1], data[var_names1])
	cat(paste(p, "\n", sep=""), file=result_fn, append=TRUE)
	for (i in 1:(length(var_names1)-1)){
		for (j in (i+1):length(var_names1)){
			corr_val <- correlations[var_names1[i], var_names1[j]]
			if (corr_val > 0.8 | corr_val < -0.8){
				cat(paste(c(var_names1[i], var_names1[j], ":", as.character(corr_val), "\n"), collapse=" "), file=result_fn, append=TRUE)
			}
		}
	}

	temp_table <- xtabs(~fix+la, data[var_names1])
	print(chisq.test(temp_table))
	temp_table <- xtabs(~fix+ld, data[var_names1])
	print(chisq.test(temp_table))
}

