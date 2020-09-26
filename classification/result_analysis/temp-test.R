projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")
projectnames <- c("ActiveMQ", "Camel", "Derby", "Geronimo", "Hadoop C.", "HBase", "Mahout", "OpenJPA", "Pig", "Tuscany")

library(effsize)
library(xtable)


szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA")

compare_label <- "buggy_RA"

result_matrix <- NULL

classifier <- "random_forest"

root_path <- "I://szz_investigation/results_imbalance/"

compare_measure <- "auc"

get_project_name<-function(project){
	projectnames <- c("ActiveMQ", "Camel", "Derby", "Geronimo", "Hadoop C.", "HBase", "Mahout", "OpenJPA", "Pig", "Tuscany")
	return(projectnames[which(projects==project)])
}

for (p in projects){
	print(p)
	fn <- paste(c(root_path, p, "_", classifier, "_", compare_label, ".csv"), collapse="")
	data <- read.csv(fn)

	this_row <- c()

	for (szz_label in szz_labels){
		temp_fn <- paste(c(root_path, p, "_", classifier, "_", szz_label, ".csv"), collapse="") 
		temp_data <- read.csv(temp_fn) 
		
		ret <- wilcox.test(data[compare_measure][,1], temp_data[compare_measure][,1], alternative="g", paired=TRUE)
		print(ret)
		p_value <- ret["p.value"][[1]]
		ret2 <- cliff.delta(temp_data[compare_measure][,1], data[compare_measure][,1])
		print(ret2)
		estimation <- ret2["estimate"][[1]]
		estimation <- round(estimation, 2)
		print(estimation)
		if (ret2["magnitude"] == 1){
			magnitude <- "N"
		}
		if (ret2["magnitude"] == 2){
			magnitude <- "S"
		}
		if (ret2["magnitude"] == 3){
			magnitude <- "M"
		}
		if (ret2["magnitude"] == 4){
			magnitude <- "L"
		}

		p_value_str <- ""
		adjusted_pvalue <- p_value * length(projects)
		if (adjusted_pvalue <= 0.001){
			p_value_str <- "***"
		}
		if (adjusted_pvalue <= 0.01 & adjusted_pvalue >0.001){
			p_value_str <- "**"
		}
		if(adjusted_pvalue <= 0.05 & adjusted_pvalue > 0.01){
			p_value_str <- "*"
		}

		
		this_row <- append(this_row, as.character(estimation))
		this_row <- append(this_row, paste(c("(", magnitude, ")", p_value_str), collapse=""))
	}
	if(is.null(result_matrix)){
		result_matrix <- matrix(this_row, nrow=1)
	}
	else{
		result_matrix <- rbind(result_matrix, matrix(this_row, nrow=1))
	}
}

result_frame <- data.frame(result_matrix)

xtable(result_frame)