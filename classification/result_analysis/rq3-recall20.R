library(xtable)

root_path <- "C://Users//deref//Desktop//classification//result//balance/"

classifiers <- c("random_forest", "logistic_regression", "naive_bayes")

classifier_col <- c("RF", rep("", 5), "LR", rep("", 5), "NB", rep("", 5))

projects <- c("activemq", "camel", "geronimo", "hcommon", "hbase",  "pig")
projectnames <- c("ActiveMQ", "Camel", "Geronimo", "Hadoop C.", "HBase",  "Pig")

szz_labels <- c("Old","New")

measures <- c("recall20")

#framenames <- c("Classifier", "Project", rep(c("B", "AG", "MA", "RA"), length(measures)-1), c("B", "AG", "MA", "RA"))
framenames <- c("Classifier", "Project", rep(c( "Old", "New","ratio"), length(measures)-1), c( "Old",  "New","ratio"))
result_matrix <- NULL


for (classifier in classifiers){
	for (p in projects){
		this_row <- c()
		for (measure in measures){
			for (szz_label in szz_labels){
				result_fn <- paste(c(root_path, p, "_", classifier, "_", szz_label, ".csv"), collapse="")
				results <- read.csv(result_fn)
				results[is.na(results)] <- 0
 				m <- mean(results[measure][,1])
				this_row <- append(this_row, m)
			}
		}


		if (is.null(result_matrix)){
			result_matrix <- matrix(this_row, nrow=1)
		}
		else{
			result_matrix <- rbind(result_matrix, matrix(this_row, nrow=1))
		}
	}
}

# result_frame <- data.frame(result_matrix)
# result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)
# result_frame <- cbind(data.frame(classifier_col), result_frame)
# names(result_frame) <- framenames

# latex_table <- xtable(result_frame)
# print(latex_table, include.rownames=FALSE)


# col1 <- paste(format(as.numeric(result_matrix[,1]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")
# col2 <- paste(format(as.numeric(result_matrix[,2]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")
# col3 <- paste(format(as.numeric(result_matrix[,3]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")
col1 <- paste(format((as.numeric(result_matrix[,2])-as.numeric(result_matrix[,1])) / as.numeric(result_matrix[,1])*100,digits=0), "%", sep="")
#result_frame <- data.frame(result_matrix[,1], col1, result_matrix[,2], col2, result_matrix[,3], col3, result_matrix[,4])
result_frame <- data.frame(result_matrix[,1], result_matrix[,2],col1)
result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)

result_frame <- cbind(data.frame(classifier_col), result_frame)

names(result_frame) <- framenames
print(result_frame)
# latex_table <- xtable(result_frame)
# print(latex_table, include.rownames=FALSE)


