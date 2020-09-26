
root_path <- "C://Users//deref//Desktop//classification//result//balance/"

classifiers <- c("random_forest", "logistic_regression", "naive_bayes")

classifier_col <- c("RF", rep("", 4), "LR", rep("", 4), "NB", rep("", 4))

projects <- c("activemq", "camel",  "hcommon", "hbase","pig")
projectnames <- c("ActiveMQ", "Camel", "Hadoop C.", "HBase",  "Pig")

szz_labels <- c( "Old","New")

measures <- c("fp", "fn", "waste_effort", "all_effort")

framenames <- c("Classifier", "Project", rep(c( "Old", "New"), length(measures)-1), c( "Old",  "New"))

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
				m <- sprintf("%.0f", m + 0.0000000001)
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

result_frame <- data.frame(result_matrix)
result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)
result_frame <- cbind(data.frame(classifier_col), result_frame)
names(result_frame) <- framenames
print(result_frame)
save_path <-paste(c(root_path, "rq3-balance-", measures, ".csv"), collapse="")
write.csv(result_frame, save_path, row.names=FALSE)





