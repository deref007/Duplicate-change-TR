library(xtable)

# root_path <- "C://Users//deref//Desktop//classification//results_balance/"
#
# classifiers <- c("random_forest", "logistic_regression", "naive_bayes")
#
# classifier_col <- c("RF", rep("", 5), "LR", rep("", 5), "NB", rep("", 5))
#
# projects <- c("activemq", "camel", "geronimo", "hcommon", "hbase","pig")
# projectnames <- c("ActiveMQ", "Camel", "Geronimo", "Hadoop C.", "HBase", "Pig")
#
# szz_labels <- c( "buggy_AG", "buggy_RA")

root_path <- "C://Users//deref//Desktop//classification//result//balance/"

classifiers <- c("random_forest", "logistic_regression", "naive_bayes")

classifier_col <- c("RF", rep("", 7), "LR", rep("", 7), "NB", rep("", 7))

#projects <- c("activemq", "camel",  "hcommon", "geronimo","hbase","pig","derby","openjpa")
projects<-c("activemq","camel","derby","geronimo","hbase","hcommon","openjpa","pig")
projectnames<-c("ActiveMQ","Camel","Derby","Geronimo","HBase","Hadoop C.","Openjpa","Pig")
#projectnames <- c("ActiveMQ", "Camel", "Hadoop C.", "Geronimo","HBase",  "Pig","Derby","Openjpa")

labels <- c( "Old","New")
##labels<-c("Old","New")


# auc, f1measure, gmean recall20
measures <- c("recall20")

framenames <- c("Classifier", "Project", rep(c( "In-dup", "Un-dup","ratio"), length(measures)-1), c( "In-dup",  "Un-dup","ratio"))
print(framenames)

result_matrix <- NULL


for (classifier in classifiers){
	for (p in projects){
		this_row <- c()
		for (measure in measures){
			for (label in labels){
				result_fn <- paste(c(root_path, p,  "_", classifier,"_", label, ".csv"), collapse="")
				results <- read.csv(result_fn)
				results[is.na(results)] <- 0
 				m <- mean(results[measure][,1])#提取gmean列
				print(m)
				m <- sprintf("%.2f", m + 0.0000000001)#保留两位小数
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
print(result_matrix)
col1 <- paste(format((as.numeric(result_matrix[,2])-as.numeric(result_matrix[,1])) / as.numeric(result_matrix[,1])*100, digits=0), "%", sep="")
# col2 <- paste(format(as.numeric(result_matrix[,2]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")
# col3 <- paste(format(as.numeric(result_matrix[,3]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")

result_frame <- data.frame(result_matrix[,1], result_matrix[,2],col1)
#print(result_frame)

result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)

result_frame <- cbind(data.frame(classifier_col), result_frame)

names(result_frame) <- framenames
print(result_frame)
ge_root_path <- "C://Users//deref//Desktop//classification//result/result_table/"
save_path <-paste(c(ge_root_path, "rq3-balance-", measures, ".csv"), collapse="")
write.csv(result_frame, save_path, row.names=FALSE)

