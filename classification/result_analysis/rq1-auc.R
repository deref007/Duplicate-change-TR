library("ggplot2")
library(xtable)
general_root_path <- "C://Users//deref//Desktop//classification//result//"


root_path <- paste(general_root_path, "imbalance/", sep="")


classifiers <- c("random_forest", "logistic_regression", "naive_bayes")

#classifier_col <- c("RF", rep("", 5), "LR", rep("", 5), "NB", rep("", 5))
classifier_col <- c("RF", rep("", 7), "LR", rep("", 7), "NB", rep("", 7))

projects<-c("activemq","camel","derby","geronimo","hbase","hcommon","openjpa","pig")
projectnames<-c("ActiveMQ","Camel","Derby","Geronimo","HBase","Hadoop C.","Openjpa","Pig")
#projects <- c("ActiveMQ","activemq", "camel","hbase","hcommon","geronimo","pig")
#projectnames <- c("ActiveMQ", "Camel", "Derby", "Geronimo", "Hadoop C.", "HBase", "Mahout", "OpenJPA", "Pig", "Tuscany")
#projectnames<-c("activemq","camel","hbase","hcommon","Geronimo","Pig")

labels <- c("Old","New")
#labels <- c("In-dup","Un-dup")
#auc
measures <- c("MCC")

framenames <- c("Classifier", "Project", rep(c("In-dup", "Un-dup","ratio"), length(measures)-1), c("In-dup","Un-dup","ratio"))

result_matrix <- NULL


for (classifier in classifiers){
	for (p in projects){
		this_row <- c()
		for (measure in measures){
			for (label in labels){
				result_fn <- paste(c(root_path, p, "_", classifier, "_", label, ".csv"), collapse="")
				results <- read.csv(result_fn)
				results[is.na(results)] <- 0
 				m <- mean(results[measure][,1])
				m <- sprintf("%.2f", m + 0.0000000001)
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

#col1 <- paste(format(as.numeric(result_matrix[,1]) / as.numeric(result_matrix[,2]) * 100, digits=0), "%", sep="")
#col2 <- paste(format(as.numeric(result_matrix[,2]) / as.numeric(result_matrix[,2]) * 100, digits=0), "%", sep="")
# col3 <- paste(format(as.numeric(result_matrix[,3]) / as.numeric(result_matrix[,4]) * 100, digits=0), "%", sep="")
col1 <- paste(format((as.numeric(result_matrix[,2])-as.numeric(result_matrix[,1])) / as.numeric(result_matrix[,1]) * 100, digits=0), "%", sep="")
#result_frame <- data.frame(result_matrix[,1], col1, result_matrix[,2], col2, result_matrix[,3], col3, result_matrix[,4])
#result_frame<- data.frame(result_matrix[,1], col1, result_matrix[,2],col2)
result_frame <- data.frame(result_matrix[,1], result_matrix[,2],col1)
# result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)
#result_frame <- cbind(data.frame(classifier_col), result_frame)

result_frame <- cbind(data.frame(rep(projectnames,3)), result_frame)

result_frame <- cbind(data.frame(classifier_col), result_frame)
names(result_frame) <- framenames
print(result_frame)
print(xtable(result_frame), include.rownames=FALSE)

# result_path<-paste(c(general_root_path,"result_table/"))
# save_path <-paste(c(result_path, "rq3_imbalance-", measures, ".csv"), collapse="")
# write.csv(result_frame, save_path, row.names=FALSE)
#ggsave(paste(c(root_path, classifier, "_", compare_measure, ".pdf"), collapse=""), width=15, height=3)
