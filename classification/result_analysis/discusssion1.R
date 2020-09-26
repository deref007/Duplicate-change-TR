
library(xtable)

projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")
projectnames <- c("ActiveMQ", "Camel", "Derby", "Geronimo", "Hadoop C.", "HBase", "Mahout", "OpenJPA", "Pig", "Tuscany")

szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA")
models <- c("Classifier", "Project", "B", "AG", "MA", "B", "AG", "MA", "B", "AG", "MA")

root_path <- "E://Study/szz_investigation/"

classifiers <- c("random_forest", "logistic_regression", "naive_bayes")

compare_szz_label <- "buggy_RA"

result_matrix <- NULL

# "auc", "brier", "precision", "recall", "f1measure", "precision20", "recall20", "f1measure20", "IFA", "PCI20"
compare_measure <- c("recall20")

folds <- 1000


pure_vector <- function(string){
	a <- c()
	for (i in 1:folds){
		a <- append(a, string)
	}
	return(a)
}

corresponding_project <- function(project_name){
	projectnames <- c("ActiveMQ", "Camel", "Derby", "Geronimo", "Hadoop C.", "HBase", "Mahout", "OpenJPA", "Pig", "Tuscany")

	pa <- projectnames[which(projects==project_name)]

	return(pa)
}

corresponding_label <- function(szz_label){
	corresponding_labels <- c("B", "AG", "MA")
	return(corresponding_labels[which(szz_labels==szz_label)])
}

for (classifier in classifiers){

	for (p in projects) {
		compare_result_fn <- paste(c(root_path, "result_balance/", p, "_", classifier, "_", compare_szz_label, ".csv"), collapse="")
		compare_result <- read.csv(compare_result_fn)
		temp <- c()
		for (cm in compare_measure){
			for (szz_label in szz_labels){
				print(p)
				print(cm)
				print(szz_label)
				result_fn <- paste(c(root_path, "result_balance/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")
				result <- read.csv(result_fn)
				ratio <- result[cm][,1] / compare_result[cm][,1]
				temp <- append(temp, median(ratio))
				print(median(ratio))
			}
		}
		
		if (is.null(result_matrix)){
			result_matrix <- matrix(temp, nrow=1)
		}
		else{
			result_matrix <- rbind(result_matrix, matrix(temp, nrow=1))
		}
	}
}

result_frame <- data.frame(result_matrix)
result_frame <- cbind(data.frame(rep(projectnames, 3)), result_frame)
classifier_col <- c("RF", rep("", 9), "LR", rep("", 9), "NB", rep("", 9))
result_frame <- cbind(data.frame(classifier_col), result_frame)
names(result_frame) <- models
print(xtable(result_frame), include.rownames=FALSE)