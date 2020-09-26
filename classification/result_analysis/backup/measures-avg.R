
projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")
projectnames <- c("ActiveMQ", "Camel", "Derby", "Geronimo", "Hadoop C.", "HBase", "Mahout", "OpenJPA", "Pig", "Tuscany")

szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA", "buggy_RA")
models <- c("Model B", "Model AG", "Modle MA", "Model RA")

root_path <- "C://Study/szz_investigation/"

classifier <- "random_forest"

result_matrix <- NULL

# "auc", "brier", "precision", "recall", "f1measure", "precision20", "recall20", "f1measure20", "IFA", "PCI20"
compare_measure <- "auc"

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


for (p in projects) {
	temp <- c()
	for (szz_label in szz_labels){
		result_fn <- paste(c(root_path, "results_imbalance/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")
		result <- read.csv(result_fn)
		temp <- append(temp, median(result[compare_measure][,1]))
	}
	
	if (is.null(result_matrix)){
		result_matrix <- matrix(temp, nrow=1)
	}
	else{
		result_matrix <- rbind(result_matrix, matrix(temp, nrow=1))
	}
}

result_frame <- data.frame(result_matrix)

row.names(result_frame) <- projects
names(result_frame) <- szz_labels
result_frame