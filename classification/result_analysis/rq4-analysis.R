library(xtable)

projects <- c("activemq", "camel", "derby", "hcommon", "hbase", "openjpa","pig","geronimo")

szz_labels <- c("buggy_AG")

classifiers <- c("random_forest", "logistic_regression", "naive_bayes")

compare_label <- "buggy_RA"


root_path <- "C://Users//deref//Desktop//classification//result/"

studied_features <- c("ns", "nf", "la", "ld", "lt", "ndev", "age", "nuc", "exp", "sexp", "entropy")
feature_names <- c("NS", "NF", "LA", "LD", "LT", "NDEV", "AGE", "NUC", "EXP", "SEXP", "Entropy")

new_result_frame <- function(){
	temp <- 1:11 - 1:11
	result_frame <- data.frame(temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp)
	names(result_frame) <- as.character(1:12)
	return(result_frame)
}

rank_n_features <- function(importance_data, szz_label, n){
	szz_group <- paste(szz_label, "_groups", sep="")
	temp_data <- importance_data[which(importance_data[szz_group][,1] == n),]
	szz_feature <- paste(szz_label, "_features", sep="")
	features <- temp_data[szz_feature][,1]
	return(features)
}

result_frame <- new_result_frame()

for (k in 1:3){
	classifier <- classifiers[k]
	#for (i in 1:2){
	i=1
	szz_label <- szz_labels[i]
	framename <- as.character((k-1) * 4 + i)
	for (p in projects)
	{
		importance_fn <- paste(c(root_path, "New_importance/", p, "_", classifier, "_importance.csv"), collapse="")
		importance_data <- read.csv(importance_fn)
		f1 <- rank_n_features(importance_data, szz_label, 1)
		# f2 <- rank_n_features(importance_data, szz_label, 2)
		# f3 <- rank_n_features(importance_data, szz_label, 3)
		# features <- union(f1, union(f2, f3))
		features <- unique(f1)
		result_frame[framename][,1] <- result_frame[framename][,1] + as.numeric(studied_features %in% features)
	}
	#}
}

result_frame1 <- result_frame

result_frame <- new_result_frame()
# for (k in 1:3){
# 	classifier <- classifiers[k]
# 	for (i in 1:4){
# 		szz_label <- szz_labels[i]
# 		framename <- as.character((k-1) * 4 + i)
# 		for (p in projects)
# 		{
# 			importance_fn <- paste(c(root_path, "importance/", p, "_", classifier, "_importance.csv"), collapse="")
# 			importance_data <- read.csv(importance_fn)
# 			f1 <- rank_n_features(importance_data, szz_label, 1)
# 			f2 <- rank_n_features(importance_data, szz_label, 2)
# 			f3 <- rank_n_features(importance_data, szz_label, 3)
# 				features <- union(f1, union(f2, f3))
# 			result_frame[framename][,1] <- result_frame[framename][,1] + as.numeric(studied_features %in% features)
# 		}
# 	}
# }

result_frame2 <- rbind(result_frame1, result_frame)
result_frame3 <- cbind(data.frame(rep(feature_names, 2)), result_frame2)
print(result_frame3)

#print(xtable(result_frame3, digits=0), include.rownames=FALSE)


