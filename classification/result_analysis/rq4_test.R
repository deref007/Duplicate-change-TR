# Title     : TODO
# Objective : TODO
# Created by: deref
# Created on: 2020/3/3

library("ggplot2")

#projects <- c("activemq", "camel", "geronimo", "hcommon", "hbase",  "pig")
projects<-c("hbase")
szz_labels <- c("buggy_AG")

classifier <- "naive_bayes"

compare_label <- "buggy_RA"

result_matrix <- matrix(c("projects", "color", "SZZ", "Rank", "Shifts"), nrow=1)

root_path <- "C://Users//deref//Desktop//classification//result//"
for (p in projects){
	importance_fn <- paste(c(root_path, "New_importance/", p, "_", classifier, "_importance.csv"), collapse="")
	importance_data <- read.csv(importance_fn)
	#print(importance_data)
}
szz_label<-"buggy_AG"
rank_n_features <- function(importance_data, szz_label, n){##特征排序
	szz_group <- paste(szz_label, "_groups", sep="")
	temp_data <- importance_data[which(importance_data[szz_group][,1] == n),]
    #print(temp_data)
	szz_feature <- paste(szz_label, "_features", sep="")
    #print(szz_feature)
	features <- temp_data[szz_feature][,1]
	return(features)
}
#print(rank_n_features(importance_data,szz_label,1))
corresponding_project <- function(project_name){##提取项目
	projectnames <- c("ActiveMQ", "Camel",  "Geronimo", "Hadoop C.", "HBase",  "Pig")

	pa <- projectnames[which(projects==project_name)]
    #print(pa)

	return(pa)
}
#print(corresponding_project("activemq"))

corresponding_label <- function(szz_label){  ##标签
	corresponding_labels <- c("AG")
	return(corresponding_labels[which(szz_labels==szz_label)])
}
#print(corresponding_label("buggy_AG"))
shift_n_features <- function(importance_data, szz_label1, szz_label2, n){
	fe_number <- nrow(importance_data)
	print(fe_number)
	szz_group1 <- paste(szz_label1, "_groups", sep="")#返回的是特征排名
	szz_group2 <- paste(szz_label2, "_groups", sep="")
	szz_feature1 <- paste(szz_label1, "_features", sep="")##返回的是特征
	szz_feature2 <- paste(szz_label2, "_features", sep="")
	rank_n_features1 <- rank_n_features(importance_data, szz_label1, n)
	rank_n_features2 <- rank_n_features(importance_data, szz_label2, n)
	print(rank_n_features2)
	shifts <- 0
	for (fe in rank_n_features1){
		temp_data <- importance_data[which(importance_data[szz_feature2][,1]== fe),]
		temp_rank <- temp_data[szz_group2][1,1]
		temp_rank[is.na(temp_rank)] <- 0
		shifts <- shifts + abs(temp_rank - n)
	}
	for (fe in rank_n_features2){
		temp_data <- importance_data[which(importance_data[szz_feature1][,1] == fe),]
		temp_rank <- temp_data[szz_group1][1,1]
        temp_rank[is.na(temp_rank)] <- 0
		if (temp_rank > 4){
			shifts <- shifts + abs(temp_rank - n)
			print(shifts)
         }
	}
	return(shifts / fe_number)
}
shifts1 <- shift_n_features(importance_data, szz_label, compare_label, 1)
print(shifts1)

# for (p in projects){
# 	importance_fn <- paste(c(root_path, "New_importance/", p, "_", classifier, "_importance.csv"), collapse="")
# 	importance_data <- read.csv(importance_fn)
# 	print(importance_data)
#}
#
# for (szz_label in szz_labels){
# 	if(szz_label != compare_label){
# 		shifts1 <- shift_n_features(importance_data, szz_label, compare_label, 1)
# 		shifts2 <- shift_n_features(importance_data, szz_label, compare_label, 2)
# 		shifts3 <- shift_n_features(importance_data, szz_label, compare_label, 3)
# 		project_name <- corresponding_project(p)
# 		szz <- corresponding_label(szz_label)
# 		temp_matrix <- matrix(c(project_name, I("red"), szz, "Rank1    ", shifts1), nrow=1)
# 		result_matrix <- rbind(result_matrix, temp_matrix)
# 		temp_matrix <- matrix(c(project_name, I("green"), szz, "Rank2    ", shifts2), nrow=1)
# 		result_matrix <- rbind(result_matrix, temp_matrix)
# 		temp_matrix <- matrix(c(project_name, I("blue"),szz, "Rank3    ", shifts3), nrow=1)
# 		result_matrix <- rbind(result_matrix, temp_matrix)
# 		print(result_matrix)
# 	}
# }
# result_frame <- data.frame(result_matrix[-1,])
# names(result_frame) <- c("projects", "Rank", "label", "SZZ_Rank", "Shifts")
# print(result_frame)
# result_frame$Shifts <- as.numeric(as.character(result_frame$Shifts))
# result_frame$label <- factor(result_frame$label, order=TRUE, levels=c("AG"))
# print(result_frame)
#
# tempv <- c()
#
# library("effsize")
#
# for (l in c("AG")){
# 	print(l)
# 	temp_result_frame <- result_frame[which(result_frame$label == l),]##选择标签为AG
# 	for (rank in unique(result_frame$SZZ_Rank)) {
# 		print(rank)
# 		print("mean_value:")
# 		print(mean(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts))
# 		print("median value:")
# 		print(median(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts))
# 		print(var(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts))
# 		temp_vec <- c(1:10) - c(1:10)
# 		print(wilcox.test(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts, temp_vec, alternative="g"))
# 		print(cliff.delta(temp_result_frame[which(temp_result_frame$SZZ_Rank==rank),]$Shifts, temp_vec))
# 	}
# }