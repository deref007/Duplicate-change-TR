library("pROC")
library("randomForest")
library("naivebayes")
library("reshape")
library("e1071")
library("ScottKnottESD")
library("caret")
library("pracma")
library("PRROC")
#文件地址指定存储此存储库代码的目录路径
# Specify the DIRECTORY path storing the code of this repository
DIR_PATH= "C:\\Users\\deref\\Desktop\\SZZ-TSE-master\\SZZ-TSE-master"




setwd(DIR_PATH)

source("code/packages/measures.R")
source("code/packages/imbalance.R")
source("code/packages/CBS.R")
source("code/packages/VarImportance.R")
source("code/packages/one-way.R")

# This path is used to store result data
root_path <- "data_results/"


collinearity_fn <- paste(root_path,"collinearity.csv", sep="")
collinearity_features <- read.csv(collinearity_fn)
szz_labels <- c("buggy_AG","buggy_RA")#2.10改
szz_baseline <- "buggy_RA"
# szz_labels <- c("bug")
# szz_baseline <- "bug"

projects <- c("pig","activemq")#2.17改项目名称
#projects <-c("activemq")
bootstrap_times <- 1000#重采样次数

classifiers <- c("logistic_regression","naive_bayes","random_forest")#分类器
#classifiers<-c("naive_bayes","random_forest")

study_methods <- c("imbalance")#不平衡数据，平衡数据，oneway方法

calculated_measures <- c("auc", "MCC")
calculated_measures2 <- c("auc", "MCC")
#存储计算结果的数据帧函数
store_result_to_frame<-function(result_frame, scores_vector){
	temp_frame <- data.frame(scores_vector)
	if (is.null(result_frame)){
		result_frame <- temp_frame
	}
	else {
		result_frame <- cbind(result_frame, temp_frame)
	}
	return(result_frame)
}

for (method in study_methods){
	for (classifier in classifiers){
		for (p in projects){
			# result preparation
			importance_rank_frame <- NULL

			filter_features <- as.vector(collinearity_features[p][,1])#转换为矩阵类型，
			filter_features <- append(filter_features, szz_labels)

			fn <- paste(c(root_path, "dup_data1/", p, ".csv"), collapse="")
			fn2 <- paste(c(root_path, "dup_data2/", p, ".csv"), collapse = "")
			fn3<-paste(c(root_path,"dup_data3/",p,".csv"),collapse="")#20改
			print(paste("filename: ", fn, sep=""))
			data <- read.csv(fn)
			raw_data <- read.csv(fn2)
			jit_data<-read.csv(fn3)#20改

			raw_data$lt <- raw_data$lt * raw_data$nf#lt 列 nf列·
			raw_data$nuc <- raw_data$nuc * raw_data$nf

			var_names <- names(data)#命名
			metrics <- var_names[!var_names %in% szz_labels]
			metrics <- metrics[!metrics %in% c("la", "ld", "commit_id")]#不包含上述的列

			var_names1 <- var_names[!var_names %in% filter_features]
			var_names_str <- paste(var_names1, collapse="+")
			print(var_names_str)
			print(szz_labels)

			for (szz_label in szz_labels){
				print(szz_label)
				result_frame <- NULL

				form <- as.formula(paste(szz_label, var_names_str, sep=" ~ "))
				var_names2 <- append(var_names1, szz_label)
				var_names2 <- append(var_names2, szz_baseline)

				temp_data <- data[var_names2]
				temp_data$real_la <- raw_data$la
				temp_data$real_ld <- raw_data$ld
                temdup_data<-jit_data[var_names2]#20改

				 #20改
		        temdup_data$real_la<-temdup_data$la
		        temdup_data$real_ld<-temdup_data$ld


				auc_scores <- c()
				precision_scores <- c()
				recall_scores <- c()
				F1_scores <- c()
				recall20_scores <- c()
				gmean_scores <- c()
				oneway_r20_scores <- c()
				MCC_scores<-c()#22改
				fp_scores <- c()
				fn_scores <- c()
				waste_lines_scores <- c()
				all_lines_scores <- c()
                importance_matrix <- NULL

				# factorise labels
				buggy_labels <- factor(temp_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
				temp_data[szz_label][,1] <- buggy_labels
				buggy_real_labels <- factor(temp_data[szz_baseline][,1], order=TRUE, levels=c("clean", "buggy"))
				temp_data[szz_baseline][,1] <- buggy_real_labels
				#20改
		        buggy_labels <- factor(temdup_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
				temdup_data[szz_label][,1] <- buggy_labels
				buggy_real_labels <- factor(temdup_data[szz_baseline][,1], order=TRUE, levels=c("clean", "buggy"))
				temdup_data[szz_baseline][,1] <- buggy_real_labels

				# start bootstrap runs
				for (i in 1:bootstrap_times){
					print(i)
					set.seed(i); train_indices<- sample(nrow(temp_data), replace=TRUE)#导入数据
					train_data <- temp_data[train_indices,]
					#test_data <- temp_data[-unique(train_indices),]#20改
					set.seed(i);train_indices2<-sample(nrow(temdup_data),replace=TRUE)
					test_data <- temdup_data[unique(train_indices2),]
                    #
					# if (method == "results_balance"){
					# 	# Undersampling
					# 	train_data <- undersampling(train_data, szz_label)
					# }
					#
					# if (method == "oneway"){
					# 	scores <- one_way(raw_data[train_indices,], raw_data[-unique(train_indices),], metrics, szz_label, szz_baseline, type="2")
					# 	oneway_r20_scores <- append(oneway_r20_scores, scores[2])
					# 	next
					# }

					# calculate the likelihood scores being "buggy" for changes in testing set
					if (classifier == "random_forest"){
						fit <- randomForest(form, train_data, ntree=100)
						prediction <- predict(fit, test_data, type="prob")
						prob <- prediction[,2]
					}

					if (classifier == "logistic_regression"){
						fit <- glm(form, train_data, family=binomial)
						prediction <- predict(fit, test_data, type="response")
						prob <- prediction
					}

					if (classifier == "naive_bayes"){
						fit <- naive_bayes(form, train_data)
						prediction <- predict(fit, test_data, type="prob")
						prob <- prediction[,2]
					}

					if (method == "imbalance"){
						# calculate variable importance
						importance_scores <- VarImportance(fit, classifier, var_names1, test_data)
						if (is.null(importance_matrix)){
							importance_matrix <- matrix(importance_scores, nrow=1)			}
						else{
							importance_matrix <- rbind(importance_matrix, matrix(importance_scores, nrow=1))
						}
					}
					#calculate Mcc##22改
					mcc_score<-MCC(test_data,prob,szz_baseline)
					mcc_scores<-append(MCC_scores,mcc_score)
					if (method == "imbalance"){
					# compute ranks of the features
					importance_frame <- data.frame(importance_matrix)
					names(importance_frame) <- var_names1
					row.names(importance_frame) <- as.character(1:bootstrap_times)
					sk <- sk_esd(importance_frame)
					features <- names(sk$groups)
					groups <- as.vector(sk$groups)
					temp_frame <- data.frame(features, groups)
					names(temp_frame) <- c(paste(szz_label, "_features", sep=""), paste(szz_label, "_groups", sep=""))

					if(is.null(importance_rank_frame)){
						importance_rank_frame <- temp_frame
					}
					else {
						importance_rank_frame <- cbind(importance_rank_frame, temp_frame)
					}
				}

				if (method == "imbalance" | method == "balance"){
					# store auc results
					result_frame <- store_result_to_frame(result_frame, auc_scores)

					# store precision
					# result_frame <- store_result_to_frame(result_frame, precision_scores)
                    #
					# # store recall
					# result_frame <- store_result_to_frame(result_frame, recall_scores)

					# # store F1
					# result_frame <- store_result_to_frame(result_frame, F1_scores)
                    #
					# # store gmean
					# result_frame <- store_result_to_frame(result_frame, gmean_scores)
                    #
					# # store recall20
					# result_frame <- store_result_to_frame(result_frame, recall20_scores)
					#storee mcc
					result_frame<-store_result_to_frame(result_frame,mcc_scores)#22改

					# # store false positive, false negative, waste effort and overall effort
					# result_frame <- store_result_to_frame(result_frame, fp_scores)
					# result_frame <- store_result_to_frame(result_frame, fn_scores)
					# result_frame <- store_result_to_frame(result_frame, waste_lines_scores)
					# result_frame <- store_result_to_frame(result_frame, all_lines_scores)
				}
				else{
					names(result_frame) <- calculated_measures2
					result_fn <- paste(c(root_path, "results_", method, "/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")

					write.csv(result_frame, result_fn, row.names=FALSE)
				}
			}

			if (method == "imbalance"){
				imp_fn <- paste(c(root_path, "importance/", p, "_", classifier, "_importance.csv"), collapse="")
				write.csv(importance_rank_frame, imp_fn, row.names=FALSE)
			}
		}
    }
}
}