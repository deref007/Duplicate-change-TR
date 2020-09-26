#导入运行代码必要的R语言包
library("pROC")
library("randomForest")
library("naivebayes")
library("reshape")
library("e1071")
library("ScottKnottESD")
library("caret")
library("pracma")
library("PRROC")
library('DMwR')
library('ROSE')
#文件地址指定存储此存储库代码的目录路径
# Specify the DIRECTORY path storing the code of this repository
DIR_PATH= "C:\\Users\\admin\\Desktop\\SZZ-TSE-master\\SZZ-TSE-master"




setwd(DIR_PATH)

source("code/packages/measures.R")
source("code/packages/imbalance.R")
source("code/packages/CBS.R")
source("code/packages/VarImportance.R")
source("code/packages/one-way.R")

# This path is used to store result data
root_path <- "0913data_results/"


collinearity_fn <- paste(root_path,"dup_collinearity.csv", sep="")


collinearity_features <- read.csv(collinearity_fn)
szz_labels <- c("buggy_AG",'buggy_RA')#2.10改
szz_baseline <- "buggy_RA"
# szz_labels <- c("bug")
# szz_baseline <- "bug"

projects <- c('pig')#2.17改项目名称
#projects <-c("activemq")
bootstrap_times <- 1000#重采样次数

classifiers <- c("logistic_regression")#分类器
#classifiers<-c("logistic_regression","naive_bayes","random_forest")

study_methods <- c("balance")#不平衡数据，平衡数据，oneway方法

calculated_measures <- c("auc",  "precision","recall","f1measure", "gmean", "recall20","MCC","fp", "fn", "waste_effort", "all_effort")
#calculated_measures2 <- c("fp", "fn", "waste_effort", "all_effort")
calculated_measures2 <- c("auc", "precision","recall","f1measure", "gmean", "recall20","MCC","fp", "fn", "waste_effort", "all_effort")
calculated_measures3 <- c("oneway_r20")


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
			fn3<-paste(c(root_path,"raw_dup_data3/",p,".csv"),collapse="")#20改
			print(paste("filename: ", fn, sep=""))
			data <- read.csv(fn)
			raw_data <- read.csv(fn2)
			jit_data<-read.csv(fn3)#20改

			raw_data$lt <- raw_data$lt * raw_data$nf#lt 列 nf列·
			raw_data$nuc <- raw_data$nuc * raw_data$nf

			var_names <- names(data)#命名
			metrics <- var_names[!var_names %in% szz_labels]
			metrics <- metrics[!metrics %in% c("la", "ld", "commit_id","commit_date")]#不包含上述的列

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
                # temdup_data<-jit_data[var_names2]#20改
				#
				#  #20改
		        # temdup_data$real_la<-temdup_data$la
		        # temdup_data$real_ld<-temdup_data$ld


				auc_scores <- c()
				precision_scores <- c()
				recall_scores <- c()
				F1_scores <- c()
				recall20_scores <- c()
				gmean_scores <- c()
				oneway_r20_scores <- c()
				mcc_scores <- c()#22改
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
		        # buggy_labels <- factor(temdup_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
				# temdup_data[szz_label][,1] <- buggy_labels
				# buggy_real_labels <- factor(temdup_data[szz_baseline][,1], order=TRUE, levels=c("clean", "buggy"))
				# temdup_data[szz_baseline][,1] <- buggy_real_labels

				# start bootstrap runs
				for (i in 1:bootstrap_times){
					print(i)
					set.seed(i); train_indices<- sample(nrow(temp_data), replace=TRUE)#导入数据
					train_data <- temp_data[train_indices,]
					test_data <- temp_data[-unique(train_indices),]#20改
					# set.seed(i);train_indices2<-sample(nrow(temdup_data),replace=TRUE)
					# test_data <- temdup_data[-unique(train_indices2),]
					#print(train_data)

					if (method == "balance"){
						# Undersampling
						#train_data <- undersampling(train_data, szz_label)
						#oversampli
						train_data <- oversampling(train_data, szz_label)
						#9.13加 SMOTE采样
						#library(DMwR)
						# 在R中，SMOTE算法是DMwR软件包的一部分，主要参数有如下三个：perc.over:过采样时，
						# 生成少数类的样本个数;k:过采样中使用K近邻算法生成少数类样本时的K值，默认是5；perc.under:欠采样时，
						# 对应每个生成的少数类样本，选择原始数据多数类样本的个数。例如，perc.over=500表示对原始数据集中的每个少数样本，
						# 都将生成5个新的少数样本；perc.under=80表示从原始数据集中选择的多数类的样本是新生的数据集中少数样本的80%
						# perc.over = xx 表示 少样本变成原来的（1+xx/100）倍
						#perc.under=yy 表示多样本变成少样本的 yy/100 *(xx/100)倍。
						##9.13 oversampling
						#cls<-temp_data[szz_label][,1]
						#train_data <- SMOTE(form,train_data)
						#train_data <- ovun.sample(form,train_data)

					}
					
					if (method == "oneway"){
						scores <- one_way(raw_data[train_indices,], raw_data[-unique(train_indices),], metrics, szz_label, szz_baseline, type="2")
						oneway_r20_scores <- append(oneway_r20_scores, scores[2])
						next
					}

					# calculate the likelihood scores being "buggy" for changes in testing set
					if (classifier == "random_forest"){
						fit <- randomForest(form, train_data, ntree=400)
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
					###改
					if (method == "imbalance"){
						# calculate variable importance
						importance_scores <- VarImportance(fit, classifier, var_names1, test_data)
						if (is.null(importance_matrix)){
							importance_matrix <- matrix(importance_scores, nrow=1)			}
						else{
							importance_matrix <- rbind(importance_matrix, matrix(importance_scores, nrow=1))
						}
					}
					###只计算MCC
					# calculate auc
					result <- roc(test_data[szz_baseline][,1], prob)
					auc_scores <- append(auc_scores, result["auc"][[1]][1])

					# calculate precision
					precision_score <- precision(test_data, prob, szz_baseline)
					precision_scores <- append(precision_scores, precision_score)

					# calculate recall
					recall_score <- recall(test_data, prob, szz_baseline)
					recall_scores <- append(recall_scores, recall_score)

					# calculate f1
					f_score <- F1(test_data, prob, szz_baseline)
					F1_scores <- append(F1_scores, f_score)

					# calculate gomeric mean
					gmean_score <- gmean(test_data, prob, szz_baseline)
					gmean_scores <- append(gmean_scores, gmean_score)
                    #calculate Mcc##22改
					mcc_score<-MCC(test_data,prob,szz_baseline)
					mcc_scores<-append(mcc_scores,mcc_score)
					# calculate cost effectiveness measure
					ordered_data <- get_ordered_data(test_data, prob,type="CBS")
					total_churn <- sum(test_data$real_la+test_data$real_ld)

					results <- calculate_cost_effectiveness2(ordered_data, total_churn, 0.2, "real_la", "real_ld", szz_baseline, "buggy")
					recall20 <- results[2]
					recall20_scores <- append(recall20_scores, recall20)

					# calculate wastes and misses
					waste_miss_results <- waste_miss(test_data, prob, szz_baseline)
					fp_scores <- append(fp_scores, waste_miss_results[1])
					fn_scores <- append(fn_scores, waste_miss_results[2])
					waste_lines_scores <- append(waste_lines_scores, waste_miss_results[3])
                    all_lines_scores <- append(all_lines_scores, waste_miss_results[4])
				  }
###gau###改
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
					result_frame <- store_result_to_frame(result_frame, precision_scores)

					# store recall
					result_frame <- store_result_to_frame(result_frame, recall_scores)

					# store F1
					result_frame <- store_result_to_frame(result_frame, F1_scores)

					# store gmean
					result_frame <- store_result_to_frame(result_frame, gmean_scores)

					#store recall20
					result_frame <- store_result_to_frame(result_frame, recall20_scores)
					#storee mcc
					result_frame <- store_result_to_frame(result_frame,   mcc_scores)#22改
					#
					# store false positive, false negative, waste effort and overall effort
					result_frame <- store_result_to_frame(result_frame, fp_scores)
					result_frame <- store_result_to_frame(result_frame, fn_scores)
					result_frame <- store_result_to_frame(result_frame, waste_lines_scores)
					result_frame <- store_result_to_frame(result_frame, all_lines_scores)
				}

				if (method == "oneway"){
					oneway_result_frame <- data.frame(oneway_r20_scores)
					names(oneway_result_frame) <- calculated_measures3
					result_fn2 <- paste(c(root_path, "oneway/", p, "_oneway_New", "_0913", szz_label, ".csv"), collapse="")
					write.csv(oneway_result_frame, result_fn2, row.names=FALSE)
				}
				else{
					names(result_frame) <- calculated_measures2
					result_fn <- paste(c(root_path, method, "/", "_",p, "_", classifier, "0920_New", szz_label, ".csv"), collapse="")

					write.csv(result_frame, result_fn, row.names=FALSE)
				}
			}
#gai

			if (method == "imbalance"){
				imp_fn <- paste(c(root_path, "importance/", p, "_0913_New", classifier, "_importance.csv"), collapse="")
				write.csv(importance_rank_frame, imp_fn, row.names=FALSE)
			}
		}
	}
}

