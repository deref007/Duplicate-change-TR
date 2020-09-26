# Title     : TODO
# Objective : TODO
# Created by: deref
# Created on: 2020/2/19
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

projects <- c("pig")#2.17改项目名称
#projects <-c("activemq")
bootstrap_times <- 1000#重采样次数

classifiers <- c("naive_bayes", "logistic_regression", "random_forest")#分类器
#classifiers<-c("naive_bayes","random_forest")

study_methods <- c("imbalance")#不平衡数据，平衡数据，oneway方法

calculated_measures <- c("auc", "precision", "recall", "f1measure", "gmean", "recall20", "fp", "fn", "waste_effort", "all_effort")
calculated_measures2 <- c("auc", "precision", "recall", "f1measure", "gmean", "recall20", "fp", "fn", "waste_effort", "all_effort")
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
for (p in projects){
    filter_features <- as.vector(collinearity_features[p][,1])#转换为矩阵类型，
    filter_features <- append(filter_features, szz_labels)

    fn <- paste(c(root_path, "dup_data1/", p, ".csv"), collapse="")
    fn2 <- paste(c(root_path, "dup_data2/", p, ".csv"), collapse = "")
	fn3<-paste(c(root_path,"dup_data3/",p,".csv"),collapse="")
    print(paste("filename: ", fn, sep=""))
    data <- read.csv(fn)
    raw_data <- read.csv(fn2)
	jit_data<-read.csv(fn3)
    raw_data$lt <- raw_data$lt * raw_data$nf#lt 列 nf列
    raw_data$nuc <- raw_data$nuc * raw_data$nf

    var_names <- names(data)#列名

    print(var_names)
	metrics <- var_names[!var_names %in% szz_labels]#度量
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

				temp_data <- data[var_names2]#读取csv内容
		        temdup_data<-jit_data[var_names2]
                #print(temp_data)
				temp_data$real_la <- raw_data$la
				temp_data$real_ld <- raw_data$ld
		        #10改
		        temdup_data$real_la<-temdup_data$la
		        temdup_data$real_ld<-temdup_data$ld

				auc_scores <- c()
				precision_scores <- c()
				recall_scores <- c()
				F1_scores <- c()
				recall20_scores <- c()
				gmean_scores <- c()
				oneway_r20_scores <- c()
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
                #10改
		        buggy_labels <- factor(temdup_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
				temdup_data[szz_label][,1] <- buggy_labels
				buggy_real_labels <- factor(temdup_data[szz_baseline][,1], order=TRUE, levels=c("clean", "buggy"))
				temdup_data[szz_baseline][,1] <- buggy_real_labels
				# start bootstrap runs
				for (i in 1:bootstrap_times){
					print(i)
					set.seed(i); train_indices <- sample(nrow(temp_data), replace=TRUE)#导入数据
					train_data <- temp_data[train_indices,]
					set.seed(i);train_indices<-sample(nrow(temdup_data),replace=TRUE)
					test_data <- temdup_data[-unique(train_indices),]

					if (method == "balance"){
						# Undersampling
						train_data <- undersampling(train_data, szz_label)
					}

					if (method == "oneway"){
						scores <- one_way(raw_data[train_indices,], raw_data[-unique(train_indices),], metrics, szz_label, szz_baseline, type="2")
						oneway_r20_scores <- append(oneway_r20_scores, scores[2])
						next
                    }


                }
	}
}