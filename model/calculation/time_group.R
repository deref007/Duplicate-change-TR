file<-'C:\\Users\\deref\\Desktop\\SZZ-TSE-master\\SZZ-TSE-master\\data_results\\dup_data1\\pig.csv'
data <- read.csv(file=file, header=TRUE, as.is=TRUE)##读取数据

    #cat("(BEG)Timewise cross-validation for", project, "\n")#名字

data$commit_date <- strptime(data$commit_date, format="%Y/%m/%d") #### data$timePOSIX  <- as.POSIXct(data$commitdate, format="%Y/%m/%d %H:%M") ### 格式化时间
data$commit_date <- strftime(data$commit_date, format="%Y/%m")
data <- data[order(data$commit_date), ] ### 对change按提交时间进行排序
#print(data)

unimon <- unique(data$commit_date)
unimon <- unimon[order(unimon)]#排序去重

totalFolds <- length(unimon)
print(totalFolds)
sub <- NULL ### dive data into totalFolds parts, each part corresponding to changes within one month
for (fold in seq( totalFolds)) {
    print(fold)
    sub[[fold]] <- data[which(data$commit_date==unimon[fold]), ]#切分月数
    }

#gap<-as.double(sprintf("%.1f", (length(unimon)-1)/5))
gap<-2
#print(gap)


theList <- NULL
train.valid <- NULL
list.index <- 1
for (fold in seq(totalFolds)) {
    print(fold)
    if (fold+5> totalFolds) { next }
    if(fold%in%c(6,11,31,32,33,34,35,36,37,38,39,40)){next}
    fit <- rbind(sub[[fold]], sub[[fold+1]])#训练集
    if(fold+%in%c(6,11,31,32,33,34,35,36,37,38,39,40)){next}
    est <- rbind(sub[[fold+2+gap]], sub[[fold+3+gap]])#预测集

    #print(fit[2])
    print(est[1])
    #theList[[list.index]] <- coreExperiment(fit=fit, est=est, sampling=sampling, seed=fold)#训练

    list.index <- list.index + 1
    train.valid <- rbind(train.valid, c(fold, fold+gap))##rbind函数合并数据数据帧
    print(train.valid)
}
######
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


collinearity_fn <- paste(root_path,"dup_collinearity.csv", sep="")


collinearity_features <- read.csv(collinearity_fn)
szz_labels <- c("buggy_AG","buggy_RA")#2.10改
szz_baseline <- "buggy_RA"
# szz_labels <- c("bug")
# szz_baseline <- "bug"

projects <- c("hbase")#2.17改项目名称
#projects <-c("activemq")
bootstrap_times <- 1000#重采样次数

classifiers <- c("random_forest")#分类器
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

			filter_features <- as.vector(collinearity_features[p][,1])#转换为矩阵类型，应丢掉的特征
			filter_features <- append(filter_features, szz_labels)###过滤特征

			fn <- paste(c(root_path, "dup_data1/", p, ".csv"), collapse="")
			fn2 <- paste(c(root_path, "dup_data2/", p, ".csv"), collapse = "")

			print(paste("filename: ", fn, sep=""))
			data <- read.csv(fn)
			raw_data <- read.csv(fn2)

			####time_3.9改
			data$commit_date <- strptime(data$commit_date, format="%Y/%m/%d") #### data$timePOSIX  <- as.POSIXct(data$commitdate, format="%Y/%m/%d %H:%M") ### 格式化时间
			data$commit_date <- strftime(data$commit_date, format="%Y/%m")
			data <- data[order(data$commit_date), ] ### 对change按提交时间进行排序
			#print(data)

			unimon <- unique(data$commit_date)
			unimon <- unimon[order(unimon)]#排序去重


			totalFolds <- length(unimon)
			print(totalFolds)
			data$commit_date<-NULL
            #####
			raw_data$lt <- raw_data$lt * raw_data$nf#lt 列 nf列·
			raw_data$nuc <- raw_data$nuc * raw_data$nf

			var_names <- names(data)#命名
			metrics <- var_names[!var_names %in% szz_labels]
			metrics <- metrics[!metrics %in% c("commit_date")]#不包含上述的列

			var_names1 <- var_names[!var_names %in% filter_features]
			var_names_str <- paste(var_names1, collapse="+")
            print(typeof(var_names1))
			print(var_names_str)
			print(szz_labels)

            for (szz_label in szz_labels){
				print(szz_label)
				result_frame <- NULL

				form <- as.formula(paste(szz_label, var_names_str, sep=" ~ "))## 连接公式函数，~的左侧是因变量，右侧是自变量。
				var_names2 <- append(var_names1, szz_label)
				var_names2 <- append(var_names2, szz_baseline)##append合并向量
                #print(form)
                print(var_names2)
                temp_data <- data[var_names2]##训练数据
				temp_data$real_la <- raw_data$la
				temp_data$real_ld <- raw_data$ld
                #print(temp_data)

            }
        }
    }
}
