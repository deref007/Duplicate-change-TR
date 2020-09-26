library("pROC")
library("randomForest")
library("reshape")
library("e1071")
library("ScottKnottESD")
library("caret")

source("C://Users/yuanruifan/bitbucket/change_defect_prediction2/classification/packages/measures.R")
source("C://Users/yuanruifan/bitbucket/change_defect_prediction2/classification/packages/imbalance.R")
source("C://Users/yuanruifan/bitbucket/change_defect_prediction2/classification/packages/CBS.R")

root_path <- "C://Study/szz_investigation/"
collinearity_fn <- paste(root_path, "collinearity.csv", sep="")
collinearity_features <- read.csv(collinearity_fn)
szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA", "buggy_RA")
szz_baseline <- "buggy_RA"

#"activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany"
projects <- c("activemq")

folds <- 10
classifier <- "logistic_regression"


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
	# result preparation
	result_frame <- NULL

	filter_features <- as.vector(collinearity_features[p][,1])
	filter_features <- append(filter_features, szz_labels)

	fn <- paste(c(root_path, "data_csvs/", p, ".csv"), collapse="")
	fn2 <- paste(c(root_path, "data_csvs2/", p, ".csv"), collapse = "")
	print(paste("filename: ", fn, sep=""))
	data <- read.csv(fn)
	raw_data <- read.csv(fn2)

	data <- data[-which(raw_data$la+raw_data$ld > 10000 | raw_data$nf > 100), ]
	raw_data <- raw_data[-which(raw_data$la+raw_data$ld > 10000 | raw_data$nf > 100), ]

	var_names <- names(data)

	var_names1 <- var_names[!var_names %in% filter_features]
	var_names_str <- paste(var_names1, collapse="+")
	print(var_names_str)
	print(szz_labels)

	for (szz_label in szz_labels){
		result_frame <- NULL

		form <- as.formula(paste(szz_label, var_names_str, sep=" ~ "))
		var_names2 <- append(var_names1, szz_label)
		var_names2 <- append(var_names2, szz_baseline)

		temp_data <- data[var_names2]
		temp_data$real_la <- raw_data$la
		temp_data$real_ld <- raw_data$ld
		
		# result preparation
		auc_scores <- c()
		brierscores <- c()
		precision_scores <- c()
		recall_scores <- c()
		F1_scores <- c()
		precision20_scores <- c()
		recall20_scores <- c()
		F1_20_scores <- c()
		IFA_scores <- c()
		inspected_changes <- c()
		importance_matrix <- NULL

		# factorise labels
		buggy_labels <- factor(temp_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
		temp_data[szz_label][,1] <- buggy_labels
		buggy_real_labels <- factor(temp_data[szz_baseline][,1], order=TRUE, levels=c("clean", "buggy"))
		temp_data[szz_baseline][,1] <- buggy_real_labels

		# start bootstrap runs
		for (i in 1:folds){
			print(i)
			data_num <- nrow(temp_data)
			train_start <- 1
			train_end <- floor(i/(folds+1)*data_num)
			test_start <- train_end + 1
			if (i < folds){
				test_end <- floor((i+1)/(folds+1)*data_num)
			}
			else{
				test_end <- data_num
			}
			train_data <- temp_data[train_start:train_end,]
			test_data <- temp_data[test_start:test_end,]

			# undersampling
			train_data <- undersampling(train_data, szz_label)

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
			
			# calculate auc
			result <- roc(test_data[szz_baseline][,1], prob)
			auc_scores <- append(auc_scores, result["auc"][[1]][1])

			# calculate brier scores
			mean_score <- BrierScore(test_data, prob, szz_baseline)
			brierscores <- append(brierscores, mean_score)

			# calculate precision
			precision_score <- precision(test_data, prob, szz_baseline)
			precision_scores <- append(precision_scores, precision_score)

			# calculate recall
			recall_score <- recall(test_data, prob, szz_baseline)
			recall_scores <- append(recall_scores, recall_score)

			# calculate f1
			f_score <- F1(test_data, prob, szz_baseline)
			F1_scores <- append(F1_scores, f_score)

			# calculate cost effectiveness measure
			ordered_data <- get_ordered_data(test_data, prob)
			raw_ce <- calculate_cost_effectiveness(test_data, ordered_data, szz_baseline)
			buggy_count <- raw_ce[1]
			change_count <- raw_ce[2]
			first_buggy <- raw_ce[3]

			precision20 <- buggy_count / change_count
			recall20 <- buggy_count / length(which(test_data[szz_baseline][,1]=="buggy"))
			F1_score20 <- 2 * precision20 * recall20 / (precision20 + recall20)
			precision20_scores <- append(precision20_scores, precision20)
			recall20_scores <- append(recall20_scores, recall20)
			F1_20_scores <- append(F1_20_scores, F1_score20)
			IFA_scores <- append(IFA_scores, first_buggy - 1)
			inspected_changes <- append(inspected_changes, change_count / nrow(test_data))
		}

		# store auc results
		print(mean(auc_results))
		result_frame <- store_result_to_frame(result_frame, auc_scores)

		# store brier scores
		print(mean(brierscores))
		result_frame <- store_result_to_frame(result_frame, brierscores)

		# store precision
		print(mean(precision_scores))
		result_frame <- store_result_to_frame(result_frame, precision_scores)

		# store recall
		print(mean(recall_scores))
		result_frame <- store_result_to_frame(result_frame, recall_scores)

		# store F1
		print(mean(F1_scores))
		result_frame <- store_result_to_frame(result_frame, F1_scores)

		# store precision20, recall20, F1_score20
		print(mean(precision20))
		print(mean(recall20))
		print(mean(F1_score20))
		result_frame <- store_result_to_frame(result_frame, precision20_scores)
		result_frame <- store_result_to_frame(result_frame, recall20_scores)
		result_frame <- store_result_to_frame(result_frame, F1_20_scores)
		result_frame <- store_result_to_frame(result_frame, IFA_scores)
		result_frame <- store_result_to_frame(result_frame, inspected_changes)

		names(result_frame) <- c("auc", "brier", "precision", "recall", "f1measure", "precision20", "recall20", "f1measure20", "IFA", "PCI20")

		result_fn <- paste(c(root_path, "longitudinal_results/", p, "_", szz_label, ".csv"), collapse="")
		write.csv(result_frame, result_fn, row.names=FALSE)
	}
}
