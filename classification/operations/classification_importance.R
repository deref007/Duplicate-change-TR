library("pROC")
library("randomForest")
library("naivebayes")
library("reshape")
library("e1071")
library("ScottKnottESD")
library("caret")
library("pracma")
library("PRROC")

FILE_PATH = "C://Users/yuanruifan/bitbucket/jit_defect_prediction/classification/operations/"

setwd(FILE_PATH)

source("../packages/measures.R")
source("../packages/imbalance.R")
source("../packages/CBS.R")
source("../packages/VarImportance.R")
source("../packages/one-way.R")

root_path <- "E://Study/szz_investigation/"
collinearity_fn <- paste(root_path, "collinearity.csv", sep="")
collinearity_features <- read.csv(collinearity_fn)
szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA", "buggy_RA")
szz_baseline <- "buggy_RA"

#"activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany"
#projects <- c("activemq")
# projects <- c("activemq", "mahout", "openjpa", "pig")
#projects <- c("camel", "derby")
#projects <- c("hbase")
#projects <- c("geronimo", "tuscany")
projects <- c("hcommon")

# projects <- c("geronimo")

# projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")

bootstrap_times <- 1000
classifier <- "random_forest"
method <- "balance"

# "auc", "brier", "precision", "recall", "f1measure", "precision20", "recall20", "f1measure20", "IFA", "PCI20", "Popt", "oneway_p20", "oneway_r20", "oneway_f20", "oneway_popt"
#"auc", "auprc", "precision", "recall", "f1measure", "gmean",  "precision20", "recall20", "f1measure20", "Popt", "fp", "fn", "waste_effort"
#"auc", "auprc", "precision", "recall", "f1measure", "gmean", "precision20", "recall20", "f1measure20", "Popt", "fp", "fn", "waste_effort"
calculated_measures <- c("auc", "precision", "recall", "f1measure", "gmean", "recall20", "fp", "fn", "waste_effort", "all_effort")
calculated_measures2 <- c("auc", "precision", "recall", "f1measure", "gmean", "recall20", "fp", "fn", "waste_effort", "all_effort")
calculated_measures3 <- c("oneway_p20", "oneway_r20", "oneway_f20", "oneway_popt")

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
	importance_rank_frame <- NULL

	filter_features <- as.vector(collinearity_features[p][,1])
	filter_features <- append(filter_features, szz_labels)
	# filter_features <- append(filter_features, c("commit_id", "is_merge"))

	fn <- paste(c(root_path, "data_csvs/", p, ".csv"), collapse="")
	fn2 <- paste(c(root_path, "data_csvs2/", p, ".csv"), collapse = "")
	print(paste("filename: ", fn, sep=""))
	data <- read.csv(fn)
	raw_data <- read.csv(fn2)

	raw_data$lt <- raw_data$lt * raw_data$nf
	raw_data$nuc <- raw_data$nuc * raw_data$nf

	var_names <- names(data)
	metrics <- var_names[!var_names %in% szz_labels]
	metrics <- metrics[!metrics %in% c("la", "ld", "commit_id", "is_merge")]

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

		# result preparation
		auc_scores <- c()
		# auprc_scores <- c()
		# brierscores <- c()
		precision_scores <- c()
		recall_scores <- c()
		F1_scores <- c()
		precision20_scores <- c()
		recall20_scores <- c()
		F1_20_scores <- c()
		gmean_scores <- c()
		IFA_scores <- c()
		inspected_changes <- c()
		popt_values <- c()
		oneway_p20_scores <- c()
		oneway_r20_scores <- c()
		oneway_f20_scores <- c()
		oneway_popt_values <- c()
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

		# start bootstrap runs
		for (i in 1:bootstrap_times){
			print(i)
			set.seed(i); train_indices <- sample(nrow(temp_data), replace=TRUE)
			train_data <- temp_data[train_indices,]
			test_data <- temp_data[-unique(train_indices),]

			if (method == "balance"){
				# Undersampling
				train_data <- undersampling(train_data, szz_label)
			}
			
			if (method == "oneway"){
				scores <- one_way(raw_data[train_indices,], raw_data[-unique(train_indices),], metrics, szz_label, szz_baseline, type="2")
				oneway_p20_scores <- append(oneway_p20_scores, scores[1])
				oneway_r20_scores <- append(oneway_r20_scores, scores[2])
				oneway_f20_scores <- append(oneway_f20_scores, scores[3])
				oneway_popt_values <- append(oneway_popt_values, scores[4])
				next
			}		

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
				
			# calculate auc
			result <- roc(test_data[szz_baseline][,1], prob)
			auc_scores <- append(auc_scores, result["auc"][[1]][1])

			# calculate prroc
			# auprc <- pr.curve(prob, weights.class0=as.numeric(test_data[szz_baseline][,1]=="buggy"))$auc.integral
			# auprc_scores <- append(auprc_scores, auprc)

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

			# calculate cost effectiveness measure
			ordered_data <- get_ordered_data(test_data, prob)
			total_churn <- sum(test_data$real_la+test_data$real_ld)
			
			results <- calculate_cost_effectiveness2(ordered_data, total_churn, 0.2, "real_la", "real_ld", szz_baseline, "buggy")
			precision20 <- results[1]
			recall20 <- results[2]
			F1_score20 <- 2 * precision20 * recall20 / (precision20 + recall20)
			precision20_scores <- append(precision20_scores, precision20)
			recall20_scores <- append(recall20_scores, recall20)
			F1_20_scores <- append(F1_20_scores, F1_score20)
			
			if ("IFA" %in% calculated_measures){
				IFA_scores <- append(IFA_scores, first_buggy - 1)
				inspected_changes <- append(inspected_changes, change_count / nrow(test_data))
			}

			# if ("Popt" %in% calculated_measures){
				# popt_value <- Popt2(ordered_data, total_churn, "real_la", "real_ld", szz_baseline, "buggy")
				# popt_values <- append(popt_values, popt_value)
			# }
			
			# calculate wastes and misses
			waste_miss_results <- waste_miss(test_data, prob, szz_baseline)
			fp_scores <- append(fp_scores, waste_miss_results[1])
			fn_scores <- append(fn_scores, waste_miss_results[2])
			waste_lines_scores <- append(waste_lines_scores, waste_miss_results[3])			
			all_lines_scores <- append(all_lines_scores, waste_miss_results[4])
		}

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
			print(mean(auc_scores))
			result_frame <- store_result_to_frame(result_frame, auc_scores)

			# store auprc results
			# print(mean(auprc_scores))
			# result_frame <- store_result_to_frame(result_frame, auprc_scores)

			# store precision
			print(mean(precision_scores))
			result_frame <- store_result_to_frame(result_frame, precision_scores)

			# store recall
			print(mean(recall_scores))
			result_frame <- store_result_to_frame(result_frame, recall_scores)

			# store F1
			print(mean(F1_scores))
			result_frame <- store_result_to_frame(result_frame, F1_scores)

			# store gmean
			print(mean(gmean_scores))
			result_frame <- store_result_to_frame(result_frame, gmean_scores)

			# store precision20, recall20, F1_score20
			print(mean(precision20))
			print(mean(recall20))
			print(mean(F1_score20))
			#result_frame <- store_result_to_frame(result_frame, precision20_scores)
			result_frame <- store_result_to_frame(result_frame, recall20_scores)
			#result_frame <- store_result_to_frame(result_frame, F1_20_scores)
			#result_frame <- store_result_to_frame(result_frame, popt_values)
			
			# store waste and misses
			result_frame <- store_result_to_frame(result_frame, fp_scores)
			result_frame <- store_result_to_frame(result_frame, fn_scores)
			result_frame <- store_result_to_frame(result_frame, waste_lines_scores)
			result_frame <- store_result_to_frame(result_frame, all_lines_scores)
		}

		if (method == "oneway"){
			oneway_result_frame <- data.frame(oneway_p20_scores, oneway_r20_scores, oneway_f20_scores, oneway_popt_values)
			result_fn2 <- paste(c(root_path, "oneway/", p, "_oneway", "_", szz_label, ".csv"), collapse="")
			# write.csv(oneway_result_frame, result_fn2, row.names=FALSE)
		}
		else{
			names(result_frame) <- calculated_measures2
			# print(szz_label)
			result_fn <- paste(c(root_path, "results_", method, "/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")
			write.csv(result_frame, result_fn, row.names=FALSE)
		}
	}

	if (method == "imbalance"){
		imp_fn <- paste(c(root_path, "importance/", p, "_", classifier, "_importance.csv"), collapse="")
		write.csv(importance_rank_frame, imp_fn, row.names=FALSE)
	}
}
