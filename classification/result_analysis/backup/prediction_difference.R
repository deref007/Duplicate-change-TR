library("pROC")
library("randomForest")
library("naivebayes")
library("reshape")
library("e1071")
library("ScottKnottESD")
library("caret")
library("pracma")

source("C://Users/yuanruifan/bitbucket/change_defect_prediction2/classification/packages/measures.R")
source("C://Users/yuanruifan/bitbucket/change_defect_prediction2/classification/packages/imbalance.R")
source("C://Users/yuanruifan/bitbucket/change_defect_prediction2/classification/packages/CBS.R")
source("C://Users/yuanruifan/bitbucket/change_defect_prediction2/classification/packages/VarImportance.R")

root_path <- "C://Study/szz_investigation/"
collinearity_fn <- paste(root_path, "collinearity.csv", sep="")
collinearity_features <- read.csv(collinearity_fn)
szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA", "buggy_RA")
szz_baseline <- "buggy_RA"

#"activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany"
projects <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")

bootstrap_times <- 5

classifier <- "random_forest"


for (p in projects){
	# result preparation
	importance_rank_frame <- NULL

	filter_features <- as.vector(collinearity_features[p][,1])
	filter_features <- append(filter_features, szz_labels)
	filter_features <- append(filter_features, "commit_id")

	fn <- paste(c(root_path, "data_csvs3/", p, ".csv"), collapse="")
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
		# factorise labels
		buggy_labels <- factor(data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
		data[szz_label][,1] <- buggy_labels
	}

	temp_data <- data
	# start bootstrap runs
	for (i in 1:bootstrap_times){
		print(i)

		set.seed(i); train_indices <- sample(nrow(temp_data), replace=TRUE)
		train_data <- temp_data[train_indices,]
		test_data <- temp_data[-unique(train_indices),]
		

		for (szz_label in szz_labels){
			if(szz_label != "buggy_RA"){
				print(szz_label)
				result_frame <- NULL
				form <- as.formula(paste(szz_label, var_names_str, sep=" ~ "))
				form2 <- as.formula(paste(szz_baseline, var_names_str, sep=" ~ "))

				#undersampling
				# train_data <- undersampling(train_data, szz_label)

				# calculate the likelihood scores being "buggy" for changes in testing set
				fit <- randomForest(form, train_data, ntree=100)
				fit2 <- randomForest(form2, train_data, ntree=100)
				prediction <- predict(fit, test_data, type="prob")
				prob <- prediction[,2]
				prediction <- predict(fit2, test_data, type="prob")
				prob2 <- prediction[,2]

				changes <- order(abs(prob2-prob), decreasing=TRUE)[1:100]

				test_data$rf_prob1 <- prob
				test_data$rf_prob2 <- prob2

				fit <- glm(form, train_data, family=binomial)
				fit2 <- glm(form2, train_data, family=binomial)
				prediction <- predict(fit, test_data, type="response")
				prob <- prediction
				prediction <- predict(fit2, test_data, type="response")
				prob2 <- prediction

				changes2 <- order(abs(prob2-prob), decreasing=TRUE)[1:100]

				test_data$lg_prob1 <- prob
				test_data$lg_prob2 <- prob2

				fit <- naive_bayes(form, train_data)
				fit2 <- naive_bayes(form2, train_data)
				prediction <- predict(fit, test_data, type="prob")
				prob <- prediction[,2]
				prediction <- predict(fit2, test_data, type="prob")
				prob2 <- prediction[,2]

				changes3 <- order(abs(prob2-prob), decreasing=TRUE)[1:100]

				test_data$nb_prob1 <- prob
				test_data$nb_prob2 <- prob2

				interset <- intersect(changes, intersect(changes2, changes3))

				if (length(interset) != 0){
					print(test_data[interset, ])
				}

			}
			
		}
		
	}
	
}


