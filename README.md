# **The Impact of Duplicate Changes on Just-in-Time Defect Prediction**
The package contains our data and scripts that are needed to reproduce the results from the paper：“**The Impact of Duplicate Changes on Just-in-Time Defect Prediction**”

In this paper, the impact of duplicate changes on JIT defect prediction is explored. An empirical study on a total of 105,828 changes from eight Apache open-source projects is given. Based on our study, we find that 13% of changes from different branches are duplicate among the studied projects. The duplicate changes have a great influence on the model metrics for JIT defect prediction. For 50% of the changes, removing duplicate changes decreases the experience metrics with an average of 6-55. In addition, the duplicate changes have a significant impact on the evaluation and interpretation of JIT defect prediction models. Removing duplicate changes among the studied projects can significantly improve the performance of JIT defect prediction models ranging from 1% to 125% concerning various performance measures (i.e., AUC, MCC and F1). Given the impact of duplicate changes, we suggest that researchers should remove duplicate changes from the original historical changes of software repository when evaluating the performance of JIT defect prediction models in future work.

# **Dataset**
Our data is crawl from Apache and labeled using RA_SZZ.
* In the `projects` directory, we provide all the studied projects repositories of our paper. Input the following commands to retrieve all the repositories:

  ```
  git clone https://github.com/deref007/Duplicate-change-TR.git
  git submodule init
  git submodule update
  ```

* Repository for RA-SZZ: https://github.com/danielcalencar/raszzprime

* The directory dataset/ containing files: ``git_log.zip``, ``data_csv.zip``, ``duplicate_change.zip``, ``test_sample.zip``.

* The file ``git_log.zip`` contains the log message is crawled from apache projects

* The file ``data_csv.zip`` is calculated for all the apache projects using RA-SZZ. The change id can be retrieved from the each change.

* The change id of duplicate changes are stored in the file ``duplicate_change.zip``.

* The sample of duplicate change for manual analysis are stored in the file ``test_sample.zip``.

# **Model**
## **Requirement**
  The following tools were installed on the machine where the scripts were originally executed:
* Python 3.7
  In addition, several Python packages including: subprocess, sqlalchemy, pandas, numpy ,etc.
* R (>=3.6)
In addition, several R packages including: pROC, DMwR, ROSE, ggplot, rms, etc.
## **Run**
### **1. Identify duplicate changes**
* Run the script ``identify_duplicate/duplicate_detector/ detector.py``
### **2. Model building**  
* Running our code needs the path of the directory. Modify a line in the file ``classification/operations/classification_importance.R``
### **3. Data propocessing**
* Run the script ``model/calculation/classification_importance.R``

# **Result** 
* After running the above script. the performance scores (e.g., AUC, MCC, F1, Recall@20) in each of the 1,000 bootstrap iterations into csv files.



