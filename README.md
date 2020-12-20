# The Impact of Duplicate Changes on Just-in-Time Defect Prediction
The package contains our data and scripts that are needed to reproduce the results from the paper: "**The Impact of Duplicate Changes on Just-in-Time Defect Prediction**". 
# **Dataset**
Our data is crawl from Apache and labeled using RA_SZZ.
* Repository for apache projects:https://www.apache.org/
* Repository for RA-SZZ: https://github.com/danielcalencar/raszzprime
The directory dataset/ containing files: ``git_log.zip``, ``data_csv.zip``, ``duplicate_change.zip``, ``Test_sample.zip``.
* The file ``git_log.zip`` contains the log message is crawled from apache projects
* The file ``data_csv.zip`` is calculated for all the apache projects using RA-SZZ. The CSV id can be retrieved from the each change.
* The CSV id of duplicate changes are stored in the file ``duplicate_change.zip``.
* The sample of duplicate change for manual analysis are stored in the file ``Test_sample.zip``.

# **Model**
## **Requirement**
The following tools were installed on the machine where the scripts were originally executed:
* Python 3.7
* R (>=3.6)
## **Run**
### **1. Identify duplicate changes**
* Run the script ``identify_duplicate/duplicate_detector/ detector.py``
### **2. Model building**  
* Running our code needs the path of the directory. Modify a line in the file ``classification/operations/classification_importance.R``
### **3. Data propocessing**
* Run the script ``model/calculation/classification_importance.R``

# **Result** 
* After running the above script. the performance scores (e.g., AUC, MCC, F1, Recall@20) in each of the 1,000 bootstrap iterations into csv files.



