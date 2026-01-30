# 📊 Advanced Data Mining & R Programming Portfolio

> **Executive Summary:**
> This project demonstrates advanced predictive modeling using **R (v4.4.3)** to assess customer creditworthiness. By benchmarking multiple supervised learning algorithms, the study identifies the optimal model for financial risk assessment based on a dataset of 1,962 assessed records.

---

## 🛠️ Data Engineering & Methodology
* **Data Orchestration:** Processed and filtered a comprehensive dataset of 46 variables, resulting in **1,962 rows** after cleaning.
* **Validation Strategy:** Implemented a robust **50/50 Train-Test split**, ensuring unbiased model evaluation with 981 records in each subset.
* **Statistical Logic:** Utilized entropy-based calculations to determine an **Information Gain of 0.110784** at the root node to optimize decision tree partitioning.

---

## 🚀 Model Benchmarking & Results
| Algorithm | Accuracy | Technical Optimization Strategy |
| :--- | :--- | :--- |
| **SVM (Tuned)** | **61.88%** | Automated tuning of **Cost (1.0)** and **Gamma (0.01)** parameters. |
| **Random Forest** | **61.37%** | Optimized ensemble of **900 trees** with `mtry=18`. |
| **Decision Tree** | **60.86%** | Recursive partitioning with entropy gain analysis using `rpart`. |
| **Naive Bayes** | **53.82%** | A-priori probability modeling for discrete predictors. |

---

## 📊 Binary Classification: "A-Rating" Prediction
To specifically predict high-credit ("A" rated) customers, the models were evaluated using **ROC/AUC** curves:
* **SVM Classifier:** Achieved the top performance with an **AUC of 0.7583** and **79.31% Accuracy** at a 0.5 threshold.
* **Logistic Regression:** Achieved an **AUC of 0.7497** and **77.78% Accuracy**.
* **Key Predictors:** Identified **9 significant predictors** at the 5% level, including `functionary`, `credit.refused.in.past`, and `savings.on.other.accounts`.

---

## 🛠️ Tech Stack: The R Ecosystem
* **Core Language:** R (v4.4.3)
* **Key Libraries:** `randomForest`, `e1071` (SVM & Naive Bayes), `rpart`, `pROC`, `caret`.
* **Methodologies:** Information Gain/Entropy analysis, Hyperparameter Tuning, and ROC/AUC Evaluation.

---
*Developed by **Yanhui Ma** 
