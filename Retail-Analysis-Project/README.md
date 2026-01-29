# Retail Customer Segmentation & Propensity Modeling (R)

## 📌 Project Overview
This project analyzes **540k+ transactional records** from a UK-based online retailer to identify geographic and temporal sales patterns, segment customers using behavioral metrics, and build a predictive model to identify high-value potential customers.

**Key Outcome:** Identified a "hidden" customer segment with an **8.3x value growth potential** and developed an ANN model with **90.54% accuracy** to prioritize marketing outreach.

---

## 🚀 Business Value & Insights
The analysis translated raw data into four strategic pillars for business growth:

* **Geographic Risk Mitigation:** Identified that **82% of revenue** is concentrated in the UK, proposing a diversification strategy to reduce market dependency.
* **Operational Efficiency:** Discovered that Thursday revenue is **2.5x higher** than Sunday, enabling data-driven staff and inventory allocation.
* **Customer Growth:** Leveraged RFM & K-means to identify four distinct clusters. Found that **40.7% of "Occasional Buyers"** have the propensity to become high-value VIPs.
* **Revenue Optimization:** Transitioning a customer from "Occasional" to "Regular" results in an **8.3-fold increase** in lifetime value.

---

## 🛠️ Technical Stack
* **Language:** R (Advanced)
* **Data Processing:** `tidyverse`, `data.table`, `lubridate`
* **Machine Learning:** K-means Clustering, Artificial Neural Networks (ANN via `caret`) 
* **Visualization:** `ggplot2`, Tableau (for interactive dashboards)

---

## 📊 Methodology
1.  **Data Cleaning:** Developed a multi-step pipeline to handle missing values (135k+ CustomerIDs) and outliers, resulting in a validated dataset of 392k unique transactions.
2.  **Feature Engineering:** Derived behavioral features including Recency, Frequency, Total Spent, Average Purchase Interval, and Product Variety.
3.  **Clustering:** Used the **Elbow Method** to determine optimal $k=4$ for consumer segmentation (VIPs, Active, Occasional, Churned).
4.  **Propensity Modeling:** Built an **ANN classifier** (5-fold cross-validation) to predict high-value potential (Top 20% of spenders).


---

## 📈 Key Visualizations
> *Note: Below are highlights from the analysis report.*

* **Customer Lifetime Value by Segment:** Demonstrates the exponential value growth between clusters.
* **Confusion Matrix:** 90.54% Accuracy | 79.8% Precision.
* **Radar Chart:** Comparison of behavioral features across clusters.

---

## 📝 Recommendations
* **Targeted Activation:** Deploy ANN scores to prioritize marketing budgets toward Cluster 4 customers with the highest conversion probability.
* **Inventory Planning:** Increase production capacity by **40%** from September to November to meet seasonality peaks.
* **Loyalty Programs:** Implement tiered retention strategies aligned with the 8.3x value increase found in segment transitions.
