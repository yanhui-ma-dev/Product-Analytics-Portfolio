# 🚖 Rome Taxi Spatiotemporal Data Analytics

> **Project Overview:** > This project involves the large-scale analysis of over **21.8 million GPS points** collected from 320 taxi drivers in Rome. The primary focus is on spatiotemporal data orchestration, feature engineering, and identifying abnormal driving behaviors.

## 🛠️ Key Technical Implementations
* **Data Cleaning & Preprocessing:** Identified and filtered invalid records, outliers based on city boundaries (41.65°N - 42.05°N), and noise (GPS jumps > 200 km/h).
* **Activity Modeling:** Quantified driver activity levels, identifying a 30-day average activity duration of **631.64 hours**.
* **Trajectory Analysis:** Performed localized analysis for specific taxi IDs, calculating total driving distances (e.g., Taxi 261 covered **2,573.96 km**) using the Haversine formula.
* **Visualization:** Generated radial distribution plots and heatmaps to visualize high-density urban driving patterns versus peripheral excursions.

## 📊 Analytical Insights
| Metric | Global Average | Taxi ID 261 |
| :--- | :--- | :--- |
| **Active Duration** | 631.64 hrs| 601.19 hrs |
| **Data Coverage** | ~87.7% of month | ~83.5% of month |
| **Mean Latitude** | 41.89 | 41.90 |

---
