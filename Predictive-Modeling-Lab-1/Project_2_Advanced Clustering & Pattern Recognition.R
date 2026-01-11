# loading resouces
library(tidyverse)
library(data.table)
library(kohonen)
library(RColorBrewer)

# read data
data <- read.csv("creditworthiness.csv", header = TRUE)
colnames(data)[46] <- "credit_rating"

# spliting trainset and test
data_assessed <- data %>% filter(credit_rating != 0)
data_unassessed <- data %>% filter(credit_rating == 0)
data_assessed$credit_rating <- as.numeric(data_assessed$credit_rating)

# check demensions
cat("Total rows:", nrow(data_assessed), "\n")
cat("Total columns:", ncol(data_assessed), "\n")
print(table(data_assessed$credit_rating))

# Pearson correlation + visual TOp 10
cor_matrix <- cor(data_assessed, use = "complete.obs")
cor_target <- abs(cor_matrix[, "credit_rating"])
cor_target <- cor_target[names(cor_target) != "credit_rating"]
cor_sorted <- sort(cor_target, decreasing = TRUE)

# Top 10
top10_cor <- cor_sorted[1:10]
top10_df <- data.frame(
  Feature = names(top10_cor),
  Correlation = round(top10_cor, 4)
)

# visualization
ggplot(top10_df, aes(x = reorder(Feature, Correlation), y = Correlation, fill = Correlation)) +
  geom_col(width = 0.7) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.15) +
  coord_flip() +
  labs(title = "Top 10 Correlations with Credit Rating",
       x = "Feature", y = "Pearson Correlation") +
  theme_minimal(base_size = 12)

#  Boxplot analysis Top 10 
original_names <- names(top10_cor)
label_map <- c(
  "functionary" = "Functionary",
  "FI3O.credit.score" = "FICO Score",
  "re.balanced..paid.back..a.recently.overdrawn.current.acount" = "Repaid Overdrawn A/C",
  "credit.refused.in.past." = "Credit Refused",
  "gender" = "Gender",
  "max..account.balance.11.months.ago" = "Max Balance (11mo)",
  "avrg..account.balance.6.months.ago" = "Avg Balance (6mo)",
  "max..account.balance.3.months.ago" = "Max Balance (3mo)",
  "max..account.balance.8.months.ago" = "Max Balance (8mo)",
  "min..account.balance.6.months.ago" = "Min Balance (6mo)"
)

# swtich staa frame
data_long <- data_assessed %>%
  select(all_of(original_names), credit_rating) %>%
  pivot_longer(cols = all_of(original_names), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Variable_label = label_map[Variable],
    credit_rating = factor(credit_rating, levels = c(1, 2, 3), labels = c("A", "B", "C"))
  )

# plot
ggplot(data_long, aes(x = credit_rating, y = Value, fill = credit_rating)) +
  geom_boxplot(outlier.size = 0.3) +
  facet_wrap(~ Variable_label, scales = "free", ncol = 5) +
  labs(title = "Top 10 Features – Boxplot by Credit Rating", x = "Credit Rating", y = "Feature Value") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

# SOM analysis
input_data <- data_assessed %>% select(-credit_rating)
data_matrix <- as.matrix(scale(input_data))

som_grid <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")
som_model <- som(data_matrix, grid = som_grid, rlen = 100)

credit_factor <- factor(data_assessed$credit_rating, levels = c(1, 2, 3), 
                        labels = c("1", "2", "3"))
class_colors <- c("red", "green", "blue")

# output U-Matrix and Class Mapping
layout(matrix(1:2, ncol = 2))
par(mar = c(3, 3, 3, 1))
plot(som_model, type = "dist.neighbours", main = "U-Matrix (All)")
plot(som_model, type = "mapping", bgcol = class_colors[credit_factor],
     main = "Class Mapping (All Features)")
legend("bottom", legend = c("1", "2", "3"), col = class_colors, pch = 16, horiz = TRUE, bty = "n", inset = 0.15)

# Feature Heatmap
par(mfrow = c(3, 3))
for (i in 34:42) {
  plot(som_model, type = "property", property = som_model$codes[[1]][, i],
       main = colnames(data_matrix)[i])
}
par(mfrow = c(1, 1))

# SOM - Top 5
top5_vars <- c(
  "functionary",
  "FI3O.credit.score",
  "re.balanced..paid.back..a.recently.overdrawn.current.acount",
  "credit.refused.in.past.",
  "gender"
)
top5_matrix <- as.matrix(scale(data_assessed[, top5_vars]))
som_grid_top5 <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")
som_model_top5 <- som(top5_matrix, grid = som_grid_top5, rlen = 100)

#  U-Matrix + Class Mapping
layout(matrix(1:2, ncol = 2))
par(mar = c(3, 3, 3, 1))
plot(som_model_top5, type = "dist.neighbours", main = "U-Matrix (Top 5 Features)")
plot(som_model_top5, type = "mapping", bgcol = class_colors[credit_factor],
     main = "Class Mapping (Top 5 Features)")
legend("bottom", legend = c("1", "2", "3"), col = class_colors, pch = 16, horiz = TRUE, bty = "n", 
       inset = 0.15)

#Top 5 FM Functionary and Gender
par(mfrow = c(1, 2))
plot(som_model_top5, type = "property",
     property = som_model_top5$codes[[1]][, "functionary"],
     main = "Feature Heatmap – Functionary")
plot(som_model_top5, type = "property",
     property = som_model_top5$codes[[1]][, "gender"],
     main = "Feature Heatmap – Gender")
par(mfrow = c(1, 1))
