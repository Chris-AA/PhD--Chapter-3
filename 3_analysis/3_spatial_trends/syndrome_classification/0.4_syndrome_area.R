# Quantify and plot areas of syndromes

library(dplyr)
library(ggplot2)

# Import data
df <- read.csv(file.path("data", "syndrome2_report.csv"))
df <- df[df$Class != 2022.29834, ]
df$Class <- as.factor(df$Class)
levels(df$Class) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                          "16", "17", "18", "19", "20", "21", "22", "23") # Rename factor levels to simple ints

# Plot
ggplot(df, aes(x= Class, y = Area_km2)) +
  geom_bar(stat = "identity") +
  labs(y= "Area_km2", x = "Syndrome")
