# Script to identify mean luc rates for syndrome classes
# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Import and format data into one dataframe
df_rates <- readRDS(file.path("data","df_syndrome2.csv")) # Pre-calculated LUC rates
df_syndrome <- readRDS(file.path("data","df_rq5.Rds")) %>%
  na.omit() %>%
  filter(syndrome2 > 0)
df_syndrome <- df_syndrome[c("grid10", "syndrome2")] # Df with grid10 and matched classified syndromes
df_syndrome <- df_syndrome[sample(nrow(df_syndrome),length(df_rates[[1]])),] # Randomly sample to same length as df_rates
df <- merge(df_rates, df_syndrome, by = "grid10") # Produces large df?
df$syndrome2 <- as.factor(df$syndrome2)
levels(df$syndrome2) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                               "16", "17", "18", "19", "20", "21", "22", "23") # Rename factor levels to simple ints
 # Summarise luc rates for each syndrome
df_means <- df %>% 
  group_by(syndrome2) %>%
  summarise(
  def_mean = mean(def),
  deg_mean = mean(deg),
  growth_mean = mean(grow),
  cropreg_mean = mean(cropreg))

# Elongate df to plot
df_long <- df_means %>%
  pivot_longer(-c(syndrome2), names_to = "luc_type", values_to = "mean_rate")

# Stacked Bar Plot
ggplot(df_long, aes(fill=luc_type, y=mean_rate, x=syndrome2)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("pink", "red", "orange", "green")) +
  theme(axis.text.x = element_text(angle = 90))

# PCA to identify syndromes to aggregate
pc <- prcomp(df_means[2:5])
attributes(pc)
library(ggfortify)
autoplot(pc, data = df_means, colour = "syndrome2", label = TRUE, shape = FALSE, 
         loadings = TRUE, loadings.label = TRUE)
