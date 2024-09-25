# Script to identify mean luc rates for syndrome classes
# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Import and format data into one dataframe
df_rates <- readRDS(file.path("data","df_syndrome.csv")) # Pre-calculated LUC rates
df_syndrome <- readRDS(file.path("data","df_rq5.Rds")) %>%
  na.omit() %>%
  filter(syndrome > 0)
df_syndrome <- df_syndrome[c("grid10", "syndrome1")] # Df with grid10 and matched classified syndromes
df_syndrome <- df_syndrome[sample(nrow(df_syndrome),length(df_rates[[1]])),] # Randomly sample to same length as df_rates
df <- merge(df_rates, df_syndrome, by = "grid10") # Produces large df?

# Summarise luc rates for each syndrome
df_means <- df %>% 
  group_by(syndrome) %>%
  summarise(
  def_mean = mean(def),
  deg_mean = mean(deg),
  growth_mean = mean(grow))

# Elongate df to plot
df_long <- df_means %>%
  pivot_longer(-c(syndrome), names_to = "luc_type", values_to = "mean_rate")
df_long$syndrome <- as.factor(df_long$syndrome)

# Plot
ggplot(df_long, aes(fill=luc_type, y=mean_rate, x=syndrome)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("red", "orange", "green")) +
  theme(axis.text.x = element_text(angle = 90))
