# Libraries
library(dplyr)
library(sjPlot)
library(randomForest)
z <- function(x) (x-mean(x))/sd(x)



# get data and clean
df <- as_tibble(readRDS(file.path("data","df.Rds"))) %>%
  select(-ends_with("_yr") & -starts_with("adm") & -starts_with("syndrome") & -starts_with("grid")) %>%
  filter(!is.na(landcover) ) %>%
  mutate(buildings = pop/9) %>% # to per km2
  mutate(across(c(landcover, catchment), as.factor)) %>%
  filter(landcover %in% c(6))



mean(df$cropreg) # mean cropreg rate for area



# scale data - not used
df_scale <- df %>% mutate_if(is.numeric, z) 
df_scale$x = df$x
df_scale$y = df$y
summary(df_scale)
#df %>% summarise_all(sd)



## Model building for cropreg
# 1. glm / logistic 
fm.cropreg <- as.formula("cropreg ~ buildings + roads + markets + urban + fire + slope + catchment")



# glm uses scaled data
m.glm <- glm(fm.cropreg, data=df_scale, family="binomial"(link = 'logit'))
summary(m.glm)
plot_model(m.glm)
anova(m.glm, test = "Chisq") # Table of deviance
performance::performance(m.glm)
performance::performance_accuracy(m.glm)
table(predict(m.glm)>0, df_scale$cropreg)



## RF model
# make balanced data set to train the model - could be improved with SMOTE
df_1 <- df %>%filter(cropreg ==1)
df_0 <- df %>%filter(cropreg ==0) %>% sample_n(nrow(df_1))
df_bal <- bind_rows(df_1, df_0)



df_bal$cropreg <- as.factor(ifelse(df_bal$cropreg, 1, 0))
m.rf <- randomForest(fm.cropreg, 
                     data = df_bal, ntree= 100, importance = FALSE, regression = FALSE, do.trace = TRUE)
m.rf

m.cropreg <- saveRDS(m.rf, file = "data/rf_models/m_cropreg.rds")
bal.cropreg <- saveRDS(df, file = "data/rf_models/bal_cropreg.rds") #save balanced df
# RF model prediction with full data set
p0 <- predict(m.rf, newdata = df)
p0m <- mean(as.numeric(p0)-1) # -1 as levels are 1 and 2, not 0 and 1
p0m # mean cropreg prediction for whole area

# Create vector of +ve/-ve values based on trends in PDPs
trend <- setNames(c(-1 , 1, -1, 1, -1, -1), c("buildings", "roads", "markets","urban", "fire", "slope"))

# define function to vary each var and make new prediction
pred_effect <- function(vari, model, data){
  stopifnot(vari %in% names(coef(m.glm)))
  df_pred <- data
  df_pred[vari] = ifelse(
    trend[vari] >0,# for positive effects, 2x the data. For negative effects, halve it
    df_pred[vari]*2 ,
    df_pred[vari]/2
  )
  cropreg_hat <- predict(model, newdata = df_pred)
  mean(as.numeric(cropreg_hat)-1) # from 0 and 1 levels, asnumeric makes 1 and 2s
}
# example for roads: 
# dhat <- pred_effect("roads", m.rf, df)



# get each var name
vars <- names(coef(m.glm))[c(-1, -8, -9, -10, -11, -12)]

k=0
vars2 = vars
for (v in vars) {
  k=k+1
  if (trend[v] >0){
    vars2[k] <- paste(vars[k], "doubled")
  } else {
    vars2[k] <- paste(vars[k], "halved")
  }
  
}
vars2

# run the prediction for each var
L <- parallel::mclapply(vars, 
                        function(v) pred_effect(v, model = m.rf, data = df), mc.cores = 8)
L



# tidy the results
scenario_cropreg_rate <- unlist(L)
names(scenario_cropreg_rate) <- vars2



scenario_cropreg_change <- (scenario_cropreg_rate-p0m)/p0m*100
lattice::dotplot(scenario_cropreg_change, 
                 xlab = "% change in cropland regrowth under doubling or halving of driver")

# PDPs
library(pdp)
library(ggplot2)
library(gridExtra)
# 

markets.pdp_cropreg <- partial(m.rf, pred.var= c("markets"), chull = TRUE, rug = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)  %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#f76dc1", linewidth = 1) +
  scale_x_continuous(lim = c(0,100000), breaks = c(0,25000,50000,75000,100000),labels = c("0", "25", "50", "75", "100")) +
  coord_cartesian(ylim = c(0.40,0.6))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Fallow growth") +
  xlab("Distance to urban centre (km)")

urban.pdp_cropreg <- partial(m.rf, pred.var= c("urban"), chull = TRUE, rug = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)  %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#f76dc1", linewidth = 1) +
  scale_x_continuous(lim = c(0,100000), breaks = c(0,25000,50000,75000,100000),labels = c("0", "25", "50", "75", "100")) +
  coord_cartesian(ylim = c(0.40,0.6))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Fallow growth") +
  xlab("Distance to urban centre (km)")

buildings.pdp_cropreg <- partial(m.rf, pred.var= c("buildings"), chull = TRUE, rug = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)  %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#f76dc1", linewidth = 1) +
  xlim(0,30) +
  coord_cartesian(ylim = c(0.40,0.6))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Fallow growth") +
  xlab(expression(Number~of~buildings~per~km^2))

# roads.pdp_cropreg <- partial(m.rf, pred.var= c("roads"),chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)
# #edge_in.pdp_cropreg <- partial(m.rf, pred.var= c("edge_in"), chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)
# edge_out.pdp_cropreg <- partial(m.rf, pred.var= c("edge_out"), chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)
# buildings.pdp_cropreg <- partial(m.rf, pred.var= c("buildings"), chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)
# fire.pdp_cropreg <- partial(m.rf, pred.var= c("fire"), chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)
# slope.pdp_cropreg <- partial(m.rf, pred.var= c("slope"), chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, prob = TRUE, which.class = 2)

cropreg_pdps <- grid.arrange(markets.pdp_cropreg, urban.pdp_cropreg, buildings.pdp_cropreg, ncol = 3)

# pdp_plots <- list()
# 
# for (var in vars) {
#   pdp_cropreg <- partial(m.rf, pred.var = var, chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, which.class = 2)
#   pdp_plots[[var]] <- pdp_cropreg
# }
# 
# cropreg_pdps <- grid.arrange(grobs = pdp_plots)

ggsave("outputs/cropreg_pdps.png", cropreg_pdps, width = 50, height = 20, units = "cm", dpi = 300)
