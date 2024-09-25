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
  filter(landcover %in% c(1, 2,3))%>%
  filter(markets < 150000) %>%
  filter(urban < 100000)



mean(df$deg) # mean degradation rate for area



# scale data - not used
df_scale <- df %>% mutate_if(is.numeric, z) 
df_scale$x = df$x
df_scale$y = df$y
summary(df_scale)
#df %>% summarise_all(sd)



## Model building for degradation
# 1. glm / logistic 
fm.deg <- as.formula("deg ~ buildings + roads + markets +edge_in + edge_out + urban + fire + slope + landcover + catchment")



# glm uses scaled data
m.glm <- glm(fm.deg, data=df_scale, family="binomial"(link = 'logit'))
summary(m.glm)
plot_model(m.glm)
anova(m.glm, test = "Chisq") # Table of deviance
performance::performance(m.glm)
performance::performance_accuracy(m.glm)
table(predict(m.glm)>0, df_scale$deg)



## RF model
# make balanced data set to train the model - could be improved with SMOTE
df_1 <- df %>%filter(deg ==1)
df_0 <- df %>%filter(deg ==0) %>% sample_n(nrow(df_1))
df_bal <- bind_rows(df_1, df_0)



df_bal$deg <- as.factor(ifelse(df_bal$deg, 1, 0))
m.rf <- randomForest(fm.deg, 
                     data = df_bal, ntree= 100, importance = FALSE, regression = FALSE, do.trace = TRUE)
m.rf

#m.deg <- saveRDS(m.rf, file = "data/rf_models/m_deg.rds")
#bal.deg <- saveRDS(df_bal, file = "data/rf_models/bal_deg.rds") #save balanced df
# RF model prediction with full data set
p0 <- predict(m.rf, newdata = df)
p0m <- mean(as.numeric(p0)-1) # -1 as levels are 1 and 2, not 0 and 1
p0m # mean degradation prediction for whole area

# Create vector of +ve/-ve values based on trends in PDPs
trend <- setNames(c(1 , -1, 1, -1, -1,-1, 1, -1), c("buildings", "roads", "markets", "edge_in", "edge_out","urban", "fire", "slope"))


# define function to vary each var and make new prediction
pred_effect <- function(vari, model, data){
  stopifnot(vari %in% names(coef(m.glm)))
  df_pred <- data
  df_pred[vari] = ifelse(
    #coef(m.glm)[vari] >0,# for positive effects, 2x the data. For negative effects, halve it
     trend[vari]> 0,
    df_pred[vari]*2 ,
    df_pred[vari]/2
  )
  deg_hat <- predict(model, newdata = df_pred)
  mean(as.numeric(deg_hat)-1) # from 0 and 1 levels, asnumeric makes 1 and 2s
}
# example for roads: 
# dhat <- pred_effect("roads", m.rf, df)



# get each var name
vars <- names(coef(m.glm))[c(-1, -10, -11, -12, -13, -14, -15, -16)]

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
scenario_deg_rate <- unlist(L)
names(scenario_deg_rate) <- vars2



scenario_deg_change <- (scenario_deg_rate-p0m)/p0m*100
deg_change <- lattice::dotplot(scenario_deg_change, 
                 xlab = "% change in dense-open transitions under doubling or halving of driver for whole of study area")
deg_change
# PDPs
library(pdp)
library(ggplot2)
library(gridExtra)

roads.pdp_deg <- partial(m.rf, pred.var= c("roads"),chull = TRUE, rug = TRUE, trim.outliers = TRUE, which.class = 2, prob = TRUE) %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#2b83ba", linewidth = 1) +
  scale_x_continuous(lim = c(0,150000), breaks = c(0,25000,50000,75000,100000, 125000, 150000),labels = c("0", "25", "50", "75", "100", "125", "150")) +
  coord_cartesian(ylim = c(0.45,0.52))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Dense-open transition") +
  xlab("Distance to road (km)")

markets.pdp_deg <- partial(m.rf, pred.var= c("markets"), chull = TRUE, rug = TRUE, trim.outliers = TRUE, which.class = 2, prob = TRUE) %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#2b83ba", linewidth = 1) +
  scale_x_continuous(lim = c(0,150000), breaks = c(0,25000,50000,75000,100000, 125000, 150000),labels = c("0", "25", "50", "75", "100", "125", "150")) +
  coord_cartesian(ylim = c(0.45,0.52))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Dense-open transition") +
  xlab("Distance to markets (km)")

urban.pdp_deg <- partial(m.rf, pred.var= c("urban"), chull = TRUE, rug = TRUE, trim.outliers = TRUE, which.class = 2, prob = TRUE)  %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#2b83ba", linewidth = 1) +
  scale_x_continuous(lim = c(0,150000), breaks = c(0,25000,50000,75000,100000, 125000, 150000),labels = c("0", "25", "50", "75", "100", "125", "150")) +
  coord_cartesian(ylim = c(0.45,0.52))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Dense-open transition") +
  xlab("Distance to urban centre(km)")

fire.pdp_deg <- partial(m.rf, pred.var= c("fire"), chull = TRUE, rug = TRUE, trim.outliers = TRUE, which.class = 2, prob = TRUE) %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#2b83ba", linewidth = 1) +
  #scale_x_continuous(lim = c(0,150000), breaks = c(0,25000,50000,75000,100000, 125000, 150000),labels = c("0", "25", "50", "75", "100", "125", "150")) +
  coord_cartesian(ylim = c(0.3,0.7))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Dense-open transition") +
  xlab("Fire count (2001-2019)")

edge_in.pdp_deg <- partial(m.rf, pred.var= c("edge_in"), chull = TRUE, rug = TRUE, trim.outliers = TRUE, which.class = 2, prob = TRUE) %>%
  autoplot(rug = TRUE, train = df_bal, colour = "#2b83ba", linewidth = 1) +
  #scale_x_continuous(lim = c(0,150000), breaks = c(0,25000,50000,75000,100000, 125000, 150000),labels = c("0", "25", "50", "75", "100", "125", "150")) +
  coord_cartesian(ylim = c(0.3,0.7))+
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  ylab("Odds of Dense-open transition") +
  xlab("Internal distance to woodland edge (m)")

edge_out.pdp_deg <- partial(m.rf, pred.var= c("edge_out"), chull = TRUE, rug = TRUE, plot = TRUE, which.class = 2, prob = TRUE) # outliers not trimmed to see data
buildings.pdp_deg <- partial(m.rf, pred.var= c("buildings"), chull = TRUE, rug = TRUE, plot = TRUE, which.class = 2, prob = TRUE) # outliers not trimmed to see data
slope.pdp_deg <- partial(m.rf, pred.var= c("slope"), chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, which.class = 2, prob = TRUE)

deg_pdps1 <- grid.arrange(roads.pdp_deg, markets.pdp_deg, urban.pdp_deg, ncol = 3)
deg_pdps2 <- grid.arrange(fire.pdp_deg, edge_in.pdp_deg,ncol = 2)

# pdp_plots <- list()
# 
# for (var in vars) {
#   pdp_deg <- partial(m.rf, pred.var = var, chull = TRUE, rug = TRUE, plot = TRUE, trim.outliers = TRUE, which.class = 2)
#   pdp_plots[[var]] <- pdp_deg
# }
# 
# deg_pdps <- grid.arrange(grobs = pdp_plots)

ggsave("outputs/deg_pdps_FLARE.png", deg_pdps, width = 35, height = 25, units = "cm", dpi = 300)
