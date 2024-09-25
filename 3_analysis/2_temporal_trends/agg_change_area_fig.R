# RQ1: 1a. Overall trends of change
# Plot aggregated change classes from error adjusted areas: deforestation, degradation, growth, crop regrowth and uncertainties

# Libraries
library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("C:\\Users\\s1318698\\Documents\\CH1\\rq1\\change_agg_2.csv")
df <- df %>% 
  mutate(year = case_when(
    epoch == 1 ~ 1993,
    epoch == 2 ~ 1996,
    epoch == 3 ~ 1999,
    epoch == 4 ~ 2002,
    epoch == 5 ~ 2005,
    epoch == 6 ~ 2008,
    epoch == 7 ~ 2011,
    epoch == 8 ~ 2014,
    epoch == 9 ~ 2017,
    epoch == 10 ~ 2020,
    ))

# Plot gross change over time
df$class <- factor(df$class, levels=c('def', 'deg', 'growth', 'cropreg')) #change factor order

agg_ch <- df %>% 
  ggplot( aes(x=year, y=erradj_km2, ymin = erradj_km2 - se_km2, ymax = erradj_km2 + se_km2,
              group= class, fill=class, colour = class)) +
  geom_vline(xintercept=c(1994, 1998, 2002), linetype="dotted") +
  geom_line(linewidth = 1) +
  #geom_point()+
  geom_errorbar(aes(ymin = erradj_km2 - se_km2,
                  ymax = erradj_km2 + se_km2), width = 0.5,position = position_dodge(0)) +
  scale_x_continuous(breaks = c(1993, 1996, 1999, 2002, 2005, 2008, 2011,
                                  2014, 2017, 2020)) +
  #scale_y_continuous(expand = c(0,0)) +
  #guides(colour = "none") +
  scale_fill_manual(name = 'Transition Type',
                    labels = c('Cropland regrowth', 'Deforestation', 'Dense-open transitions', 'Open-dense transitions'),
                    values = c("#f76dc1", "#d7191c", "#2b83ba", "#a6d96a")) +
  scale_colour_manual(values = c("#d7191c", "#2b83ba", "#a6d96a", "#f76dc1"),
                      labels = c('Deforestation', 'Canopy opening', 'Canopy closure', 'Vegetation regrowth'))+ 
  scale_y_continuous(labels = scales::comma_format()) +
  #ggtitle("Aggregated area of LUC types") +
  theme_classic() +
  theme(legend.title=element_blank(),
        legend.position = c(0.85, 0.7),
        legend.text=element_text(size= 12),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14),
        plot.margin = margin(0.2,1,0.2,0.2,"cm")) +
  ylab(expression(Gross~area~of~change~(km^2))) +
  xlab("Year") +
  annotate(geom="text", x=1992, y=7300, label="War", color="black", size = 4) + #92-94 war
  annotate(geom="text", x=1996, y=7300, label="Ceasefire", color="black", size = 4) + #94-98 ceasefire
  annotate(geom="text", x=2000, y=7300, label="War", color="black", size = 4) #98-02 war


agg_ch

ggsave("C:\\Users\\s1318698\\Documents\\CH1\\rq1\\outputs\\figure_2.png", agg_ch, width = 25, height = 15, units = "cm", dpi = 300)

# # Calculate overall net change
# df_net <- df %>%
#   group_by(class) %>%
#   summarize(gross_erradj_km2 = sum(erradj_km2))
# 
# gross_km2_sub <- c(8421.834, 7397.275, 	15838.580, 	26159.092) # gross area values as vector in order to subtract
# 
# df_net$net_erradj_km2 <- df_net$gross_erradj_km2 - gross_km2_sub
# 
# # Plot net change
# ggplot(df_net, aes(x = class, y = net_erradj_km2)) +
#   geom_bar(stat = "identity")
# 
# # War vs post war net change
# df_war <- df %>%
#   filter(epoch <= 4) %>%
#   group_by(class) %>%
#   summarize(gross_erradj_km2 = sum(erradj_km2)) %>%
#   mutate(epoch = "war")
# 
# df_post <- df %>%
#   filter(epoch >= 5) %>%
#   group_by(class) %>%
#   summarize(gross_erradj_km2 = sum(erradj_km2)) %>%
#   mutate(epoch = "post")
# 
# df_net_epoch <- rbind(df_war, df_post)
# 
# gross_km2_sub2 <- c(2299.807, 2510.014, 4264.350, 16101.864, 6122.027, 4887.261, 11574.230, 10057.228)
# 
# df_net_epoch$net_erradj_km2 <- df_net_epoch$gross_erradj_km2 - gross_km2_sub2
# 
# # Plot
# ggplot(df_net_epoch, aes(x = class, y = net_erradj_km2, fill = epoch)) +
#   geom_bar(position = "dodge", stat = "identity")


