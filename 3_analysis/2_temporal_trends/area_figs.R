### Area line, stacked and barplot figures ###

### COLOURS AND CLASS LABELS NEED FINALISING ON SOME FIGS ###


# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Static Data
ds_s<- read.csv(file = "C:\\Users\\s1318698\\Documents\\ch1_rq1\\static.csv", header = TRUE)

# Static LC graphs----
# Modify data types

ds_s$class <- as.factor(ds_s$class)
ds_s$err_adj_area <- as.numeric(as.character(ds_s$err_adj_area))
ds_s$se <- as.numeric(as.character(ds_s$se))
ds_s$X95_ci <- as.numeric(as.character(ds_s$X95_ci))

# Create subset after changing data-types
ds_s_sub <- na.omit(ds_s) # subset to remove rows with na

ds_s_sub$X95_ci <- as.numeric(ds_s_sub$X95_ci) # not necessary

# Order classes
ds_s$class <- factor(ds_s$class, levels=c("9","8","7","6","5","4","3","2","1"))

# Static_map area: stacked area chart
pal <- c("#000000","#2e9e0f", "#54cb2f", "#a86700", "#e8dd00", "#a8f83a", "#e439e2", "#ff0101", "#1680e0", "#afb7b0")
ggplot(ds_s, aes(x=ds_s$year, y=ds_s$map_km2, fill=ds_s$class)) + 
  geom_area() +
  scale_fill_manual(labels = c("Bare", "Riverine", "Urban", "Cropland", "Brachystegia grasslands", "Parinari grasslands", "Savanna", "Open woodland", "Dense woodland"), values = rev(pal))+
  theme_classic() +
  labs(x = "Year",
       y = "Area (km2)",
       fill = "Class"
  ) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

# Static error adjusted stacked
ggplot(ds_s_sub, aes(x=ds_s_sub$year, y=ds_s_sub$err_adj_area, fill=ds_s_sub$class)) + 
  geom_area() +
  scale_fill_manual(labels = c("Bare", "Riverine", "Urban", "Cropland", "Brachystegia grasslands", "Parinari grasslands", "Savanna", "Open woodland", "Dense woodland"), values = rev(pal))+
  theme_classic() +
  labs(x = "Year",
       y = "Area (km2)",
       fill = "Class"
  ) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

# Static map area line graph
ggplot(ds_s, aes(x=year, y=map_km2, group=class, color=class)) +
  geom_line()+
  theme_classic() +
  scale_color_manual(labels = c("Bare", "Riverine", "Urban", "Cropland", "Brachystegia grasslands", "Parinari grasslands", "Savanna", "Open woodland", "Dense woodland"), values = rev(pal))+
  labs(x = "Year",
       y = "Area (km2)",
       color = "Class"
  )

# Static error adjusted area: line graph 
ggplot(ds_s_sub, aes(x=year, y=err_adj_area, group=class, color=class)) +
  geom_line()+
  theme_classic() +
  scale_color_manual(labels = c("Bare", "Riverine", "Urban", "Cropland", "Brachystegia grasslands", "Parinari grasslands", "Savanna", "Open woodland", "Dense woodland"), values = rev(pal))+
  labs(x = "Year",
       y = expression(Area~km^2),
       color = "Class"
  )

# Combined woodland calc
discard <- c(3,4,5,6,7,8,9,10)
ds_wood <- ds_s_sub %>% filter(!as.integer(class) %in% discard) # subset to class 1 and 2
ds_wood_comb <- ds_wood %>% #sum areas
  group_by(year) %>%
  summarise(map_km2 = sum(map_km2), err_adj_area= sum(err_adj_area))
ds_wood_comb$se <- c(4000,4000,4000,3000)
ds_wood_comb$X95_ci <- ds_wood_comb$se*1.96
ds_wood_comb$class <- as.factor(rep("total",times = 4))

# Bind combined to d_s_sub
ds_s_sub <- subset(ds_s_sub, select = -c(pix_area))
ds_s_sub <- rbind(ds_s_sub, ds_wood_comb)
ds_s_sub$linetype <- rep("error adjusted", times = 40)

# Calc total woodland map area in ds_s
ds_s$class <- factor(ds_s$class, levels= rev(c("9","8","7","6","5","4","3","2","1"))) #reorder
ds_map_wood <- ds_s %>% filter(!as.integer(class) %in% discard)
ds_map_wood_comb <- ds_map_wood %>% #sum areas
  group_by(year) %>%
  summarise(map_km2 = sum(map_km2))
ds_map_wood_comb$class <- as.factor(rep("total",times = 31))

# Bind combined to d_s
ds_s <- subset(ds_s, select = -c(pix_area, err_adj_area, se, X95_ci))
ds_s <- rbind(ds_s, ds_map_wood_comb)
ds_s$linetype <- rep("map", times=310)

ds_s$class <- factor(ds_s$class, levels= rev(c("9","8","7","6","5","4","3","2","1", "total")))
ds_s_sub$class <- factor(ds_s_sub$class, levels= rev(c("9","8","7","6","5","4","3","2","1","total")))

labels <- rev(c("Bare", "Riverine", "Urban", "Cropland", "Grassland 2", "Grassland 1", "Open savanna", "Woodland", "Dense woodland and forest", "Total woodland"))

# Merged line graph
area <- ggplot(NULL, aes(x=year)) +
  #geom_line(data = ds_s, aes(y=map_km2, group=class, color=class, linetype = linetype)) +
  geom_line(data = ds_s_sub, aes(x=year, y=err_adj_area, group=class,color=class), size = 1) + #linetype = linetype
  geom_point(data = ds_s_sub, aes(x=year, y=err_adj_area, group=class, color=class))+
  geom_errorbar(data = ds_s_sub, aes(ymin=err_adj_area-X95_ci, ymax=err_adj_area+X95_ci, color= class),width=.1)+ # line graph bad for visualising CI
  theme_classic() +
  theme(legend.text=element_text(size=12),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14)) +
  scale_color_manual(labels = labels, values = pal) +
  scale_y_continuous(labels = scales::comma_format()) +
  #scale_linetype_manual(name = NULL, values = c(1,3), labels = c("Error adjusted area", "Map area")) +
  labs(x = "Year",
       y = expression(Area~(km^2)),
       color = element_blank()
  )

area
ggsave("C:\\Users\\s1318698\\Documents\\ch1_rq1\\outputs\\area_fig3.png", area, width = 25, height = 20, units = "cm", dpi = 300)
