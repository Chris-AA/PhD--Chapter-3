# Sankey diagram for land cover change 1984-2020

# Import libraries
library(raster)
library(dplyr)
library(networkD3)

# define file info
fileInfo <- data.frame(nodeCol=1, rasterFile="C:\\Users\\s1318698\\Desktop\\TEMP\\1990_REPROJ.tif", rasterBand=1) %>%
  rbind(data.frame(nodeCol=2, rasterFile="C:\\Users\\s1318698\\Desktop\\TEMP\\2000_REPROJ.tif", rasterBand= 1)) %>%
  rbind(data.frame(nodeCol=3, rasterFile="C:\\Users\\s1318698\\Desktop\\TEMP\\2010_REPROJ.tif", rasterBand= 1)) %>%
  rbind(data.frame(nodeCol=4, rasterFile="C:\\Users\\s1318698\\Desktop\\TEMP\\2020_REPROJ.tif", rasterBand=1))

# define node info
nodeInfo <- data.frame(nodeName="Dense woodland"          , nodeID=0,  mapClass=1, nodeCol=1, nodeGroup='a') %>%
  rbind(data.frame(nodeName="Open woodland"               , nodeID=1,  mapClass=2, nodeCol=1, nodeGroup='b')) %>%
  rbind(data.frame(nodeName="Savanna"                     , nodeID=2,  mapClass=3, nodeCol=1, nodeGroup='c')) %>%
  rbind(data.frame(nodeName="Parinari grasslands"         , nodeID=3,  mapClass=4, nodeCol=1, nodeGroup='d')) %>%
  rbind(data.frame(nodeName="Brachystegia grasslands"     , nodeID=4,  mapClass=5, nodeCol=1, nodeGroup='e')) %>%
  rbind(data.frame(nodeName="Cropland"                    , nodeID=5,  mapClass=6, nodeCol=1, nodeGroup='f')) %>%
  rbind(data.frame(nodeName="Urban"                       , nodeID=6,  mapClass=7, nodeCol=1, nodeGroup='g')) %>%
  rbind(data.frame(nodeName="Riverine"                    , nodeID=7,  mapClass=8, nodeCol=1, nodeGroup='h')) %>%
  rbind(data.frame(nodeName="Bare"                        , nodeID=8,  mapClass=9, nodeCol=1, nodeGroup='i')) %>%
  
  rbind(data.frame(nodeName="Dense woodland"              , nodeID=9,  mapClass=1, nodeCol=2, nodeGroup='a')) %>%
  rbind(data.frame(nodeName="Open woodland"               , nodeID=10,  mapClass=2, nodeCol=2, nodeGroup='b')) %>%
  rbind(data.frame(nodeName="Savanna"                     , nodeID=11,  mapClass=3, nodeCol=2, nodeGroup='c')) %>%
  rbind(data.frame(nodeName="Parinari grasslands"         , nodeID=12,  mapClass=4, nodeCol=2, nodeGroup='d')) %>%
  rbind(data.frame(nodeName="Brachystegia grasslands"     , nodeID=13,  mapClass=5, nodeCol=2, nodeGroup='e')) %>%
  rbind(data.frame(nodeName="Cropland"                    , nodeID=14,  mapClass=6, nodeCol=2, nodeGroup='f')) %>%
  rbind(data.frame(nodeName="Urban"                       , nodeID=15,  mapClass=7, nodeCol=2, nodeGroup='g')) %>%
  rbind(data.frame(nodeName="Riverine"                    , nodeID=16,  mapClass=8, nodeCol=2, nodeGroup='h')) %>%
  rbind(data.frame(nodeName="Bare"                        , nodeID=17,  mapClass=9, nodeCol=2, nodeGroup='i')) %>%
  
  rbind(data.frame(nodeName="Dense woodland"              , nodeID=18,  mapClass=1, nodeCol=3, nodeGroup='a')) %>%
  rbind(data.frame(nodeName="Open woodland"               , nodeID=19,  mapClass=2, nodeCol=3, nodeGroup='b')) %>%
  rbind(data.frame(nodeName="Savanna"                     , nodeID=20,  mapClass=3, nodeCol=3, nodeGroup='c')) %>%
  rbind(data.frame(nodeName="Parinari grasslands"         , nodeID=21,  mapClass=4, nodeCol=3, nodeGroup='d')) %>%
  rbind(data.frame(nodeName="Brachystegia grasslands"     , nodeID=22,  mapClass=5, nodeCol=3, nodeGroup='e')) %>%
  rbind(data.frame(nodeName="Cropland"                    , nodeID=23,  mapClass=6, nodeCol=3, nodeGroup='f')) %>%
  rbind(data.frame(nodeName="Urban"                       , nodeID=24,  mapClass=7, nodeCol=3, nodeGroup='g')) %>%
  rbind(data.frame(nodeName="Riverine"                    , nodeID=25,  mapClass=8, nodeCol=3, nodeGroup='h')) %>%
  rbind(data.frame(nodeName="Bare"                        , nodeID=26,  mapClass=9, nodeCol=3, nodeGroup='i')) %>%
  
  rbind(data.frame(nodeName="Dense woodland"              , nodeID=27,  mapClass=1, nodeCol=4, nodeGroup='a')) %>%
  rbind(data.frame(nodeName="Open woodland"               , nodeID=28,  mapClass=2, nodeCol=4, nodeGroup='b')) %>%
  rbind(data.frame(nodeName="Savanna"                     , nodeID=29,  mapClass=3, nodeCol=4, nodeGroup='c')) %>%
  rbind(data.frame(nodeName="Parinari grasslands"         , nodeID=30,  mapClass=4, nodeCol=4, nodeGroup='d')) %>%
  rbind(data.frame(nodeName="Brachystegia grasslands"     , nodeID=31,  mapClass=5, nodeCol=4, nodeGroup='e')) %>%
  rbind(data.frame(nodeName="Cropland"                    , nodeID=32,  mapClass=6, nodeCol=4, nodeGroup='f')) %>%
  rbind(data.frame(nodeName="Urban"                       , nodeID=33,  mapClass=7, nodeCol=4, nodeGroup='g')) %>%
  rbind(data.frame(nodeName="Riverine"                    , nodeID=34,  mapClass=8, nodeCol=4, nodeGroup='h')) %>%
  rbind(data.frame(nodeName="Bare"                        , nodeID=35,  mapClass=9, nodeCol=4, nodeGroup='i'))


# define group color - note that the colors correspond to the nodeGroups, one for each unique group, we have used (a, b, c, d, e, f, g, h, i) - color is applied in order
groupColor <- c("#2e9e0f", "#54cb2f", "#a86700", "#e8dd00", "#a8f83a", "#e439e2", "#ff0101", "#1680e0", "#afb7b0")

# define plot features
fontSize <- 10
fontFamily <- "sans-serif"
nodeWidth <- 2

# collapse groupColor to a string
groupColor <- paste0('"',paste(groupColor, collapse = '", "'),'"')

# join fileInfo to nodeInfo
nodeInfo <- dplyr::left_join(nodeInfo, fileInfo, by='nodeCol')

# convert factors to characters
nodeInfo$nodeName <- as.character(nodeInfo$nodeName)
nodeInfo$rasterFile <- as.character(nodeInfo$rasterFile)

# define the the links
NodeCols <- sort(unique(nodeInfo$nodeCol))
linkInfo <- data.frame()
for(i in 1:(length(NodeCols)-1)){
  fromCol <- dplyr::filter(nodeInfo, nodeCol==NodeCols[i])
  toCol <- dplyr::filter(nodeInfo, nodeCol==NodeCols[i+1])
  fromR <- values(raster(fromCol$rasterFile[1], fromCol$rasterBand[1]))
  toR <- values(raster(toCol$rasterFile[1], toCol$rasterBand[1]))
  for(f in 1:nrow(fromCol)){
    for(t in 1:nrow(toCol)){
      nFromTo <- length(which(fromR == fromCol$mapClass[f] & toR == toCol$mapClass[t]))
      linkInfo <- rbind(linkInfo, data.frame(source=fromCol$nodeID[f], target=toCol$nodeID[t], value=nFromTo))
    }
  }
}

# Define link group
linkInfo<- linkInfo %>%
  select(source, target, value) %>%
  mutate(group = case_when(
    grepl("^0$", source) ~ "a",
    grepl("^1$", source) ~ "b",
    grepl("^2$", source) ~ "c",
    grepl("^3$", source) ~ "d",
    grepl("^4$", source) ~ "e",
    grepl("^5$", source) ~ "f",
    grepl("^6$", source) ~ "g",
    grepl("^7$", source) ~ "h",
    grepl("^8$", source) ~ "i",
    grepl("^9$", source) ~ "a",
    grepl("^10$", source) ~ "b",
    grepl("11", source) ~ "c",
    grepl("12", source) ~ "d",
    grepl("13", source) ~ "e",
    grepl("14", source) ~ "f",
    grepl("15", source) ~ "g",
    grepl("16", source) ~ "h",
    grepl("17", source) ~ "i",
    grepl("18", source) ~ "a",
    grepl("19", source) ~ "b",
    grepl("20", source) ~ "c",
    grepl("21", source) ~ "d",
    grepl("22", source) ~ "e",
    grepl("23", source) ~ "f",
    grepl("24", source) ~ "g",
    grepl("25", source) ~ "h",
    grepl("26", source) ~ "i",
    grepl("27", source) ~ "a",
    grepl("28", source) ~ "b",
    grepl("29", source) ~ "c",
    grepl("30", source) ~ "d",
    grepl("31", source) ~ "e",
    grepl("32", source) ~ "f",
    grepl("33", source) ~ "g",
    grepl("34", source) ~ "h",
    grepl("35", source) ~ "i"
  ))


# make the sankey plot
sankeyNetwork(Links = linkInfo, Nodes = nodeInfo,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "nodeName",
              NodeGroup = "nodeGroup",
              LinkGroup = "group",
              fontSize = fontSize,
              fontFamily = fontFamily,
              nodeWidth = nodeWidth,
              height = 650,
              width = 1100,
              colourScale = paste0('d3.scaleOrdinal().domain(["a","b","c","d","e","f","g","h","i"]) .range([',groupColor,'])'))



