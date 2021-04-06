library(dplyr)
library(LICORS)
library(scales)
library(networkD3)
data_csv<-read.csv(file.choose())

year_2008_2018<-filter(data_csv, Yp >= 2008 & Yp <= 2018)

IRegionRefinedp<-filter(year_2008_2018, IRegionRefinedp > 0 & IRegionRefinedp < 4)

year_2008_2018

df_mono = year_2008_2018 %>% filter(NEUROLONGXSAp == 0 & NEUROLONGXCIPp == 0)

mono_mat = matrix(0L, nrow = 9, ncol = 6)
# mono matrix
for(i in 1:nrow(df_mono)){
  row = df_mono[i,]
  vSA = c(row$SA1, row$SA2, row$SA3, row$SA4, row$SA5, row$SA6)
  vCIP = c(row$CIP3, row$CIP1, row$CIP4, row$CIP2, row$CIP6, row$CIP7, row$CIP5, row$CIP8, row$CIP9)
  vSA = round(vSA / sum(vSA),2)
  for(k in which(vCIP > 0)){
    for(j in 1:6){
      mono_mat[[k,j]] = mono_mat[[k,j]] + vSA[j]
    }
    
  }
  
}
print(mono_mat)


m = mono_mat
for(i in 1:9){
  row = mono_mat[i,]
  # m[i,] = sapply(row, function(X) {(X - min(row))/(max(row)-min(row))})
  m[i,] = rescale(row, to=c(0,1))
  
}
mm_b = apply(m, 2, function(x) {ifelse(x > 0.5, round(x,2), 0)})
mm = rescale(mm_b, to=c(0,0.02))




nodes = data.frame("name" = c("CIP3", "CIP1", "CIP4", "CIP2", "CIP6", "CIP7", "CIP5", "CIP8", "CIP9", "SA1", "SA2", "SA3", "SA4", "SA5", "SA6"))
links = as.data.frame(matrix(c(0,9, mm[1,1],
                               0,12, mm[1,4],
                               1,9, mm[2,1],
                               1,10, mm[2,2],
                               1,12, mm[2,4],
                               2,10, mm[3,2],
                               2,11, mm[3,3],
                               2,12, mm[3,4],
                               3,10, mm[4,2],
                               3,11, mm[4,3],
                               3,12, mm[4,4],
                               4,10, mm[5,2],
                               4,12, mm[5,4],
                               5,10, mm[6,2],
                               5,11, mm[6,3],
                               5,12, mm[6,4],
                               6,12, mm[7,4],
                               7,10, mm[8,2],
                               7,12, mm[8,4],
                               8,10, mm[9,2],
                               8,11, mm[9,3],
                               8,12, mm[9,4],
                               12,12, 0.02), byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
links$group <- as.factor(c("type_0","type_0","type_1","type_1","type_1","type_2", "type_2","type_2","type_3","type_3", "type_3","type_4","type_4","type_5","type_5","type_5","type_6","type_7","type_7","type_8","type_8","type_8", "type_12"))
node_color <- 'd3.scaleOrdinal() .domain(["CIP3", "CIP1", "CIP4", "CIP2", "CIP6", "CIP7", "CIP5", "CIP8", "CIP9", "SA1", "SA2", "SA3", "SA4", "SA5", "SA6", "type_0", "type_1", "type_2", "type_3", "type_4", "type_5", "type_6", "type_7", "type_8", "type_12"]) .range(["yellow", "red", "darkolivegreen" , "orange", "seagreen", "palegreen", "green", "gray", "gray", "red", "orange", "lightgreen", "darkolivegreen", "black", "gray", "yellow", "red", "darkolivegreen" , "orange", "seagreen", "palegreen", "green", "gray", "gray","white"])'





p = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 12, nodeWidth = 20,
                  height = 800, width = "100%",
                  colourScale=node_color,
                  LinkGroup="group",
                  iterations = 0, 
                  nodePadding=10)

p














