library(ggplot2)
library(MASS)
library(tidyverse)
library(dplyr)
library(sqldf)
library(data.table)
library(ggtext)
library(gridExtra)
library(gtable)
library(cowplot)
library(grid)
dataFromCSV<-read.csv(file.choose())

fd<-dataFromCSV %>%
  group_by(Yp, NSAp, Zp>0) %>%
  summarise(
    cnt = n()
  ) %>%
  filter(
    Yp>1979
  )

fd
yrcnt = NULL
years = 1980:2018

for (i in 1:length(years)){
  hht <- filter(fd, Yp==i+1979)
  yrcnt[i] <- ( sum(hht[4]))
}

yrcnt





############################################
# FIGURE 2A - 1st Graph SA Categories
############################################

# Specify years and categories to analyze
years <- 1980:2018
cats <- 1:4


# Create matrix to store SA papers in specified categories by years
yrSAcat <- matrix(data=NA,nrow= length(cats)*2, ncol=length(years) )

# Create vectors to store total SA papers with below average citations in specified categories by years
yrSAtotNEG <- matrix(data=NA, nrow = 1, ncol = length(years))

# Create vectors to store total SA papers with above average citations in specified categories by years
yrSAtotPOS <- matrix(data=NA, nrow = 1, ncol = length(years))



# create negative & positive arrays to store values from yrSAcat
negarr <- (cats-1)*2+1
posarr <- (cats)*2





for (i in 1:length(years)){
  for (j in 1:(length(cats)*2)){
    rr <- filter(fd, Yp == (i+1979) )
    yrSAcat[j,i] <- rr[[j,4]]
  }
  yrSAtotNEG[1,i] <- sum(yrSAcat[negarr,i])
  yrSAtotPOS[1,i] <- sum(yrSAcat[posarr,i])
  
}



colnames(yrSAcat) <- c(1980:2018)
rownames(yrSAcat) <- c("SA1 Zp-", "SA1 Zp+", "SA2 Zp-", "SA2 Zp+", "SA3 Zp-", "SA3 Zp+", "SA4 Zp-", "SA4 Zp+")
colnames(yrSAtotNEG) <- paste(1980:2018, "Neg Cite.")
colnames(yrSAtotPOS) <- paste(1980:2018, "Pos Cite.")



SApercTotNEG <- matrix(data=NA, nrow = length(cats), ncol = length(years))
SApercTotPOS <- matrix(data=NA, nrow = length(cats), ncol = length(years))



for (i in 1:length(years)){
  for (j in 1:length(cats) ) {
    SApercTotNEG[j,i] <- yrSAcat[((j-1)*2+1),i]/yrSAtotNEG[1,i]
    SApercTotPOS[j,i] <- yrSAcat[(j*2),i]/yrSAtotPOS[1,i]
  }
}



colnames(SApercTot) <- c(1980:2018)
rownames(SApercTotNEG) <- c("SA1 Zp- ", "SA2 Zp- ", "SA3 Zp- ", "SA4 Zp- ")
rownames(SApercTotPOS) <- c("SA1 Zp+ ", "SA2 Zp+ ", "SA3 Zp+ ", "SA4 Zp+ ")


######################################
# FIGURE 2A - 2nd Graph CIP Categories
######################################

# Specify years and categories to analyze
years <- 1980:2018
cats <- 1:4

# Store CIP values of all categories from CSV data
CIPp<-dataFromCSV %>%
  group_by(Yp, NCIPp, Zp>0) %>%
  summarise(
    cnt = n()
  ) %>%
  filter(
    Yp>1979
  )

# Create matrix to store CIP papers in specified categories by years
yrCIPcat <- matrix(data=NA,nrow= length(cats)*2, ncol=length(years) )

# Create vectors to store total CIP papers with below average citations in specified categories by years
yrCIPtotNEG <- matrix(data=NA, nrow = 1, ncol = length(years))
# Create vectors to store total CIP papers with above average citations in specified categories by years
yrCIPtotPOS <- matrix(data=NA, nrow = 1, ncol = length(years))

# create negative & positive arrays to store values from yrCIPcat
negarr <- (cats-1)2+1
posarr <- (cats)*2

zer <- matrix(data = 0, nrow = 8, ncol = 4)
tt2 <- tibble(Yp = as.integer(Yp = zer[1:8]), NCIPp=as.integer(0), 'Zp > 0'=FALSE, cnt= as.integer(0))

for (i in 1:length(years)){
  tt <- filter(CIPp, Yp == (i+1979) )
  if (dim(tt)[1] < 8){
    tt2 <- tibble(Yp = as.integer(zer[(8-dim(tt)[1]):8]), NCIPp=as.integer(0), 'Zp > 0'=FALSE, cnt= as.integer(0))
    tt <- tt %>% full_join(tt2,by=NULL)
  }
  for (j in 1:dim(tt)[1]){
    if (j>8){
      next
    }
    
    yrCIPcat[j,i] <- tt[[j,4]]
    
  } 
  yrCIPtotNEG[1,i] <- sum(yrCIPcat[negarr,i])
  yrCIPtotPOS[1,i] <- sum(yrCIPcat[posarr,i])
}
colnames(yrCIPcat) <- c(1980:2018)
rownames(yrCIPcat) <- c("CIP1 Zp-", "CIP1 Zp+", "CIP2 Zp-", "CIP2 Zp+", "CIP3 Zp-", "CIP3 Zp+", "CIP4 Zp-", "CIP4 Zp+")
colnames(yrCIPtotNEG) <- paste(1980:2018, "Neg Cite.")
colnames(yrCIPtotPOS) <- paste(1980:2018, "Pos Cite.")

CIPpercTotNEG <- matrix(data=NA, nrow = length(cats), ncol = length(years))
CIPpercTotPOS <- matrix(data=NA, nrow = length(cats), ncol = length(years))

for (i in 1:length(years)){
  for (j in 1:length(cats) ) {
    CIPpercTotNEG[j,i] <- yrCIPcat[((j-1)*2+1),i]/yrCIPtotNEG[1,i]
    CIPpercTotPOS[j,i] <- yrCIPcat[(j*2),i]/yrCIPtotPOS[1,i]
  }
}

colnames(CIPpercTotNEG) <- c(1980:2018)
colnames(CIPpercTotPOS) <- c(1980:2018)

rownames(CIPpercTotNEG) <- c("CIP1 Zp- %", "CIP2 Zp- %", "CIP3 Zp- %", "CIP4 Zp- %")
rownames(CIPpercTotPOS) <- c("CIP1 Zp+ %", "CIP2 Zp+ %", "CIP3 Zp+ %", "CIP4 Zp+ %")


############################################
# FIGURE 2A - 3rd Graph SA & CIP Categories
############################################

# Specify years and categories to analyze
years <- 1980:2018
cats <- 1:4

SA_CIP1 <- dataFromCSV %>%
  group_by(Yp, NSAp, NCIPp, Zp>0) %>%
  summarise(
    cnt = n()
  ) %>%
  filter(NCIPp == 1 &
           NSAp == 1 &
           Yp>1979)


#2 cat
SA_CIP2 <- dataFromCSV %>%
  group_by(Yp, NSAp, NCIPp, Zp>0) %>%
  summarise(
    cnt = n()
  ) %>%
  filter(NCIPp == 2 &
           NSAp == 2 &
           Yp>1979)

SA_CIP3 <- dataFromCSV %>%
  group_by(Yp, NSAp, NCIPp, Zp>0) %>%
  summarise(
    cnt = n()
  ) %>%
  filter(NCIPp == 3 &
           NSAp == 3 &
           Yp>1979)

SA_CIP4 <- dataFromCSV %>%
  group_by(Yp, NSAp, NCIPp, Zp>0) %>%
  summarise(
    cnt = n()
  ) %>%
  filter(NCIPp == 4 &
           NSAp == 4 &
           Yp>1979)

SA_CIP <- SA_CIP1 %>% full_join(SA_CIP2, by = NULL) %>% full_join(SA_CIP3, by = NULL) %>% full_join(SA_CIP4, by = NULL)


# Create matrix to store CIP papers in specified categories by years
yrSA_CIPcat <- matrix(data=NA,nrow= length(cats)*2, ncol=length(years) )


# Create vectors to store total CIP papers with below average citations in specified categories by years
yrSA_CIPtotNEG <- matrix(data=NA, nrow = 1, ncol = length(years))


# Create vectors to store total CIP papers with above average citations in specified categories by years
yrSA_CIPtotPOS <- matrix(data=NA, nrow = 1, ncol = length(years))


# create negative & positive arrays to store values from yrSA_CIPcat
negarr <- (cats-1)*2+1
posarr <- (cats)*2

zer <- matrix(data = 0, nrow = 8, ncol = 4)
tmp1 <- tibble(Yp = as.integer(Yp = zer[1:8]), NCIPp=as.integer(0), 'Zp > 0'=FALSE, cnt= as.integer(0))
for (i in 1:length(years) ){
  
  tmp1 <- filter(SA_CIP, Yp == (i+1979) )
  if (dim(tmp1)[1] < 8){
    tt2 <- tibble(Yp = as.integer(zer[(8-dim(tmp1)[1]):8]), NCIPp=as.integer(0), 'Zp > 0'=FALSE, cnt= as.integer(0))
    tmp1 <- tmp1 %>% full_join(tt2,by=NULL)
  }
  
  for (j in 1:dim(tmp1)[1]){
    if (j>8){
      next
    } 
    
    yrSA_CIPcat[j,i] <- tmp1[[j,5]]
    
  } 
  yrSA_CIPtotNEG[1,i] <- sum(yrSA_CIPcat[negarr,i])
  yrSA_CIPtotPOS[1,i] <- sum(yrSA_CIPcat[posarr,i])
}


colnames(yrSA_CIPcat) <- c(1980:2018)
rownames(yrSA_CIPcat) <- c("SA_CIP1 Zp-", "SA_CIP1 Zp+", "SA_CIP2 Zp-", "SA_CIP2 Zp+", "SA_CIP3 Zp-", "SA_CIP3 Zp+", "SA_CIP4 Zp-", "SA_CIP4 Zp+")
colnames(yrSA_CIPtotNEG) <- paste(1980:2018, "Neg Cite.")
colnames(yrSA_CIPtotPOS) <- paste(1980:2018, "Pos Cite.")

SA_CIPpercTotNEG <- matrix(data=NA, nrow = length(cats), ncol = length(years))
SA_CIPpercTotPOS <- matrix(data=NA, nrow = length(cats), ncol = length(years))

for (i in 1:length(years)){
  for (j in 1:length(cats) ) {
    SA_CIPpercTotNEG[j,i] <- yrSA_CIPcat[((j-1)*2+1),i]/yrSA_CIPtotNEG[1,i]
    SA_CIPpercTotPOS[j,i] <- yrSA_CIPcat[(j*2),i]/yrSA_CIPtotPOS[1,i]
  }
}

colnames(SA_CIPpercTotNEG) <- c(1980:2018)
colnames(SA_CIPpercTotPOS) <- c(1980:2018)

rownames(SA_CIPpercTotNEG) <- c("SA_CIP1 Zp- %", "SA_CIP2 Zp- %", "SA_CIP3 Zp- %", "SA_CIP4 Zp- %")
rownames(SA_CIPpercTotPOS) <- c("SA_CIP1 Zp+ %", "SA_CIP2 Zp+ %", "SA_CIP3 Zp+ %", "SA_CIP4 Zp+ %")


SA_CIPpercTotPOS
years <- 1980:2018

#PLOTTING for SA

p1 = data.frame(years=years, Fraction = (SApercTotPOS[1,]))
p2 = data.frame(years=years, Fraction = (SApercTotPOS[2,]))
p3 = data.frame(years=years, Fraction = SApercTotPOS[3,])
p4 = data.frame(years=years, Fraction = (SApercTotPOS[4,]))


n1 = data.frame(years=years, Fraction = (SApercTotNEG[1,]))
n2 = data.frame(years=years, Fraction = (SApercTotNEG[2,]))
n3 = data.frame(years=years, Fraction = SApercTotNEG[3,])
n4 = data.frame(years=years, Fraction = (SApercTotNEG[4,]))

e1<-ggplot()+geom_line(data=p1, mapping = aes(x=years,y=Fraction), size=0.5, color="grey30") + geom_line(data=p2, mapping = aes(x=years,y=Fraction), size=0.5, color="grey69") +
             geom_line(data=p3, mapping = aes(x=years,y=Fraction), size=0.5, color="royalblue") + geom_line(data=p4, mapping = aes(x=years,y=Fraction), size=0.5, color="blue") +
             geom_line(data=n1, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="grey30") + geom_line(data=n2, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="grey69") +
             geom_line(data=n3, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="royalblue") + geom_line(data=n4, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="blue")
  
e2<-e1+theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                      axis.text.x   = element_text(size=10)) 
e3<-e2+scale_x_continuous(limits=c(1980, 2020), breaks=seq(1980, 2020, 5))+scale_y_continuous(limits=c(0.0, 0.6), breaks=seq(0.0, 0.6, 0.2))


e4<-e3+labs(title = "", x = "", y = "")


e5<-e4+ scale_color_manual(name = "Y series")


#Plotting for CIP




cp1 = data.frame(years=years, Fraction = (CIPpercTotPOS[1,]))
cp2 = data.frame(years=years, Fraction = (CIPpercTotPOS[2,]))
cp3 = data.frame(years=years, Fraction = (CIPpercTotPOS[3,]))
cp4 = data.frame(years=years, Fraction = (CIPpercTotPOS[4,]))


cn1 = data.frame(years=years, Fraction = (CIPpercTotNEG[1,]))
cn2 = data.frame(years=years, Fraction = (CIPpercTotNEG[2,]))
cn3 = data.frame(years=years, Fraction = (CIPpercTotNEG[3,]))
cn4 = data.frame(years=years, Fraction = (CIPpercTotNEG[4,]))

ce1<-ggplot()+geom_line(data=cp1, mapping = aes(x=years,y=Fraction), size=0.5, color="grey30") + geom_line(data=cp2, mapping = aes(x=years,y=Fraction), size=0.5, color="grey69") +
  geom_line(data=cp3, mapping = aes(x=years,y=Fraction), size=0.5, color="royalblue") + geom_line(data=cp4, mapping = aes(x=years,y=Fraction), size=0.5, color="blue") +
  geom_line(data=cn1, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="grey30") + geom_line(data=cn2, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="grey69") +
  geom_line(data=cn3, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="royalblue") + geom_line(data=cn4, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="blue")

ce2<-ce1+theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                          axis.text.x   = element_text(size=10)) 
ce3<-ce2+scale_x_continuous(limits=c(1980, 2020), breaks=seq(1980, 2020, 5))+scale_y_continuous(limits=c(0.0, 1.0), breaks=seq(0.0, 1.0, 0.2))


ce4<-ce3+labs(title = "", x = "", y = "")

ce4


#Plotting for SACIP


scp1 = data.frame(years=years, Fraction = (SA_CIPpercTotPOS[1,]))
scp2 = data.frame(years=years, Fraction = (SA_CIPpercTotPOS[2,]))
scp3 = data.frame(years=years, Fraction = SA_CIPpercTotPOS[3,])
scp4 = data.frame(years=years, Fraction = (SA_CIPpercTotPOS[4,]))


scn1 = data.frame(years=years, Fraction = (SA_CIPpercTotNEG[1,]))
scn2 = data.frame(years=years, Fraction = (SA_CIPpercTotNEG[2,]))
scn3 = data.frame(years=years, Fraction = SA_CIPpercTotNEG[3,])
scn4 = data.frame(years=years, Fraction = (SA_CIPpercTotNEG[4,]))

sce1<-ggplot()+geom_line(data=scp1, mapping = aes(x=years,y=Fraction), size=0.5, color="grey30") + geom_line(data=scp2, mapping = aes(x=years,y=Fraction), size=0.5, color="grey69") +
  geom_line(data=scp3, mapping = aes(x=years,y=Fraction), size=1, color="royalblue") + geom_line(data=scp4, mapping = aes(x=years,y=Fraction), size=0.5, color="blue") +
  geom_line(data=scn1, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="grey30") + geom_line(data=scn2, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="grey69") +
  geom_line(data=scn3, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="royalblue") + geom_line(data=scn4, mapping = aes(x=years,y=Fraction), linetype = "dashed", size=0.5, color="blue")

sce2<-ce1+theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                          axis.text.x   = element_text(size=10)) 
sce3<-ce2+scale_x_continuous(limits=c(1980, 2020), breaks=seq(1980, 2020, 5))+scale_y_continuous(limits=c(0.0, 1.0), breaks=seq(0.0, 1.0, 0.2))


sce4<-ce3+labs(title = "", x = "", y = "")


#e5<-e4+ scale_color_manual(name = "Y series")
#par(mfrow=c(1,3))
#e5
#ce4
#Fraction of articles featuring cross-domain combinations,f#(t|z)
#plot_grid(e5, ce4,ce4, align = "h", nrow = 3, rel_heights = c(1/2, 1/2, 1/2),label_y ="" )
grid.arrange(arrangeGrob(e5 + theme(legend.position="none"), 
                         ce4 + theme(legend.position="none"),
                         sce4 + theme(legend.position="none"),
                         nrow = 3,
                         top = textGrob("#Categories", vjust = 1, gp = gpar(fontface = "bold", cex = 1)),
                         left = textGrob("Fraction of articles featuring cross-domain combinations,f#(t|z)", rot = 90, vjust = 1),
                         right = textGrob("Cross-Topic(SA)       Cross-Discipline(CIP)           Cross - SA&CIP", rot = 90)))

