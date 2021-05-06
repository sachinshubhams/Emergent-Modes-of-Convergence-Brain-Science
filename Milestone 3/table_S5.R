library(dplyr)
library(ggplot2)
library(rcompanion)
library(lmtest)
library(sandwich)
library(plm)
library(RTextTools)
library(tidyverse)
library(fastDummies)

# Read data
setwd("D:/Statistical Methods/Project")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
author_data = read_data('AuthorArticleLevel-RegData-ALLSA_Xc_1_NData_864590_LONGXCIP2.txt')
options(digits=4)

#Convert the Data types
author_data$Yp = as.integer(author_data$Yp)
author_data$Kp = as.integer(author_data$Kp)
author_data$MeanZJp = as.double(author_data$MeanZJp)
author_data$NRegp = as.integer(author_data$NRegp)
author_data$NSAp = as.integer(author_data$NSAp)
author_data$NCIPp = as.integer(author_data$NCIPp)
author_data$nMeSHMain = as.integer(author_data$nMeSHMain)
author_data$Tauip = as.integer(author_data$Tauip)
author_data$Zp = as.double(author_data$Zp)
author_data$logKp = as.double(log(author_data$Kp))
author_data$logMajorMeSHp = as.double(log(author_data$nMeSHMain))

# Filter Year [1970-2018] and Filter Kp >= 2, NPubsnAuthorIDi >= 10 and nMeSHMain >= 2
author_data = author_data %>% filter(NPubsnAuthorIDi >= 10) %>% 
  filter(Kp >= 2) %>% filter(nMeSHMain >= 2) %>% 
  filter(Yp >= 1970 & Yp <= 2018)
author_data$I_year <- ifelse(author_data$Yp >= 2014,1,0)


author_data_2 <- dummy_cols(author_data, select_columns = c('Yp'),   remove_selected_columns = TRUE)
author_data_2 <- dummy_cols(author_data_2, select_columns = c('IRegionRefinedp'),   remove_selected_columns = TRUE)

#getting columns names
#model1
author_data_2$XSACIPp <- ifelse((author_data_2$XSAp==1) & (author_data_2$XCIPp == 1),1,0)
author_data_2$XSACIPp <- as.numeric(author_data_2$XSACIPp)
author_data_2_XSACIP <- author_data_2 %>% filter((XSAp == 1 & XCIPp == 1)|(XSAp == 0 & XCIPp == 0))

#model
model1 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + I_year +  XSACIPp +(I_year*XSACIPp) +
                SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                Yp_1970 +Yp_1971+Yp_1972+Yp_1973+Yp_1974+
                Yp_1975+Yp_1976+Yp_1977+Yp_1978+Yp_1979+Yp_1980+          
                Yp_1981+ Yp_1982+Yp_1983+Yp_1984+Yp_1985+ Yp_1986+          
                Yp_1987+Yp_1988+Yp_1989+Yp_1990+Yp_1991+Yp_1992+Yp_1993+
                Yp_1994+Yp_1995+Yp_1996+Yp_1997+Yp_1998+Yp_1999+Yp_2000+
                Yp_2001+Yp_2002+Yp_2003+Yp_2004+Yp_2005+Yp_2006+Yp_2007+
                Yp_2008+Yp_2009+Yp_2010+Yp_2011+Yp_2012+Yp_2013+Yp_2014+Yp_2015+
                Yp_2016 +Yp_2017+Yp_2018+IRegionRefinedp_0+IRegionRefinedp_1
              +IRegionRefinedp_2+IRegionRefinedp_3+IRegionRefinedp_4+IRegionRefinedp_5+IRegionRefinedp_6+IRegionRefinedp_7, 
              data = author_data_2_XSACIP , model = "within", index="nAuthorID")

#summary
output1 <- summary(model1, robust=TRUE)

#intercept
intse1 <- within_intercept(model1)
intercept1 <- intse1[1]
intercept1 <- format(as.list(intercept1), digits=4, scientific=FALSE)
intse_ch1 <- capture.output(intse1)
se_dirty <- intse_ch1[4]
se <- substring(se_dirty,5)

final_se<- paste("(",se,")",sep="")

cons1 <- paste(intercept1,final_se,sep=" ")

# robust standard error
SE1 <- output1$coefficients[(1:5),2]
SE1 <-c(SE1,output1$coefficients['I_year:XSACIPp',2])
SE1 <- format(as.list(SE1), digits=4, scientific=FALSE)

SE1 <- paste("(",SE1,")",sep="")

#coefficients
coef1 <- output1$coefficients[(1:5),1]
coef1 <- c(coef1,output1$coefficients['I_year:XSACIPp',1])
coef1 <- format(as.list(coef1), digits=4, scientific=FALSE)

#significance
star <- ifelse(as.numeric(output1$coefficients[(1:5),4])<0.05,"*"," ")
star <- ifelse(as.numeric(output1$coefficients[(1:5),4])<0.01,"**",star)
star <- ifelse(as.numeric(output1$coefficients[(1:5),4])<0.001,"***",star)

star2 <- ifelse(as.numeric(output1$coefficients['I_year:XSACIPp',4])<0.05,"*"," ")
star2 <- ifelse(as.numeric(output1$coefficients['I_year:XSACIPp',4])<0.01,"**",star2)
star2 <- ifelse(as.numeric(output1$coefficients['I_year:XSACIPp',4])<0.001,"***",star2)

star <- c(star,star2)

#merging coeffcuintes values
coef1 <- paste(coef1,star,sep="")
coefSe1 <- paste(coef1,SE1,sep=" ")

#stat
N1 <- nobs(model1)
adjR1 <- parse_number(as.character(output1$r.squared[2]))
adjR1 <- format(as.list(adjR1), digits=4, scientific=FALSE)

F1 <- parse_number(as.character(output1$fstatistic$statistic))
F1 <- format(as.list(F1), digits=4, scientific=FALSE)

researchers <- length(unique(author_data_2_XSACIP$nAuthorID))

#assemble
model1_full<- c(coefSe1 ,"","","","",cons1,"Y","Y","Y","Y",N1,adjR1,F1,researchers)

#model2
author_data_2$neighborSA <- ifelse(author_data_2$SA1>=1 & (author_data_2$SA2>=1 | author_data_2$SA3>=1 |author_data_2$SA4>=1),1,0)
author_data_2$neighborCIP <- ifelse((author_data_2$CIP1>=1 |author_data_2$CIP3>=1 ) & (author_data_2$CIP2>=1 | author_data_2$CIP4>=1 |author_data_2$CIP5>=1|author_data_2$CIP6>=1|author_data_2$CIP7>=1),1,0)
author_data_2$neighborSACIP <- ifelse((author_data_2$neighborSA==1) & (author_data_2$neighborCIP == 1),1,0)
author_data_2$neighborSACIP<- as.numeric(author_data_2$neighborSACIP)
author_data_2_neighborSACIP <- author_data_2 %>% filter((neighborSA == 1 & neighborCIP == 1)|(neighborSA == 0 & neighborCIP == 0))

#model
model2 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + I_year +  neighborSACIP +(I_year*neighborSACIP) +
                SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                Yp_1970 +Yp_1971+Yp_1972+Yp_1973+Yp_1974+
                Yp_1975+Yp_1976+Yp_1977+Yp_1978+Yp_1979+Yp_1980+          
                Yp_1981+ Yp_1982+Yp_1983+Yp_1984+Yp_1985+ Yp_1986+          
                Yp_1987+Yp_1988+Yp_1989+Yp_1990+Yp_1991+Yp_1992+Yp_1993+
                Yp_1994+Yp_1995+Yp_1996+Yp_1997+Yp_1998+Yp_1999+Yp_2000+
                Yp_2001+Yp_2002+Yp_2003+Yp_2004+Yp_2005+Yp_2006+Yp_2007+
                Yp_2008+Yp_2009+Yp_2010+Yp_2011+Yp_2012+Yp_2013+Yp_2014+Yp_2015+
                Yp_2016 +Yp_2017+Yp_2018+IRegionRefinedp_0+IRegionRefinedp_1
              +IRegionRefinedp_2+IRegionRefinedp_3+IRegionRefinedp_4+IRegionRefinedp_5+IRegionRefinedp_6+IRegionRefinedp_7, 
              data = author_data_2_neighborSACIP , model = "within", index="nAuthorID")
#summary
output2 <- summary(model2, robust=TRUE)

#intercept
intse1 <- within_intercept(model2)
intercept1 <- intse1[1]
intercept1 <- format(as.list(intercept1), digits=4, scientific=FALSE)
intse_ch1 <- capture.output(intse1)
se_dirty <- intse_ch1[4]
se <- substring(se_dirty,5)

final_se<- paste("(",se,")",sep="")

cons1 <- paste(intercept1,final_se,sep=" ")

# robust standard error
SE1 <- output2$coefficients[(1:5),2]
SE1 <-c(SE1,output2$coefficients['I_year:neighborSACIP',2])
SE1 <- format(as.list(SE1), digits=4, scientific=FALSE)

SE1 <- paste("(",SE1,")",sep="")


#coefficients
coef1 <- output2$coefficients[(1:5),1]
coef1 <- c(coef1,output2$coefficients['I_year:neighborSACIP',1])
coef1 <- format(as.list(coef1), digits=4, scientific=FALSE)

#significance
star <- ifelse(as.numeric(output2$coefficients[(1:5),4])<0.05,"*"," ")
star <- ifelse(as.numeric(output2$coefficients[(1:5),4])<0.01,"**",star)
star <- ifelse(as.numeric(output2$coefficients[(1:5),4])<0.001,"***",star)

star2 <- ifelse(as.numeric(output2$coefficients['I_year:neighborSACIP',4])<0.05,"*"," ")
star2 <- ifelse(as.numeric(output2$coefficients['I_year:neighborSACIP',4])<0.01,"**",star2)
star2 <- ifelse(as.numeric(output2$coefficients['I_year:neighborSACIP',4])<0.001,"***",star2)

star <- c(star,star2)

#merging coeffs values
coef1 <- paste(coef1,star,sep="")
coefSe1 <- paste(coef1,SE1,sep=" ")

#stat
N1 <- nobs(model2)
adjR1 <- parse_number(as.character(output2$r.squared[2]))
adjR1 <- format(as.list(adjR1), digits=4, scientific=FALSE)

F1 <- parse_number(as.character(output2$fstatistic$statistic))
F1 <- format(as.list(F1), digits=4, scientific=FALSE)

researchers <- length(unique(author_data_2_neighborSACIP$nAuthorID))

#assemble data
model2_full<- c(coefSe1 ,"","","","",cons1,"Y","Y","Y","Y",N1,adjR1,F1,researchers)

#Model3
author_data_2$distantSA <- ifelse((author_data_2$SA1>=1|author_data_2$SA2>=1|author_data_2$SA3>=1|author_data_2$SA4>=1) & (author_data_2$SA5>=1 | author_data_2$SA6>=1 ),1,0)
author_data_2$distantCIP <- ifelse((author_data_2$CIP1>=1|author_data_2$CIP3>=1|author_data_2$CIP5>=1) & (author_data_2$CIP4>=1 | author_data_2$CIP8>=1 ),1,0)
author_data_2$distantSACIP <- ifelse((author_data_2$distantSA==1) & (author_data_2$distantCIP == 1),1,0)
author_data_2$distantSACIP<- as.numeric(author_data_2$distantSACIP)
author_data_2_distantSACIP <- author_data_2 %>% filter((distantSACIP == 1 & distantCIP == 1)|(distantSA == 0 & distantCIP == 0))

#model
model3 <- plm(Zp ~ logKp + logMajorMeSHp + Tauip + I_year +  distantSACIP +(I_year*distantSACIP) +
                SA1 + SA2 + SA3 + SA4 + SA5 + SA6 +
                CIP1 + CIP2 + CIP3+ CIP4+ CIP5+ CIP6+ CIP7+ CIP8+ CIP9+
                Yp_1970 +Yp_1971+Yp_1972+Yp_1973+Yp_1974+
                Yp_1975+Yp_1976+Yp_1977+Yp_1978+Yp_1979+Yp_1980+          
                Yp_1981+ Yp_1982+Yp_1983+Yp_1984+Yp_1985+ Yp_1986+          
                Yp_1987+Yp_1988+Yp_1989+Yp_1990+Yp_1991+Yp_1992+Yp_1993+
                Yp_1994+Yp_1995+Yp_1996+Yp_1997+Yp_1998+Yp_1999+Yp_2000+
                Yp_2001+Yp_2002+Yp_2003+Yp_2004+Yp_2005+Yp_2006+Yp_2007+
                Yp_2008+Yp_2009+Yp_2010+Yp_2011+Yp_2012+Yp_2013+Yp_2014+Yp_2015+
                Yp_2016 +Yp_2017+Yp_2018+IRegionRefinedp_0+IRegionRefinedp_1
              +IRegionRefinedp_2+IRegionRefinedp_3+IRegionRefinedp_4+IRegionRefinedp_5+IRegionRefinedp_6+IRegionRefinedp_7, 
              data = author_data_2_distantSACIP , model = "within", index="nAuthorID")
#summary
output3 <- summary(model3, robust=TRUE)

#intercept
intse1 <- within_intercept(model3)
intercept1 <- intse1[1]
intercept1 <- format(as.list(intercept1), digits=4, scientific=FALSE)
intse_ch1 <- capture.output(intse1)
se_dirty <- intse_ch1[4]
se <- substring(se_dirty,5)

final_se<- paste("(",se,")",sep="")

cons1 <- paste(intercept1,final_se,sep=" ")

#robust standard error
SE1 <- output3$coefficients[(1:5),2]
SE1 <-c(SE1,output3$coefficients['I_year:distantSACIP',2])
SE1 <- format(as.list(SE1), digits=4, scientific=FALSE)

SE1 <- paste("(",SE1,")",sep="")


#coefficients
coef1 <- output3$coefficients[(1:5),1]
coef1 <- c(coef1,output3$coefficients['I_year:distantSACIP',1])
coef1 <- format(as.list(coef1), digits=4, scientific=FALSE)

#significance
star <- ifelse(as.numeric(output3$coefficients[(1:5),4])<0.05,"*"," ")
star <- ifelse(as.numeric(output3$coefficients[(1:5),4])<0.01,"**",star)
star <- ifelse(as.numeric(output3$coefficients[(1:5),4])<0.001,"***",star)

star2 <- ifelse(as.numeric(output3$coefficients['I_year:distantSACIP',4])<0.05,"*"," ")
star2 <- ifelse(as.numeric(output3$coefficients['I_year:distantSACIP',4])<0.01,"**",star2)
star2 <- ifelse(as.numeric(output3$coefficients['I_year:distantSACIP',4])<0.001,"***",star2)

star <- c(star,star2)

#merging coefficients values
coef1 <- paste(coef1,star,sep="")
coefSe1 <- paste(coef1,SE1,sep=" ")

#stat
N1 <- nobs(model3)
adjR1 <- parse_number(as.character(output3$r.squared[2]))
adjR1 <- format(as.list(adjR1), digits=4, scientific=FALSE)

F1 <- parse_number(as.character(output3$fstatistic$statistic))
F1 <- format(as.list(F1), digits=4, scientific=FALSE)

researchers <- length(unique(author_data_2_distantSACIP$nAuthorID))

#assemble the data
model3_full<- c(coefSe1 ,"","","","",cons1,"Y","Y","Y","Y",N1,adjR1,F1,researchers)

#arranging vectors
model2_full[7]<-model2_full[5]
model2_full[8]<-model2_full[6]
model2_full[5] <- ""
model2_full[6] <- ""

model3_full[9]<-model3_full[5]
model3_full[10]<-model3_full[6]
model3_full[5] <- ""
model3_full[6] <- ""

#assemble data frame
S5 <- data.frame(model1_full,model2_full,model3_full,
                          row.names = c("ln k","ln w","t","$I_{2014+}$","$I_{X_{SA\\&CIP}}$",
                                        "$I_{X_{SA\\&CIP}}\\times I_{2014+}$","$I_{X_{Neighboring,SA\\&CIP}}$",
                                        "$I_{X_{Neighboring,SA\\&CIP}}\\times I_{2014+}$","$I_{X_{Distant,SA\\&CIP}}$",
                                        "$I_{X_{Distant,SA\\&CIP}}\\times I_{2014+} $",
                                        "constant","year dummy","topic category dummy",
                                        "department category dummy","Region dummy","N","adj.$R^2$","F","\\# researcher profiles"))

#write data into CSV file
write.csv(S5,"D:/Statistical Methods/Milestone III/S5.csv")
