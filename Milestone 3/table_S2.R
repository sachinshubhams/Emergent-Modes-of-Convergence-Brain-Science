library(dplyr)
library(ggplot2)
library(rcompanion)
library(rms)
library(questionr)
library(readr)

# Read data
setwd("D:/Statistical Methods/Project")
article_Data <- read.csv('ArticleLevel-RegData-ALLSA_Xc_1_NData_655386_LONGXCIP2.csv')

# Filter Year [1970-2018] and Filter Kp >= 2 and Wp >= 2
article_Data = article_Data %>% filter(Yp >= 1970)
article_Data = article_Data %>% filter(Yp <= 2018)
article_Data = article_Data %>% filter(Kp >= 2)
article_Data = article_Data %>% filter(nMeSHMain >= 2)
article_Data = article_Data %>% filter(IRegionRefinedp > 0 & IRegionRefinedp < 7)

#Convert the Data types
article_Data$eidsp = as.factor(article_Data$eidsp)
article_Data$Yp = as.integer(article_Data$Yp)
article_Data$Kp = as.integer(article_Data$Kp)
article_Data$MeanZJp = as.double(article_Data$MeanZJp)
article_Data$XSAp = as.factor(article_Data$XSAp)
article_Data$XCIPp = as.factor(article_Data$XCIPp)
article_Data$NRegp = as.integer(article_Data$NRegp)
article_Data$NSAp = as.integer(article_Data$NSAp)
article_Data$NCIPp = as.integer(article_Data$NCIPp)
article_Data$nMeSHMain = as.integer(article_Data$nMeSHMain)
article_Data$IRegionRefinedp = as.factor(article_Data$IRegionRefinedp)

#Model1_ XnSA
article_Data$neighborSA <- ifelse(article_Data$SA1>=1 & (article_Data$SA2>=1 | article_Data$SA3>=1 |article_Data$SA4>=1),1,0)

#model
model1 <- glm(neighborSA  ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp + NCIPp, 
              data = article_Data, family=binomial(link='logit'))
#summary
summary(model1)

#calculating r sqaure  
nmodel1 <- nagelkerke(model1)

#coefficients
output = odds.ratio(model1) # HEAVY COMPUTATIONAL!
output = apply(output, 2, formatC, format="f", digits=4)
output

#get R sqaure from nmodel1
S2v1_XSA <- capture.output(nmodel1$Pseudo.R.squared.for.model.vs.null)
S2R_XSA <- parse_number(S2v1_XSA[2])

#get coefficients from output
S2v2_XSA <- capture.output(output)
S2y_XSA  <- parse_number(S2v2_XSA[3])
S2z_XSA  <- parse_number(S2v2_XSA[4])
S2k_XSA  <- parse_number(S2v2_XSA[5])
S2w_XSA  <- parse_number(S2v2_XSA[6])
S2r_XSA  <- parse_number(S2v2_XSA[7])
S2ncip_XSA  <- parse_number(S2v2_XSA[8])
S2nsa_XSA  <- NA

#robust SD
SD <- as.character(format((as.numeric(output[2,3])-as.numeric(output[2,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
star <- ifelse(as.numeric(output[2,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[2,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[2,4])<0.001,"***",star)
S2y_XSA <- as.character(S2y_XSA)
S2y_XSA <- paste(S2y_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[3,3])-as.numeric(output[3,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[3,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[3,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[3,4])<0.001,"***",star)
S2z_XSA <- as.character(S2z_XSA)
S2z_XSA <- paste(S2z_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[4,3])-as.numeric(output[4,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[4,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[4,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[4,4])<0.001,"***",star)
S2k_XSA <- as.character(S2k_XSA)
S2k_XSA <- paste(S2k_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[5,3])-as.numeric(output[5,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[5,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[5,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[5,4])<0.001,"***",star)
S2w_XSA <- as.character(S2w_XSA)
S2w_XSA <- paste(S2w_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[6,3])-as.numeric(output[6,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[6,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[6,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[6,4])<0.001,"***",star)
S2r_XSA <- as.character(S2r_XSA)
S2r_XSA <- paste(S2r_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[7,3])-as.numeric(output[7,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[7,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[7,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[7,4])<0.001,"***",star)
S2ncip_XSA <- as.character(S2ncip_XSA)
S2ncip_XSA <- paste(S2ncip_XSA,star,SD,sep=" ")


#get the value of  N
S2v3_XSA <- capture.output(nmodel1$Number.of.observations)
S2N_XSA <- parse_number(S2v3_XSA[2])

#vector for coefficients of the model
m1coef <- c(S2y_XSA ,S2z_XSA, S2k_XSA, S2w_XSA , S2r_XSA, S2ncip_XSA, S2nsa_XSA)

#vector for N and R square
m1stat <- c(S2N_XSA,S2R_XSA)

#Model 2 - for X_CIP
options(scipen=2)
article_Data$neighborCIP <- ifelse((article_Data$CIP1>=1 |article_Data$CIP3>=1 ) & (article_Data$CIP2>=1 | article_Data$CIP4>=1 |article_Data$CIP5>=1|article_Data$CIP6>=1|article_Data$CIP7>=1),1,0)
#model
model2 <- glm(neighborCIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp + NSAp, 
              data = article_Data, family=binomial(link='logit'))
#summary
summary(model2)

#calculating r sqaure
nmodel2 <- nagelkerke(model2)

#coefficients
output2 = odds.ratio(model2) # HEAVY COMPUTATIONAL!
output2= apply(output2, 2, formatC, format="f", digits=4)
output2

#get the r square from nmodel2
S2v1_XCIP <- capture.output(nmodel2$Pseudo.R.squared.for.model.vs.null)
S2R_XCIP <- parse_number(S2v1_XCIP[2])

#get coefficients from output
S2v2_XCIP <- capture.output(output2)
S2y_XCIP  <- parse_number(S2v2_XCIP[3])
S2z_XCIP  <- parse_number(S2v2_XCIP[4])
S2k_XCIP  <- parse_number(S2v2_XCIP[5])
S2w_XCIP  <- parse_number(S2v2_XCIP[6])
S2r_XCIP  <- parse_number(S2v2_XCIP[7])
S2nsa_XCIP  <- parse_number(S2v2_XCIP[8])
S2ncip_XCIP  <- NA

S2y_XSA <- as.character(S2y_XSA)
S2y_XSA <- paste(S2y_XSA,star,SD,sep=" ")

S2v3_XCIP <- capture.output(nmodel2$Number.of.observations)
S2N_XCIP <- parse_number(S2v3_XCIP[2])
m2coef <- c(S2y_XCIP ,S2z_XCIP, S2k_XCIP, S2w_XCIP , S2r_XCIP, S2ncip_XCIP, S2nsa_XCIP)
m2stat <- c(S2N_XCIP,S2R_XCIP)

#robust SD
SD <- as.character(format((as.numeric(output2[ ,3])-as.numeric(output2[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model2)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # Pull out p-values
mod_summary_stars <- NA                             # Named vector with significance stars
mod_summary_stars[mod_summary_sign < 0.1] <- " "
mod_summary_stars[mod_summary_sign < 0.05] <- "*"
mod_summary_stars[mod_summary_sign < 0.01] <- "**"
mod_summary_stars[mod_summary_sign < 0.001] <- "***"
mod_summary_stars[is.na(mod_summary_stars)] <- " "
names(mod_summary_stars) <- names(mod_summary_sign)
mod_summary_stars  

#prepare SD
SD <- c(SD[2:6],"",SD[7])

#prepare star
stars <- c(mod_summary_stars[2:6],"",mod_summary_stars[7])

#concatenate coeff, SD and star
m2coef <- paste(as.character(m2coef),stars,SD,sep=" ")
m2coef[m2coef == "NA  "] <- ""
m2

#Model 3 - for X_CIP
article_Data$neighborSACIP <- ifelse((article_Data$neighborSA==1) & (article_Data$neighborCIP == 1),1,0)
article_Data$neighborSACIP<- as.numeric(article_Data$neighborSACIP)

df_article_neighborSACIP <- article_Data %>% filter((neighborSA == 1 & neighborCIP == 1)|(neighborSA == 0 & neighborCIP == 0))

options(scipen=2)
#model
model3 <- glm(neighborSACIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp ,
              data = df_article_neighborSACIP , family=binomial(link='logit'))

#summary
summary(model3)

#calculating r sqaure
nmodel3 <- nagelkerke(model3)

#coefficients
output3 = odds.ratio(model3) # HEAVY COMPUTATIONAL!
output3= apply(output3, 2, formatC, format="f", digits=4)
output3

#get the r square from nmodel3
S2v1_XCIPSA <- capture.output(nmodel3$Pseudo.R.squared.for.model.vs.null)
S2R_XCIPSA <- parse_number(S2v1_XCIPSA[2])

#get coefficients from output3
S2v2_XCIPSA <- capture.output(output3)
S2y_XCIPSA  <- parse_number(S2v2_XCIPSA[3])
S2z_XCIPSA  <- parse_number(S2v2_XCIPSA[4])
S2k_XCIPSA  <- parse_number(S2v2_XCIPSA[5])
S2w_XCIPSA  <- parse_number(S2v2_XCIPSA[6])
S2r_XCIPSA  <- parse_number(S2v2_XCIPSA[7])
S2nsa_XCIPSA  <- NA
S2ncip_XCIPSA  <- NA



S2v3_XCIPSA <- capture.output(nmodel3$Number.of.observations)
S2N_XCIPSA <- parse_number(S2v3_XCIPSA[2])
m3coef <- c(S2y_XCIPSA ,S2z_XCIPSA, S2k_XCIPSA, S2w_XCIPSA , S2r_XCIPSA, S2ncip_XCIPSA, S2nsa_XCIPSA)
m3stat <- c(S2N_XCIPSA,S2R_XCIPSA)

#robust SD
SD <- as.character(format((as.numeric(output3[ ,3])-as.numeric(output3[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model3)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # Pull out p-values
mod_summary_stars <- NA                             # Named vector with significance stars
mod_summary_stars[mod_summary_sign < 0.1] <- " "
mod_summary_stars[mod_summary_sign < 0.05] <- "*"
mod_summary_stars[mod_summary_sign < 0.01] <- "**"
mod_summary_stars[mod_summary_sign < 0.001] <- "***"
mod_summary_stars[is.na(mod_summary_stars)] <- " "
names(mod_summary_stars) <- names(mod_summary_sign)
mod_summary_stars  

#prepare SD
SD <- c(SD[2:6],"","")

#prepare star
stars <- c(mod_summary_stars[2:6],"","")

#concatenate coeff, SD and star
m3coef <- paste(as.character(m3coef),stars,SD,sep=" ")
m3coef[m3coef == "NA  "] <- ""

#Model 4 - for X_SA
article_Data$I_year <- ifelse(article_Data$Yp >= 2014,1,0)
article_Data$I_RNA <- ifelse(article_Data$IRegionRefinedp == 1,1,0)
article_Data$I_REU <- ifelse(article_Data$IRegionRefinedp == 2,1,0)
article_Data$I_RAA <- ifelse(article_Data$IRegionRefinedp == 3,1,0)
#model
model4 <- glm(neighborSA ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + 
                NRegp + NCIPp + I_year + I_RNA + I_REU + I_RAA +
                I_RNA*I_year + I_REU *I_year + I_RAA* I_year , 
              data = article_Data, family=binomial(link='logit'))
#summary
summary(model4)

#calculating r sqaure #coefficients #get the r square from nmodel2 #get coefficients from output
nmodel4 <- nagelkerke(model4)

#coefficients
output4 = odds.ratio(model4) # HEAVY COMPUTATIONAL!
output4 = apply(output4, 2, formatC, format="f", digits=4)
output4

#get the r square from nmodel4
S2v1_XSAI <- capture.output(nmodel4$Pseudo.R.squared.for.model.vs.null)
S2R_XSAI <- parse_number(S2v1_XSAI[2])
S2v2_XSAI <- capture.output(output4)
S2y_XSAI  <- parse_number(S2v2_XSAI[3])
S2z_XSAI  <- parse_number(S2v2_XSAI[4])
S2k_XSAI  <- parse_number(S2v2_XSAI[5])
S2w_XSAI  <- parse_number(S2v2_XSAI[6])
S2r_XSAI  <- parse_number(S2v2_XSAI[7])
S2ncip_XSAI  <- parse_number(S2v2_XSAI[8])
S2nsa_XSAI  <- NA
S2Iyear_XSAI <- parse_number(S2v2_XSAI[9])
S2IRNA_XSAI <- parse_number(S2v2_XSAI[10])
S2IREU_XSAI <- parse_number(S2v2_XSAI[11])
S2IRAA_XSAI <- parse_number(S2v2_XSAI[12])
S2IyearRNA_XSAI <- parse_number(S2v2_XSAI[13])
S2IyearREU_XSAI <- parse_number(S2v2_XSAI[14])
S2IyearRAA_XSAI <- parse_number(S2v2_XSAI[15])
S2v3_XSAI <- capture.output(nmodel4$Number.of.observations)
S2N_XSAI <- parse_number(S2v3_XSAI[2])
m4coef <- c(S2y_XSAI ,S2z_XSAI, S2k_XSAI, S2w_XSAI , S2r_XSAI,
            S2ncip_XSAI, S2nsa_XSAI,S2Iyear_XSAI ,S2IRNA_XSAI
            ,S2IREU_XSAI ,S2IRAA_XSAI ,S2IyearRNA_XSAI ,
            S2IyearREU_XSAI ,S2IyearRAA_XSAI)
m4stat <- c(S2N_XSAI,S2R_XSAI)

#robust SD
SD <- as.character(format((as.numeric(output4[ ,3])-as.numeric(output4[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model4)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # Pull out p-values
mod_summary_stars <- NA                             # Named vector with significance stars
mod_summary_stars[mod_summary_sign < 0.1] <- " "
mod_summary_stars[mod_summary_sign < 0.05] <- "*"
mod_summary_stars[mod_summary_sign < 0.01] <- "**"
mod_summary_stars[mod_summary_sign < 0.001] <- "***"
mod_summary_stars[is.na(mod_summary_stars)] <- " "
names(mod_summary_stars) <- names(mod_summary_sign)
mod_summary_stars  

#prepare SD
SD <- c(SD[2:7],"",SD[8:14])

#prepare star
stars <- c(mod_summary_stars[2:7],"",mod_summary_stars[8:14])

#concatenate coeff, SD and Star
m4coef <- paste(as.character(m4coef),stars,SD,sep=" ")
m4coef[m4coef == "NA  "] <- ""

#model 5 for XCIP with I
options(scipen=2)

#model
model5 <- glm(neighborCIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) +
                NRegp + NSAp+ I_year + I_RNA + I_REU + I_RAA +
                I_RNA*I_year + I_REU *I_year + I_RAA* I_year , 
              data = article_Data, family=binomial(link='logit'))
#summary
summary(model5)

#calculating r sqaure
nmodel5 <- nagelkerke(model5)

#coefficients
output5 = odds.ratio(model5) # HEAVY COMPUTATIONAL!
output5= apply(output5, 2, formatC, format="f", digits=4)
output5

#get the r square from nmodel5
S2v1_XCIPI <- capture.output(nmodel5$Pseudo.R.squared.for.model.vs.null)
S2R_XCIPI <- parse_number(S2v1_XCIPI[2])

#get coefficients from output5
S2v2_XCIPI <- capture.output(output5)
S2y_XCIPI  <- parse_number(S2v2_XCIPI[3])
S2z_XCIPI  <- parse_number(S2v2_XCIPI[4])
S2k_XCIPI  <- parse_number(S2v2_XCIPI[5])
S2w_XCIPI  <- parse_number(S2v2_XCIPI[6])
S2r_XCIPI  <- parse_number(S2v2_XCIPI[7])
S2nsa_XCIPI  <- parse_number(S2v2_XCIPI[8])
S2ncip_XCIPI  <- NA
S2Iyear_XCIPI <- parse_number(S2v2_XCIPI[9])
S2IRNA_XCIPI <- parse_number(S2v2_XCIPI[10])
S2IREU_XCIPI <- parse_number(S2v2_XCIPI[11])
S2IRAA_XCIPI <- parse_number(S2v2_XCIPI[12])
S2IyearRNA_XCIPI <- parse_number(S2v2_XCIPI[13])
S2IyearREU_XCIPI <- parse_number(S2v2_XCIPI[14])
S2IyearRAA_XCIPI <- parse_number(S2v2_XCIPI[15])
S2v3_XCIPI <- capture.output(nmodel5$Number.of.observations)
S2N_XCIPI <- parse_number(S2v3_XCIPI[2])
m5coef <- c(S2y_XCIPI ,S2z_XCIPI, S2k_XCIPI, S2w_XCIPI , S2r_XCIPI,
            S2ncip_XCIPI, S2nsa_XCIPI,S2Iyear_XCIPI ,S2IRNA_XCIPI
            ,S2IREU_XCIPI ,S2IRAA_XCIPI ,S2IyearRNA_XCIPI ,
            S2IyearREU_XCIPI ,S2IyearRAA_XCIPI)
m5stat <- c(S2N_XCIPI,S2R_XCIPI)

#robust SD
SD <- as.character(format((as.numeric(output5[ ,3])-as.numeric(output5[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model5)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # Pull out p-values
mod_summary_stars <- NA                             # Named vector with significance stars
mod_summary_stars[mod_summary_sign < 0.1] <- " "
mod_summary_stars[mod_summary_sign < 0.05] <- "*"
mod_summary_stars[mod_summary_sign < 0.01] <- "**"
mod_summary_stars[mod_summary_sign < 0.001] <- "***"
mod_summary_stars[is.na(mod_summary_stars)] <- " "
names(mod_summary_stars) <- names(mod_summary_sign)
mod_summary_stars  

#prepare SD
SD <- c(SD[2:6],"",SD[7:14])
#prepare star
stars <- c(mod_summary_stars[2:6],"",mod_summary_stars[7:14])

#concatenate coeff, SD and Star
m5coef <- paste(as.character(m5coef),stars,SD,sep=" ")
m5coef[m5coef == "NA  "] <- ""

#model 6 for XSACIP with I
df_article_neighborSACIP$I_year <- ifelse(df_article_neighborSACIP$Yp >= 2014,1,0)
df_article_neighborSACIP$I_RNA <- ifelse(df_article_neighborSACIP$IRegionRefinedp == 1,1,0)
df_article_neighborSACIP$I_REU <- ifelse(df_article_neighborSACIP$IRegionRefinedp == 2,1,0)
df_article_neighborSACIP$I_RAA <- ifelse(df_article_neighborSACIP$IRegionRefinedp == 3,1,0)
options(scipen=2)
model6 <- glm(neighborSACIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) +
                NRegp + I_year + I_RNA + I_REU + I_RAA +
                I_RNA *I_year + I_REU *I_year + I_RAA* I_year , 
              data = df_article_neighborSACIP, family=binomial(link='logit'))
#summary
summary(model6)

#calculating r sqaure   
nmodel6 <- nagelkerke(model6)

#coefficients
output6 = odds.ratio(model6) # HEAVY COMPUTATIONAL!
output6= apply(output6, 2, formatC, format="f", digits=4)
output6

#get the r square from nmodel6
S2v1_XSACIPI <- capture.output(nmodel6$Pseudo.R.squared.for.model.vs.null)
S2R_XSACIPI <- parse_number(S2v1_XSACIPI[2])

#get coefficients from output6
S2v2_XSACIPI <- capture.output(output6)
S2y_XSACIPI  <- parse_number(S2v2_XSACIPI[3])
S2z_XSACIPI  <- parse_number(S2v2_XSACIPI[4])
S2k_XSACIPI  <- parse_number(S2v2_XSACIPI[5])
S2w_XSACIPI  <- parse_number(S2v2_XSACIPI[6])
S2r_XSACIPI  <- parse_number(S2v2_XSACIPI[7])
S2nsa_XSACIPI  <- NA
S2ncip_XSACIPI  <- NA
S2Iyear_XSACIPI <- parse_number(S2v2_XSACIPI[8])
S2IRNA_XSACIPI <- parse_number(S2v2_XSACIPI[9])
S2IREU_XSACIPI <- parse_number(S2v2_XSACIPI[10])
S2IRAA_XSACIPI <- parse_number(S2v2_XSACIPI[11])
S2IyearRNA_XSACIPI <- parse_number(S2v2_XSACIPI[12])
S2IyearREU_XSACIPI <- parse_number(S2v2_XSACIPI[13])
S2IyearRAA_XSACIPI <- parse_number(S2v2_XSACIPI[14])
S2v3_XSACIPI <- capture.output(nmodel6$Number.of.observations)
S2N_XSACIPI <- parse_number(S2v3_XSACIPI[2])
m6coef <- c(S2y_XSACIPI ,S2z_XSACIPI, S2k_XSACIPI, S2w_XSACIPI , S2r_XSACIPI,
            S2ncip_XSACIPI, S2nsa_XSACIPI,S2Iyear_XSACIPI ,S2IRNA_XSACIPI
            ,S2IREU_XSACIPI ,S2IRAA_XSACIPI ,S2IyearRNA_XSACIPI ,
            S2IyearREU_XSACIPI ,S2IyearRAA_XSACIPI)
m6stat <- c(S2N_XSACIPI,S2R_XSACIPI)

#robust SD
SD <- as.character(format((as.numeric(output6[ ,3])-as.numeric(output6[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model6)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # Pull out p-values
mod_summary_stars <- NA                             # Named vector with significance stars
mod_summary_stars[mod_summary_sign < 0.1] <- " "
mod_summary_stars[mod_summary_sign < 0.05] <- "*"
mod_summary_stars[mod_summary_sign < 0.01] <- "**"
mod_summary_stars[mod_summary_sign < 0.001] <- "***"
mod_summary_stars[is.na(mod_summary_stars)] <- " "
names(mod_summary_stars) <- names(mod_summary_sign)
mod_summary_stars  

#prepare SD
SD <- c(SD[2:6],"","",SD[7:13])
#prepare star
stars <- c(mod_summary_stars[2:6],"","",mod_summary_stars[7:13])

#concatenate coeff, SD and Star
m6coef <- paste(as.character(m6coef),stars,SD,sep=" ")
m6coef[m6coef == "NA  "] <- ""

#Gathering Data

#nos of rows
n<-length(m6coef) 

#filling empty rows with NA in the first columns
length(m1coef) <- n
length(m2coef) <- n
length(m3coef) <- n

#coef dataframe
S2_df <- data.frame(m1coef,m2coef,m3coef,m4coef,m5coef,m6coef,row.names = c("y","$\\bar{z_j}$","ln k","ln w","$N_R$","$N_{CIP}$","$N_{SA}$","$I_{2014+}$","$I_{R_{NA}}$","$I_{R_{EU}}$","$I_{R_{AA}}$","$I_{R_{NA}} \\times I_{2014+}$","$I_{R_{EU}} \\times  I_{2014+}$","$I_{R_{AA}} \\times  I_{2014+}$"))

#rename columns 
colnames(S2_df) <- c("m1coef"="XSA","m2coef"="XCIP","m3coef"=" XSACIP","m4coef"="XSA","m5coef"="XCIP","m6coef"="XSA&CIP")

#stat dataframe
S2_df_stat <- data.frame(m1stat,m2stat,m3stat,m4stat,m5stat,m6stat,row.names = c("$\\textit{N}$","Pseudo $R^2$"))

#rename columns
colnames(S2_df_stat) <- c("m1stat"="XSA","m2stat"="XCIP","m3stat"=" XSACIP","m4stat"="XSA","m5stat"="XCIP","m6stat"="XSA&CIP") 

S2 <- rbind(S2_df,S2_df_stat)

#replace NA with " "
S2[is.na(S2)] <- " "

#write data into CSV file
write.csv(S2,"D:/Statistical Methods/Milestone III/S2.csv")

