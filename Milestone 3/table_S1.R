library(dplyr)
library(ggplot2)
library(rcompanion)
library(rms)
library(questionr)
library(readr)

#Read data
setwd("D:/Statistical Methods/Project")
article_Data <- read.csv('ArticleLevel-RegData-ALLSA_Xc_1_NData_655386_LONGXCIP2.csv')

#Filter Year [1970-2018] and Filter Kp >= 2 and Wp >= 2
article_Data = article_Data %>% filter(Yp >= 1970)
article_Data = article_Data %>% filter(Yp <= 2018)
article_Data = article_Data %>% filter(Kp >= 2)
article_Data = article_Data %>% filter(nMeSHMain >= 2)
article_Data = article_Data %>% filter(IRegionRefinedp > 0 & IRegionRefinedp < 7)

# Convert the Data types
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

# Model 1 - for X_SA

#model
model1 <- glm(XSAp ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp + NCIPp, 
              data = article_Data, family=binomial(link='logit'))
#summary
summary(model1)

#calculating r sqaure and N
nmodel1 <- nagelkerke(model1)

# coefficients
output = odds.ratio(model1) # HEAVY COMPUTATIONAL!
output = apply(output, 2, formatC, format="f", digits=4)
output

#get R sqaured from nmodel1 
S1v1_XSA <- capture.output(nmodel1$Pseudo.R.squared.for.model.vs.null)
S1R_XSA <- parse_number(S1v1_XSA[2])

#get coefficients from output
S1v2_XSA <- capture.output(output)
S1y_XSA  <- parse_number(S1v2_XSA[3])
S1z_XSA  <- parse_number(S1v2_XSA[4])
S1k_XSA  <- parse_number(S1v2_XSA[5])
S1w_XSA  <- parse_number(S1v2_XSA[6])
S1r_XSA  <- parse_number(S1v2_XSA[7])
S1ncip_XSA  <- parse_number(S1v2_XSA[8])
S1nsa_XSA  <- NA

#robust SD
SD <- as.character(format((as.numeric(output[2,3])-as.numeric(output[2,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[2,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[2,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[2,4])<0.001,"***",star)
S1y_XSA <- as.character(S1y_XSA)
S1y_XSA <- paste(S1y_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[3,3])-as.numeric(output[3,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[3,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[3,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[3,4])<0.001,"***",star)
S1z_XSA <- as.character(S1z_XSA)
S1z_XSA <- paste(S1z_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[4,3])-as.numeric(output[4,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[4,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[4,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[4,4])<0.001,"***",star)
S1k_XSA <- as.character(S1k_XSA)
S1k_XSA <- paste(S1k_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[5,3])-as.numeric(output[5,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[5,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[5,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[5,4])<0.001,"***",star)
S1w_XSA <- as.character(S1w_XSA)
S1w_XSA <- paste(S1w_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[6,3])-as.numeric(output[6,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[6,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[6,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[6,4])<0.001,"***",star)
S1r_XSA <- as.character(S1r_XSA)
S1r_XSA <- paste(S1r_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[7,3])-as.numeric(output[7,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[7,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[7,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[7,4])<0.001,"***",star)
S1ncip_XSA <- as.character(S1ncip_XSA)
S1ncip_XSA <- paste(S1ncip_XSA,star,SD,sep=" ")

#get N
S1v3_XSA <- capture.output(nmodel1$Number.of.observations)
S1N_XSA <- parse_number(S1v3_XSA[2])

#vector for coefficients of the model
m1coef <- c(S1y_XSA ,S1z_XSA, S1k_XSA, S1w_XSA , S1r_XSA, S1ncip_XSA, S1nsa_XSA)

#vector for N and R square
m1stat <- c(S1N_XSA,S1R_XSA)

#Model 2 - for X_CIP
options(scipen=2)
#model
model2 <- glm(XCIPp ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp + NSAp, 
              data = article_Data, family=binomial(link='logit'))
#Summary 
summary(model2)

#calculating r sqaure
nmodel2 <- nagelkerke(model2)

#coefficients
output2 = odds.ratio(model2) # HEAVY COMPUTATIONAL!
output2= apply(output2, 2, formatC, format="f", digits=4)
output2

#get R sqaured from nmodel2
S1v1_XCIP <- capture.output(nmodel2$Pseudo.R.squared.for.model.vs.null)
S1R_XCIP <- parse_number(S1v1_XCIP[2])

#get coefficients from output2
S1v2_XCIP <- capture.output(output2)
S1y_XCIP  <- parse_number(S1v2_XCIP[3])
S1z_XCIP  <- parse_number(S1v2_XCIP[4])
S1k_XCIP  <- parse_number(S1v2_XCIP[5])
S1w_XCIP  <- parse_number(S1v2_XCIP[6])
S1r_XCIP  <- parse_number(S1v2_XCIP[7])
S1nsa_XCIP  <- parse_number(S1v2_XCIP[8])
S1ncip_XCIP  <- NA


S1y_XSA <- as.character(S1y_XSA)
S1y_XSA <- paste(S1y_XSA,star,SD,sep=" ")

S1v3_XCIP <- capture.output(nmodel2$Number.of.observations)
S1N_XCIP <- parse_number(S1v3_XCIP[2])
m2coef <- c(S1y_XCIP ,S1z_XCIP, S1k_XCIP, S1w_XCIP , S1r_XCIP, S1ncip_XCIP, S1nsa_XCIP)
m2stat <- c(S1N_XCIP,S1R_XCIP)

#robust SD
SD <- as.character(format((as.numeric(output2[ ,3])-as.numeric(output2[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model2)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # get out p-values
mod_summary_stars <- NA                             # Named vector with significance stars
mod_summary_stars[mod_summary_sign < 0.1] <- " "
mod_summary_stars[mod_summary_sign < 0.05] <- "*"
mod_summary_stars[mod_summary_sign < 0.01] <- "**"
mod_summary_stars[mod_summary_sign < 0.001] <- "***"
mod_summary_stars[is.na(mod_summary_stars)] <- " "
names(mod_summary_stars) <- names(mod_summary_sign)
mod_summary_stars  

#concatenate coeff, SD and stars and prepare SD
SD <- c(SD[2:6],"",SD[7])

#prepare star
stars <- c(mod_summary_stars[2:6],"",mod_summary_stars[7])

#concat concatenate
m2coef <- paste(as.character(m2coef),stars,SD,sep=" ")

#Model 3 - for X_CIP
article_Data$XSACIPp <- ifelse((article_Data$XSAp==1) & (article_Data$XCIPp == 1),1,0)
article_Data$XSACIPp <- as.numeric(article_Data$XSACIPp)

df_article_XSACIP <- article_Data %>% filter((XSAp == 1 & XCIPp == 1)|(XSAp == 0 & XCIPp == 0))

#model
options(scipen=2)
model3 <- glm(XSACIPp ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp ,
              data = df_article_XSACIP , family=binomial(link='logit'))
#summary
summary(model3)

#calculating r sqaure
nmodel3 <- nagelkerke(model3)

#coefficients
output3 = odds.ratio(model3) # HEAVY COMPUTATIONAL!
output3= apply(output3, 2, formatC, format="f", digits=4)
output3

#get R sqaured from nmodel3
S1v1_XCIPSA <- capture.output(nmodel3$Pseudo.R.squared.for.model.vs.null)
S1R_XCIPSA <- parse_number(S1v1_XCIPSA[2])

#get coefficients from output3
S1v2_XCIPSA <- capture.output(output3)
S1y_XCIPSA  <- parse_number(S1v2_XCIPSA[3])
S1z_XCIPSA  <- parse_number(S1v2_XCIPSA[4])
S1k_XCIPSA  <- parse_number(S1v2_XCIPSA[5])
S1w_XCIPSA  <- parse_number(S1v2_XCIPSA[6])
S1r_XCIPSA  <- parse_number(S1v2_XCIPSA[7])
S1nsa_XCIPSA  <- NA
S1ncip_XCIPSA  <- NA

S1v3_XCIPSA <- capture.output(nmodel3$Number.of.observations)
S1N_XCIPSA <- parse_number(S1v3_XCIPSA[2])
m3coef <- c(S1y_XCIPSA ,S1z_XCIPSA, S1k_XCIPSA, S1w_XCIPSA , S1r_XCIPSA, S1ncip_XCIPSA, S1nsa_XCIPSA)
m3stat <- c(S1N_XCIPSA,S1R_XCIPSA)

#robust SD
SD <- as.character(format((as.numeric(output3[ ,3])-as.numeric(output3[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model3)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # get the p-values
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

#concatenate
m3coef <- paste(as.character(m3coef),stars,SD,sep=" ")

#Model 4 - for X_SA
article_Data$I_year <- ifelse(article_Data$Yp >= 2014,1,0)
article_Data$I_RNA <- ifelse(article_Data$IRegionRefinedp == 1,1,0)
article_Data$I_REU <- ifelse(article_Data$IRegionRefinedp == 2,1,0)
article_Data$I_RAA <- ifelse(article_Data$IRegionRefinedp == 3,1,0)

#model
model4 <- glm(XSAp ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + 
                NRegp + NCIPp + I_year + I_RNA + I_REU + I_RAA +
                I_RNA*I_year + I_REU *I_year + I_RAA* I_year , 
              data = article_Data, family=binomial(link='logit'))
#summary
summary(model4)

#calculating r sqaure 
nmodel4 <- nagelkerke(model4)

#coefficients
output4 = odds.ratio(model4) # HEAVY COMPUTATIONAL!
output4 = apply(output4, 2, formatC, format="f", digits=4)
output4

#get R sqaured from nmodel3
S1v1_XSAI <- capture.output(nmodel4$Pseudo.R.squared.for.model.vs.null)
S1R_XSAI <- parse_number(S1v1_XSAI[2])

#get coefficients from output3
S1v2_XSAI <- capture.output(output4)
S1y_XSAI  <- parse_number(S1v2_XSAI[3])
S1z_XSAI  <- parse_number(S1v2_XSAI[4])
S1k_XSAI  <- parse_number(S1v2_XSAI[5])
S1w_XSAI  <- parse_number(S1v2_XSAI[6])
S1r_XSAI  <- parse_number(S1v2_XSAI[7])
S1ncip_XSAI  <- parse_number(S1v2_XSAI[8])
S1nsa_XSAI  <- NA
S1Iyear_XSAI <- parse_number(S1v2_XSAI[9])
S1IRNA_XSAI <- parse_number(S1v2_XSAI[10])
S1IREU_XSAI <- parse_number(S1v2_XSAI[11])
S1IRAA_XSAI <- parse_number(S1v2_XSAI[12])
S1IyearRNA_XSAI <- parse_number(S1v2_XSAI[13])
S1IyearREU_XSAI <- parse_number(S1v2_XSAI[14])
S1IyearRAA_XSAI <- parse_number(S1v2_XSAI[15])
S1v3_XSAI <- capture.output(nmodel4$Number.of.observations)
S1N_XSAI <- parse_number(S1v3_XSAI[2])
m4coef <- c(S1y_XSAI ,S1z_XSAI, S1k_XSAI, S1w_XSAI , S1r_XSAI,
            S1ncip_XSAI, S1nsa_XSAI,S1Iyear_XSAI ,S1IRNA_XSAI
            ,S1IREU_XSAI ,S1IRAA_XSAI ,S1IyearRNA_XSAI ,
            S1IyearREU_XSAI ,S1IyearRAA_XSAI)
m4stat <- c(S1N_XSAI,S1R_XSAI)

#robust SD
SD <- as.character(format((as.numeric(output4[ ,3])-as.numeric(output4[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model4)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # get the p-values
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

#concatenate
m4coef <- paste(as.character(m4coef),stars,SD,sep=" ")

#model 5 for XCIP with I
options(scipen=2)

#model
model5 <- glm(XCIPp ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) +
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

#get R sqaured from nmodel5
S1v1_XCIPI <- capture.output(nmodel5$Pseudo.R.squared.for.model.vs.null)
S1R_XCIPI <- parse_number(S1v1_XCIPI[2])

#get coefficients from output3
S1v2_XCIPI <- capture.output(output5)
S1y_XCIPI  <- parse_number(S1v2_XCIPI[3])
S1z_XCIPI  <- parse_number(S1v2_XCIPI[4])
S1k_XCIPI  <- parse_number(S1v2_XCIPI[5])
S1w_XCIPI  <- parse_number(S1v2_XCIPI[6])
S1r_XCIPI  <- parse_number(S1v2_XCIPI[7])
S1nsa_XCIPI  <- parse_number(S1v2_XCIPI[8])
S1ncip_XCIPI  <- NA
S1Iyear_XCIPI <- parse_number(S1v2_XCIPI[9])
S1IRNA_XCIPI <- parse_number(S1v2_XCIPI[10])
S1IREU_XCIPI <- parse_number(S1v2_XCIPI[11])
S1IRAA_XCIPI <- parse_number(S1v2_XCIPI[12])
S1IyearRNA_XCIPI <- parse_number(S1v2_XCIPI[13])
S1IyearREU_XCIPI <- parse_number(S1v2_XCIPI[14])
S1IyearRAA_XCIPI <- parse_number(S1v2_XCIPI[15])
S1v3_XCIPI <- capture.output(nmodel5$Number.of.observations)
S1N_XCIPI <- parse_number(S1v3_XCIPI[2])
m5coef <- c(S1y_XCIPI ,S1z_XCIPI, S1k_XCIPI, S1w_XCIPI , S1r_XCIPI,
            S1ncip_XCIPI, S1nsa_XCIPI,S1Iyear_XCIPI ,S1IRNA_XCIPI
            ,S1IREU_XCIPI ,S1IRAA_XCIPI ,S1IyearRNA_XCIPI ,
            S1IyearREU_XCIPI ,S1IyearRAA_XCIPI)
m5stat <- c(S1N_XCIPI,S1R_XCIPI)

#robust SD
SD <- as.character(format((as.numeric(output5[ ,3])-as.numeric(output5[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model5)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # get the p-values
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

#concatenate
m5coef <- paste(as.character(m5coef),stars,SD,sep=" ")

#model 56 for XSACIP with I
df_article_XSACIP$I_year <- ifelse(df_article_XSACIP$Yp >= 2014,1,0)
df_article_XSACIP$I_RNA <- ifelse(df_article_XSACIP$IRegionRefinedp == 1,1,0)
df_article_XSACIP$I_REU <- ifelse(df_article_XSACIP$IRegionRefinedp == 2,1,0)
df_article_XSACIP$I_RAA <- ifelse(df_article_XSACIP$IRegionRefinedp == 3,1,0)
options(scipen=2)
#model
model6 <- glm(XCIPp ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) +
                NRegp + I_year + I_RNA + I_REU + I_RAA +
                I_RNA *I_year + I_REU *I_year + I_RAA* I_year , 
              data = df_article_XSACIP, family=binomial(link='logit'))

#summary
summary(model6)

#calculating r sqaure  
nmodel6 <- nagelkerke(model6)

#coefficients
output6 = odds.ratio(model6) # HEAVY COMPUTATIONAL!
output6= apply(output6, 2, formatC, format="f", digits=4)
output6

#get R sqaured from nmodel6
S1v1_XSACIPI <- capture.output(nmodel6$Pseudo.R.squared.for.model.vs.null)
S1R_XSACIPI <- parse_number(S1v1_XSACIPI[2])

#get coefficients from output6
S1v2_XSACIPI <- capture.output(output6)
S1y_XSACIPI  <- parse_number(S1v2_XSACIPI[3])
S1z_XSACIPI  <- parse_number(S1v2_XSACIPI[4])
S1k_XSACIPI  <- parse_number(S1v2_XSACIPI[5])
S1w_XSACIPI  <- parse_number(S1v2_XSACIPI[6])
S1r_XSACIPI  <- parse_number(S1v2_XSACIPI[7])
S1nsa_XSACIPI  <- NA
S1ncip_XSACIPI  <- NA
S1Iyear_XSACIPI <- parse_number(S1v2_XSACIPI[8])
S1IRNA_XSACIPI <- parse_number(S1v2_XSACIPI[9])
S1IREU_XSACIPI <- parse_number(S1v2_XSACIPI[10])
S1IRAA_XSACIPI <- parse_number(S1v2_XSACIPI[11])
S1IyearRNA_XSACIPI <- parse_number(S1v2_XSACIPI[12])
S1IyearREU_XSACIPI <- parse_number(S1v2_XSACIPI[13])
S1IyearRAA_XSACIPI <- parse_number(S1v2_XSACIPI[14])
S1v3_XSACIPI <- capture.output(nmodel6$Number.of.observations)
S1N_XSACIPI <- parse_number(S1v3_XSACIPI[2])
m6coef <- c(S1y_XSACIPI ,S1z_XSACIPI, S1k_XSACIPI, S1w_XSACIPI , S1r_XSACIPI,
            S1ncip_XSACIPI, S1nsa_XSACIPI,S1Iyear_XSACIPI ,S1IRNA_XSACIPI
            ,S1IREU_XSACIPI ,S1IRAA_XSACIPI ,S1IyearRNA_XSACIPI ,
            S1IyearREU_XSACIPI ,S1IyearRAA_XSACIPI)
m6stat <- c(S1N_XSACIPI,S1R_XSACIPI)

#robust SD
SD <- as.character(format((as.numeric(output6[ ,3])-as.numeric(output6[ ,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")

#stars
mod_summary <- summary(model6)
mod_summary_sign <- mod_summary$coefficients[ , 4]  # get the p-values
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

#concatenate
m6coef <- paste(as.character(m6coef),stars,SD,sep=" ")

#Gather all the data
#number of rows
n<-length(m6coef) 

#filling empty rows with NA
length(m1coef) <- n
length(m2coef) <- n
length(m3coef) <- n

#coeff's dataframe
S1_df <- data.frame(m1coef,m2coef,m3coef,m4coef,m5coef,m6coef,row.names = c("y","$\\bar{z_j}$","ln k","ln w","$N_R$","$N_{CIP}$","$N_{SA}$","$I_{2014+}$","$I_{R_{NA}}$","$I_{R_{EU}}$","$I_{R_{AA}}$","$I_{R_{NA}} \\times I_{2014+}$","$I_{R_{EU}} \\times  I_{2014+}$","$I_{R_{AA}} \\times  I_{2014+}$"))

#rename the columns 
colnames(S1_df) <- c("m1coef"="XSA","m2coef"="XCIP","m3coef"=" XSACIP","m4coef"="XSA","m5coef"="XCIP","m6coef"="XSA&CIP")

#stat dataframe
S1_df_stat <- data.frame(m1stat,m2stat,m3stat,m4stat,m5stat,m6stat,row.names = c("$\\textit{N}$","Pseudo $R^2$"))

#rename the columns
colnames(S1_df_stat) <- c("m1stat"="XSA","m2stat"="XCIP","m3stat"=" XSACIP","m4stat"="XSA","m5stat"="XCIP","m6stat"="XSA&CIP") 

S1 <- rbind(S1_df,S1_df_stat)

#replace NA with " "
S1[is.na(S1)] <- " "

#write data into CSV file
write.csv(S1,"D:/Statistical Methods/Milestone III/S1.csv")

