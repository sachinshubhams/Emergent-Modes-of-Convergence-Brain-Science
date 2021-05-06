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

#Convert Data types
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
article_Data$distantSA <- ifelse((article_Data$SA1>=1|article_Data$SA2>=1|article_Data$SA3>=1|article_Data$SA4>=1) & (article_Data$SA5>=1 | article_Data$SA6>=1 ),1,0)

#model
model1 <- glm(distantSA  ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp + NCIPp, 
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
S3v1_XSA <- capture.output(nmodel1$Pseudo.R.squared.for.model.vs.null)
S3R_XSA <- parse_number(S3v1_XSA[2])

#get coefficients from output
S3v2_XSA <- capture.output(output)
S3y_XSA  <- parse_number(S3v2_XSA[3])
S3z_XSA  <- parse_number(S3v2_XSA[4])
S3k_XSA  <- parse_number(S3v2_XSA[5])
S3w_XSA  <- parse_number(S3v2_XSA[6])
S3r_XSA  <- parse_number(S3v2_XSA[7])
S3ncip_XSA  <- parse_number(S3v2_XSA[8])
S3nsa_XSA  <- NA

#robust SD
SD <- as.character(format((as.numeric(output[2,3])-as.numeric(output[2,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[2,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[2,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[2,4])<0.001,"***",star)
S3y_XSA <- as.character(S3y_XSA)
S3y_XSA <- paste(S3y_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[3,3])-as.numeric(output[3,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[3,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[3,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[3,4])<0.001,"***",star)
S3z_XSA <- as.character(S3z_XSA)
S3z_XSA <- paste(S3z_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[4,3])-as.numeric(output[4,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[4,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[4,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[4,4])<0.001,"***",star)
S3k_XSA <- as.character(S3k_XSA)
S3k_XSA <- paste(S3k_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[5,3])-as.numeric(output[5,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[5,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[5,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[5,4])<0.001,"***",star)
S3w_XSA <- as.character(S3w_XSA)
S3w_XSA <- paste(S3w_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[6,3])-as.numeric(output[6,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[6,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[6,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[6,4])<0.001,"***",star)
S3r_XSA <- as.character(S3r_XSA)
S3r_XSA <- paste(S3r_XSA,star,SD,sep=" ")

SD <- as.character(format((as.numeric(output[7,3])-as.numeric(output[7,2]))/2),digits=4)
SD <-paste("(",SD,")",sep="")
star <- ifelse(as.numeric(output[7,4])<0.05,"*"," ")
star <- ifelse(as.numeric(output[7,4])<0.01,"**",star)
star <- ifelse(as.numeric(output[7,4])<0.001,"***",star)
S3ncip_XSA <- as.character(S3ncip_XSA)
S3ncip_XSA <- paste(S3ncip_XSA,star,SD,sep=" ")


#extract N
S3v3_XSA <- capture.output(nmodel1$Number.of.observations)
S3N_XSA <- parse_number(S3v3_XSA[2])
#vector for coefficients of the model
m1coef <- c(S3y_XSA ,S3z_XSA, S3k_XSA, S3w_XSA , S3r_XSA, S3ncip_XSA, S3nsa_XSA)
#vector for N and R square
m1stat <- c(S3N_XSA,S3R_XSA)

#Model 2 - for X_CIP
options(scipen=2)
article_Data$distantCIP <- ifelse((article_Data$CIP1>=1|article_Data$CIP3>=1|article_Data$CIP5>=1) & (article_Data$CIP4>=1 | article_Data$CIP8>=1 ),1,0)
model2 <- glm(distantCIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp + NSAp, 
              data = article_Data, family=binomial(link='logit'))

#summary
summary(model2)

#calculating r sqaure   
nmodel2 <- nagelkerke(model2)

#coefficients
output2 = odds.ratio(model2) # HEAVY COMPUTATIONAL!
output2= apply(output2, 2, formatC, format="f", digits=4)
output2

#get R sqaure from nmodel2
S3v1_XCIP <- capture.output(nmodel2$Pseudo.R.squared.for.model.vs.null)
S3R_XCIP <- parse_number(S3v1_XCIP[2])

#get coefficients from output2
S3v2_XCIP <- capture.output(output2)
S3y_XCIP  <- parse_number(S3v2_XCIP[3])
S3z_XCIP  <- parse_number(S3v2_XCIP[4])
S3k_XCIP  <- parse_number(S3v2_XCIP[5])
S3w_XCIP  <- parse_number(S3v2_XCIP[6])
S3r_XCIP  <- parse_number(S3v2_XCIP[7])
S3nsa_XCIP  <- parse_number(S3v2_XCIP[8])
S3ncip_XCIP  <- NA


S3y_XSA <- as.character(S3y_XSA)
S3y_XSA <- paste(S3y_XSA,star,SD,sep=" ")

S3v3_XCIP <- capture.output(nmodel2$Number.of.observations)
S3N_XCIP <- parse_number(S3v3_XCIP[2])
m2coef <- c(S3y_XCIP ,S3z_XCIP, S3k_XCIP, S3w_XCIP , S3r_XCIP, S3ncip_XCIP, S3nsa_XCIP)
m2stat <- c(S3N_XCIP,S3R_XCIP)

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

#concatenate star, Sd and coeff
m2coef <- paste(as.character(m2coef),stars,SD,sep=" ")
m2coef[m2coef == "NA  "] <- ""

#Model 3 - for X_CIP
article_Data$distantSACIP <- ifelse((article_Data$distantSA==1) & (article_Data$distantCIP == 1),1,0)
article_Data$distantSACIP<- as.numeric(article_Data$distantSACIP)

df_article_distantSACIP <- article_Data %>% filter((distantSA == 1 & distantCIP == 1)|(distantSA == 0 & distantCIP == 0))

#model
options(scipen=2)
model3 <- glm(distantSACIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp ,
              data = df_article_distantSACIP , family=binomial(link='logit'))

#summary
summary(model3)

#calculating r sqaure  
nmodel3 <- nagelkerke(model3)

#coefficients
output3 = odds.ratio(model3) # HEAVY COMPUTATIONAL!
output3= apply(output3, 2, formatC, format="f", digits=4)
output3

#get R sqaure from nmodel3
S3v1_XCIPSA <- capture.output(nmodel3$Pseudo.R.squared.for.model.vs.null)
S3R_XCIPSA <- parse_number(S3v1_XCIPSA[2])

#get coefficients from output3
S3v2_XCIPSA <- capture.output(output3)
S3y_XCIPSA  <- parse_number(S3v2_XCIPSA[3])
S3z_XCIPSA  <- parse_number(S3v2_XCIPSA[4])
S3k_XCIPSA  <- parse_number(S3v2_XCIPSA[5])
S3w_XCIPSA  <- parse_number(S3v2_XCIPSA[6])
S3r_XCIPSA  <- parse_number(S3v2_XCIPSA[7])
S3nsa_XCIPSA  <- NA
S3ncip_XCIPSA  <- NA

S3v3_XCIPSA <- capture.output(nmodel3$Number.of.observations)
S3N_XCIPSA <- parse_number(S3v3_XCIPSA[2])
m3coef <- c(S3y_XCIPSA ,S3z_XCIPSA, S3k_XCIPSA, S3w_XCIPSA , S3r_XCIPSA, S3ncip_XCIPSA, S3nsa_XCIPSA)
m3stat <- c(S3N_XCIPSA,S3R_XCIPSA)

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

#concatenate for SD, star and coeff
m3coef <- paste(as.character(m3coef),stars,SD,sep=" ")
m3coef[m3coef == "NA  "] <- ""

#Model 4 - for X_SA
article_Data$I_year <- ifelse(article_Data$Yp >= 2014,1,0)
article_Data$I_RNA <- ifelse(article_Data$IRegionRefinedp == 1,1,0)
article_Data$I_REU <- ifelse(article_Data$IRegionRefinedp == 2,1,0)
article_Data$I_RAA <- ifelse(article_Data$IRegionRefinedp == 3,1,0)

#model
model4 <- glm(distantSA ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + 
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

#get R sqaure from nmodel4
S3v1_XSAI <- capture.output(nmodel4$Pseudo.R.squared.for.model.vs.null)
S3R_XSAI <- parse_number(S3v1_XSAI[2])

#get coefficients from output4
S3v2_XSAI <- capture.output(output4)
S3y_XSAI  <- parse_number(S3v2_XSAI[3])
S3z_XSAI  <- parse_number(S3v2_XSAI[4])
S3k_XSAI  <- parse_number(S3v2_XSAI[5])
S3w_XSAI  <- parse_number(S3v2_XSAI[6])
S3r_XSAI  <- parse_number(S3v2_XSAI[7])
S3ncip_XSAI  <- parse_number(S3v2_XSAI[8])
S3nsa_XSAI  <- NA
S3Iyear_XSAI <- parse_number(S3v2_XSAI[9])
S3IRNA_XSAI <- parse_number(S3v2_XSAI[10])
S3IREU_XSAI <- parse_number(S3v2_XSAI[11])
S3IRAA_XSAI <- parse_number(S3v2_XSAI[12])
S3IyearRNA_XSAI <- parse_number(S3v2_XSAI[13])
S3IyearREU_XSAI <- parse_number(S3v2_XSAI[14])
S3IyearRAA_XSAI <- parse_number(S3v2_XSAI[15])
S3v3_XSAI <- capture.output(nmodel4$Number.of.observations)
S3N_XSAI <- parse_number(S3v3_XSAI[2])
m4coef <- c(S3y_XSAI ,S3z_XSAI, S3k_XSAI, S3w_XSAI , S3r_XSAI,
            S3ncip_XSAI, S3nsa_XSAI,S3Iyear_XSAI ,S3IRNA_XSAI
            ,S3IREU_XSAI ,S3IRAA_XSAI ,S3IyearRNA_XSAI ,
            S3IyearREU_XSAI ,S3IyearRAA_XSAI)
m4stat <- c(S3N_XSAI,S3R_XSAI)

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

#concatenate SD, star and Coeff
m4coef <- paste(as.character(m4coef),stars,SD,sep=" ")
m4coef[m4coef == "NA  "] <- ""

#model 5 for XCIP with I
options(scipen=2)

#model
model5 <- glm(distantCIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) +
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

#get R sqaure from nmodel5
S3v1_XCIPI <- capture.output(nmodel5$Pseudo.R.squared.for.model.vs.null)
S3R_XCIPI <- parse_number(S3v1_XCIPI[2])

#get coefficients from output5
S3v2_XCIPI <- capture.output(output5)
S3y_XCIPI  <- parse_number(S3v2_XCIPI[3])
S3z_XCIPI  <- parse_number(S3v2_XCIPI[4])
S3k_XCIPI  <- parse_number(S3v2_XCIPI[5])
S3w_XCIPI  <- parse_number(S3v2_XCIPI[6])
S3r_XCIPI  <- parse_number(S3v2_XCIPI[7])
S3nsa_XCIPI  <- parse_number(S3v2_XCIPI[8])
S3ncip_XCIPI  <- NA
S3Iyear_XCIPI <- parse_number(S3v2_XCIPI[9])
S3IRNA_XCIPI <- parse_number(S3v2_XCIPI[10])
S3IREU_XCIPI <- parse_number(S3v2_XCIPI[11])
S3IRAA_XCIPI <- parse_number(S3v2_XCIPI[12])
S3IyearRNA_XCIPI <- parse_number(S3v2_XCIPI[13])
S3IyearREU_XCIPI <- parse_number(S3v2_XCIPI[14])
S3IyearRAA_XCIPI <- parse_number(S3v2_XCIPI[15])
S3v3_XCIPI <- capture.output(nmodel5$Number.of.observations)
S3N_XCIPI <- parse_number(S3v3_XCIPI[2])
m5coef <- c(S3y_XCIPI ,S3z_XCIPI, S3k_XCIPI, S3w_XCIPI , S3r_XCIPI,
            S3ncip_XCIPI, S3nsa_XCIPI,S3Iyear_XCIPI ,S3IRNA_XCIPI
            ,S3IREU_XCIPI ,S3IRAA_XCIPI ,S3IyearRNA_XCIPI ,
            S3IyearREU_XCIPI ,S3IyearRAA_XCIPI)
m5stat <- c(S3N_XCIPI,S3R_XCIPI)

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

#concatenate Sd, star and coeff
m5coef <- paste(as.character(m5coef),stars,SD,sep=" ")
m5coef[m5coef == "NA  "] <- ""

#model 6 for XSACIP with I
df_article_distantSACIP$I_year <- ifelse(df_article_distantSACIP$Yp >= 2014,1,0)
df_article_distantSACIP$I_RNA <- ifelse(df_article_distantSACIP$IRegionRefinedp == 1,1,0)
df_article_distantSACIP$I_REU <- ifelse(df_article_distantSACIP$IRegionRefinedp == 2,1,0)
df_article_distantSACIP$I_RAA <- ifelse(df_article_distantSACIP$IRegionRefinedp == 3,1,0)
options(scipen=2)

#model
model6 <- glm(distantSACIP ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) +
                NRegp + I_year + I_RNA + I_REU + I_RAA +
                I_RNA *I_year + I_REU *I_year + I_RAA* I_year , 
              data = df_article_distantSACIP, family=binomial(link='logit'))

#summary
summary(model6)

#calculating r sqaure   
nmodel6 <- nagelkerke(model6)

#coefficients
output6 = odds.ratio(model6) # HEAVY COMPUTATIONAL!
output6= apply(output6, 2, formatC, format="f", digits=4)
output6

#get R sqaure from nmodel6
S3v1_XSACIPI <- capture.output(nmodel6$Pseudo.R.squared.for.model.vs.null)
S3R_XSACIPI <- parse_number(S3v1_XSACIPI[2])

#get coefficients from output6
S3v2_XSACIPI <- capture.output(output6)
S3y_XSACIPI  <- parse_number(S3v2_XSACIPI[3])
S3z_XSACIPI  <- parse_number(S3v2_XSACIPI[4])
S3k_XSACIPI  <- parse_number(S3v2_XSACIPI[5])
S3w_XSACIPI  <- parse_number(S3v2_XSACIPI[6])
S3r_XSACIPI  <- parse_number(S3v2_XSACIPI[7])
S3nsa_XSACIPI  <- NA
S3ncip_XSACIPI  <- NA
S3Iyear_XSACIPI <- parse_number(S3v2_XSACIPI[8])
S3IRNA_XSACIPI <- parse_number(S3v2_XSACIPI[9])
S3IREU_XSACIPI <- parse_number(S3v2_XSACIPI[10])
S3IRAA_XSACIPI <- parse_number(S3v2_XSACIPI[11])
S3IyearRNA_XSACIPI <- parse_number(S3v2_XSACIPI[12])
S3IyearREU_XSACIPI <- parse_number(S3v2_XSACIPI[13])
S3IyearRAA_XSACIPI <- parse_number(S3v2_XSACIPI[14])
S3v3_XSACIPI <- capture.output(nmodel6$Number.of.observations)
S3N_XSACIPI <- parse_number(S3v3_XSACIPI[2])
m6coef <- c(S3y_XSACIPI ,S3z_XSACIPI, S3k_XSACIPI, S3w_XSACIPI , S3r_XSACIPI,
            S3ncip_XSACIPI, S3nsa_XSACIPI,S3Iyear_XSACIPI ,S3IRNA_XSACIPI
            ,S3IREU_XSACIPI ,S3IRAA_XSACIPI ,S3IyearRNA_XSACIPI ,
            S3IyearREU_XSACIPI ,S3IyearRAA_XSACIPI)
m6stat <- c(S3N_XSACIPI,S3R_XSACIPI)

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

#concatenate SD, star and Coeff
m6coef <- paste(as.character(m6coef),stars,SD,sep=" ")
m6coef[m6coef == "NA  "] <- ""

#Gather Data
#number of rows
n<-length(m6coef) 

#filling empty rows with NA in the first columns
length(m1coef) <- n
length(m2coef) <- n
length(m3coef) <- n

#coeff dataframe
S3_df <- data.frame(m1coef,m2coef,m3coef,m4coef,m5coef,m6coef,row.names = c("y","$\\bar{z_j}$","ln k","ln w","$N_R$","$N_{CIP}$","$N_{SA}$","$I_{2014+}$","$I_{R_{NA}}$","$I_{R_{EU}}$","$I_{R_{AA}}$","$I_{R_{NA}} \\times I_{2014+}$","$I_{R_{EU}} \\times  I_{2014+}$","$I_{R_{AA}} \\times  I_{2014+}$"))

#rename columns 
colnames(S3_df) <- c("m1coef"="XSA","m2coef"="XCIP","m3coef"=" XSACIP","m4coef"="XSA","m5coef"="XCIP","m6coef"="XSA&CIP")

#stat dataframe
S3_df_stat <- data.frame(m1stat,m2stat,m3stat,m4stat,m5stat,m6stat,row.names = c("$\\textit{N}$","Pseudo $R^2$"))

#rename columns
colnames(S3_df_stat) <- c("m1stat"="XSA","m2stat"="XCIP","m3stat"=" XSACIP","m4stat"="XSA","m5stat"="XCIP","m6stat"="XSA&CIP") 

S3 <- rbind(S3_df,S3_df_stat)

#write data into CSV file
write.csv(S3,"D:/Statistical Methods/Milestone III/S3.csv")


