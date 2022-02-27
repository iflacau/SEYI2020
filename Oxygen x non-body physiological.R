##Oxygen x Other things

ext_ox <- data.frame('Period' = timeline$interval_name, 'Number_Genera' = NA, 
        'Mean_Size' = NA, 'Mean_Oxygen_Overall' = mean(timeline$pO2), 'Fifth' = NA, 
        'NinetyFifth' = NA, 'Percent_ex' = NA, 'Number_ex' = NA, 'Age_Bottom' = NA,
        'Age_Mid' = NA, 'Age_Top' = NA, 'oxygen' = NA, 'Circ_Odds' = NA, 'tierSizeOdds' = NA, 
        'feedSizeOdds' = NA, 'motSizeOdds' = NA, 'tierExtOdds' = NA, 'feedExtOdds' = NA, 
        'motExtOdds' = NA, 'feedpZVal' = NA, 'tierpZVal' = NA, 'motpZVal' = NA )
mollusca[mollusca$circ == "closed",]$circ = 1
mollusca[mollusca$circ == "open",]$circ = 0

##Tiering motility and feeding mechanism 
mollusca$tiering2<-mollusca$tiering 
mollusca$tiering2[mollusca$tiering>=2] <- 0
mollusca$tiering2 <- as.factor(mollusca$tiering2)
mollusca$feeding2[mollusca$feeding == 5] <- 1
mollusca$feeding2[mollusca$feeding != 5] <- 0
mollusca$feeding2 <- as.factor(mollusca$feeding2)
mollusca$motility2[mollusca$motility>=3]<-0
mollusca$motility2[mollusca$motility<3]<-1
mollusca$motility2 <- as.factor(mollusca$motility2)
mollusca$motility3 <- mollusca$motility
for (i in 1:length(timeline)) { ###Motility, tiering and feeding loop
  temp<- subset(mollusca, mollusca$fad_age >= timeline$age_top[i] & mollusca$lad_age <= timeline$age_bottom[i])
  temp$ex <- 0
  temp$ex[temp$lad_age == timeline$age_bottom[i]] <- 1
  temp2 <- temp[!is.na(temp$tiering2), ]
  temp3 <- temp[!is.na(temp$feeding2), ]
  temp4 <- temp[!is.na(temp$motility2), ]
  size <- log10(temp$calc_max_vol)
  size2 <- log10(temp2$calc_max_vol)
  size3 <- log10(temp3$calc_max_vol)
  size4 <- log10(temp4$calc_max_vol)

  
  glm1<-glm(temp2$ex~temp2$tiering2, data=temp2, family="binomial", maxit=100)
  ext_ox$tierExtOdds [i] <- summary(glm1)$coefficients[2,1] 
  ext_ox$tierpZVal[i] <- summary(glm1)$coefficients[2,4] 
  
  glm2<-glm(temp3$ex~temp3$feeding2, data=temp3, family="binomial", maxit=100)
  ext_ox$feedExtOdds [i] <- summary(glm2)$coefficients[2,1] 
  ext_ox$feedpZVal[i] <- summary(glm2)$coefficients[2,4] 
  
  glm3<-glm(temp4$ex~temp4$motility2, data=temp4, maxit=100)
  ext_ox$motExtOdds [i] <- summary(glm3)$coefficients[2,1] 
  ext_ox$motpZVal[i] <- summary(glm3)$coefficients[2,4] 

}
for (i in 1:length(timescale)){
  temp4<-subset(tier, tier$fad_age>=timescale$age_top[i] & tier$lad_age<=timescale$age_bottom[i])
  temp5<-subset(mot, mot$fad_age>=timescale$age_top[i] & mot$lad_age<=timescale$age_bottom[i])
  temp6<-subset(feed, feed$fad_age>=timescale$age_top[i] & feed$lad_age<=timescale$age_bottom[i])
  temp4$extinct<-0
  temp5$extinct<-0
  temp6$extinct<-0
  temp4$extinct[temp4$lad_age==timescale$age_bottom[i]]<-1
  temp5$extinct[temp5$lad_age==timescale$age_bottom[i]]<-1
  temp6$extinct[temp6$lad_age==timescale$age_bottom[i]]<-1
  glmod7<-glm(extinct~tiering2,data=temp4, family="binomial", maxit=100)
  glmod8<-glm(extinct~motility2,data=temp5, family="binomial", maxit=100)
  glmod9<-glm(extinct~feeding2,data=temp6, family="binomial", maxit=100)
  analysismolluscs$TieringvExtinctOdds[i] <-summary(glmod7)$coefficients[2,1]
  analysismolluscs$MotilityvExtinctOdds[i] <-summary(glmod8)$coefficients[2,1]
  analysismolluscs$FeedingvExtinctOdds[i] <-summary(glmod9)$coefficients[2,1]
}

for (i in 1:length(timescale)){
  temp4<-subset(tier, tier$fad_age>=timescale$age_top[i] & tier$lad_age<=timescale$age_bottom[i])
  temp5<-subset(mot, mot$fad_age>=timescale$age_top[i] & mot$lad_age<=timescale$age_bottom[i])
  temp6<-subset(feed, feed$fad_age>=timescale$age_top[i] & feed$lad_age<=timescale$age_bottom[i])
  size1 <-temp4$logvol
  size2 <-temp5$logvol
  size3 <-temp6$logvol
  glmod7<-glm(tiering2~size1,data=temp4, family="binomial", maxit=100)
  glmod8<-glm(motility2~size2,data=temp5, family="binomial", maxit=100)
  glmod9<-glm(feeding2~size3,data=temp6, family="binomial", maxit=100)
  analysismolluscs$TieringvBDSOdds[i] <-summary(glmod7)$coefficients[2,1]
  analysismolluscs$MotilityvBDSOdds[i] <-summary(glmod8)$coefficients[2,1]
  analysismolluscs$FeedingvBDSOdds[i] <-summary(glmod9)$coefficients[2,1]
}


#glm4<-glm(temp2$tiering~size2, data=temp2, family="binomial", maxit=100)
#ext_ox$tierSizeOdds [i] <- summary(glm4)$coefficients[2,1] 

#glm5<-glm(temp3$feeding ~size3, data=temp3, family="binomial", maxit=100)
#ext_ox$feedSizeOdds [i] <- summary(glm5)$coefficients[2,1] 

#glm6<-glm(temp4$motility ~size4, data=temp4, family="binomial", maxit=100)
#ext_ox$motSizeOdds [i] <- summary(glm6)$coefficients[2,1] 

#Assigning to data frame 
for (i in 1:length(timeline)) {
  temp<- subset(mollusca, mollusca$fad_age >= timeline$age_top[i] & mollusca$lad_age <= timeline$age_bottom[i])
  temp$ex <- 0
  temp$ex[temp$lad_age == timeline$age_bottom[i]] <- 1
  y <- temp$ex
  
  paleozoic <- sum(temp$fauna == 2)
  modern <- sum(temp$fauna == 1)
  cambrian <- sum(temp$fauna == 0)
  
  size <- log10(temp$calc_max_vol)
  glm2<-glm(temp$ex~circ, data=temp, family="binomial", maxit=100)
  ext_ox$Circ_Odds [i] <- summary(glm2)$coefficients[2,1] 
  ext_ox$Number_Genera[i] <- nrow(temp)
  ext_ox$Age_Bottom[i] <- timeline$age_bottom[i]
  ext_ox$Age_Mid[i] <- timeline$age_mid[i]
  ext_ox$Age_Top[i] <- timeline$age_top[i]
  ext_ox$Mean_Size[i] <- mean(size)
  ext_ox$NinetyFifth[i] <- quantile(size, 0.95)
  ext_ox$Fifth[i] <- quantile(size, 0.05)
  ext_ox$Number_ex[i] <- sum(y)
  ext_ox$Percent_ex[i] <- (nrow(temp[temp$ex == 1, ])/nrow(temp)) *100
  ext_ox$oxygen[i] <- timeline$pO2[i]
  

}
View(ext_ox)

tmf <- data.frame('Tiering' = NA, 'Motility_Level' = NA, 'Feeding_Mechanism' = NA)

for (j in 1:6) {
  temp$yes <- 0
  temp$yes[temp$tiering == j] <- 1
  list <- list(length = 7)
  
  for (i in 1:length(timeline)) {
    temp<- subset(mollusca, mollusca$fad_age >= timeline$age_top[i] & mollusca$lad_age <= timeline$age_bottom[i])
    temp$ex <- 0
    temp$ex[temp$lad_age == timeline$age_bottom[i]] <- 1
    y <- temp$ex
    
  
    myGlm1 <- glm(y ~ temp$yes, data = temp, family = 'binomial', maxit = 100)
    myGlm2 <- glm(y ~ temp$yes, data = temp, family = 'binomial', maxit = 100)
    myGlm3 <- glm(y ~ temp$yes, data = temp, family = 'binomial', maxit = 100)
    myGlm4 <- glm(y ~ temp$yes, data = temp, family = 'binomial', maxit = 100)
    myGlm5 <- glm(y ~ temp$yes, data = temp, family = 'binomial', maxit = 100)
    myGlm6 <- glm(y ~ temp$yes, data = temp, family = 'binomial', maxit = 100)
    myGlm7 <- glm(y ~ temp$yes, data = temp, family = 'binomial', maxit = 100)
  
  }
}



###Oxygen and Body Size/exion
corOx <- cor(ext_ox$oxygen, boSi_ext$Log_Odds)
corOxCirc <- cor(ext_ox$oxygen, ext_ox$Circ_Odds)

##~~~~~ Sea Levels
OxSl <- cor(timeline$SeaLevel, ext_ox$oxygen)
corSea <- cor(timeline$SeaLevel, boSi_ext$Log_Odds)
corSeaCirc <- cor(timeline$SeaLevel, ext_ox$Circ_Odds)


#Fauna
corOxPaleo <- cor(ext_ox$oxygen, ext_ox$pPaleo)
coOxrMod <- cor(ext_ox$oxygen, ext_ox$pMod)
corOxCam<- cor(ext_ox$oxygen, ext_ox$pCam)

corSlPaleo <- cor(timeline$SeaLevel, ext_ox$pPaleo)
coSlrMod <- cor(timeline$SeaLevel, ext_ox$pMod)
corSlCam<- cor(timeline$SeaLevel, ext_ox$pCam)


faunaAnalysis <- data.frame('Oxygen' = NA, 'Risk of exion' = NA, 'BodySize' = NA)


##Adarsh
analysismollusca <- data.frame('TieringvexOdds' = NA, 'MotilityvexOdds' = NA, 'FeedingvexOdds' = NA)
mollusca$tiering2<-mollusca$tiering
mollusca$tiering2[mollusca$tiering>=2] <-0
mollusca$motility2<-mollusca$motility
mollusca$motility2[mollusca$motility>=3]<-0
mollusca$motility2[mollusca$motility<3]<-1
mollusca$feeding2<-mollusca$feeding
mollusca$feeding2[mollusca$feeding==5]<-1
mollusca$feeding2[mollusca$feeding!=5]<-0
tier <-as.data.frame(subset(mollusca, mollusca$tiering2<=1))
mot <-as.data.frame(subset(mollusca, mollusca$motility2<=1))
feed <-as.data.frame(subset(mollusca, mollusca$feeding2<=1))
for (i in 1:length(timeline)){
  temp4<-subset(tier, tier$fad_age>=timeline$age_top[i] & tier$lad_age<=timeline$age_bottom[i])
  temp5<-subset(mot, mot$fad_age>=timeline$age_top[i] & mot$lad_age<=timeline$age_bottom[i])
  temp6<-subset(feed, feed$fad_age>=timeline$age_top[i] & feed$lad_age<=timeline$age_bottom[i])
  temp4$ex<-0
  temp5$ex<-0
  temp6$ex<-0
  temp4$ex[temp4$lad_age==timeline$age_bottom[i]]<-1
  temp5$ex[temp5$lad_age==timeline$age_bottom[i]]<-1
  temp6$ex[temp6$lad_age==timeline$age_bottom[i]]<-1
  glmod7<-glm(ex~tiering2,data=temp4, family="binomial", maxit=100)
  glmod8<-glm(ex~motility2,data=temp5, family="binomial", maxit=100)
  glmod9<-glm(ex~feeding2,data=temp6, family="binomial", maxit=100)
  analysismollusca$TieringvexOdds[i] <-summary(glmod7)$coefficients[2,1]
  analysismollusca$MotilityvexOdds[i] <-summary(glmod8)$coefficients[2,1]
  analysismollusca$FeedingvexOdds[i] <-summary(glmod9)$coefficients[2,1]
}
View(analysismollusca)




