boSi_ext <- data.frame('Period' = timeline$interval_name, 'Number_Genera' = NA, 'Mean' = NA, 'Fifth' = NA, 
                 'NinetyFifth' = NA, 'Percent_Extinct' = NA, 'Number_Extinct' = NA, 
                 'Age_Bottom' = NA, 'Age_Mid' = NA, 'Age_Top' = NA, 'Log_Odds' = NA,
                 'SE' = NA, 'Circ_Odds' = NA, 'Circ_SE' = NA, 'pVal' = NA)
bivalvia <- subset(mainData, mainData$class == 'Bivalvia')

mollusca[mollusca$circ == "closed",]$circ = 1
mollusca[mollusca$circ == "open",]$circ = 0

###Data Frame
for (i in 1:length(timeline)) {
  temp<- subset(bivalvia, bivalvia$fad_age >= timeline$age_top[i] & bivalvia$lad_age <= timeline$age_bottom[i])
  temp$ex <- 0
  temp$ex[temp$lad_age == timeline$age_bottom[i]] <- 1
  y <- temp$ex
  size <- log10(temp$calc_max_vol)
  
  myGlm <- glm(y ~ size, data = temp, family = 'binomial', maxit = 100)
  boSi_ext$Log_Odds [i] <- summary(myGlm)$coefficients[2,1]
  print(summary(myGlm))
  boSi_ext$SE[i] <- summary(myGlm)$coefficients[2,2]
  boSi_ext$pVal [i] <- summary(myGlm)$coefficients[2, 4]
  
  boSi_ext$Number_Genera[i] <- nrow(temp)
  boSi_ext$Age_Bottom[i] <- timeline$age_bottom[i]
  boSi_ext$Age_Mid[i] <- timeline$age_mid[i]
  boSi_ext$Age_Top[i] <- timeline$age_top[i]
  boSi_ext$Mean[i] <- mean(size)
  boSi_ext$NinetyFifth[i] <- quantile(size, 0.95)
  boSi_ext$Fifth[i] <- quantile(size, 0.05)
  boSi_ext$Number_Extinct[i] <- sum(y)
  boSi_ext$Percent_Extinct[i] <- (nrow(temp[temp$ex == 1, ])/nrow(temp)) *100
}
for (i in 1:length(timeline)) {
  temp<- subset(bivalvia, bivalvia$fad_age >= timeline$age_top[i] & bivalvia$lad_age <= timeline$age_bottom[i])
  temp$ex <- 0
  temp$ex[temp$lad_age == timeline$age_bottom[i]] <- 1
  y <- temp$ex
  size <- log10(temp$calc_max_vol)
  
  myGlm <- glm(y ~ size, data = temp, family = 'binomial', maxit = 100)
  boSi_ext$Log_Odds [i] <- summary(myGlm)$coefficients[2,1]
  print(summary(myGlm))
  boSi_ext$SE[i] <- summary(myGlm)$coefficients[2,2]
  boSi_ext$pVal [i] <- summary(myGlm)$coefficients[2, 4]
  
  boSi_ext$Number_Genera[i] <- nrow(temp)
  boSi_ext$Age_Bottom[i] <- timeline$age_bottom[i]
  boSi_ext$Age_Mid[i] <- timeline$age_mid[i]
  boSi_ext$Age_Top[i] <- timeline$age_top[i]
  boSi_ext$Mean[i] <- mean(size)
  boSi_ext$NinetyFifth[i] <- quantile(size, 0.95)
  boSi_ext$Fifth[i] <- quantile(size, 0.05)
  boSi_ext$Number_Extinct[i] <- sum(y)
  boSi_ext$Percent_Extinct[i] <- (nrow(temp[temp$ex == 1, ])/nrow(temp)) *100
}
for (i in 1:length(timeline)) {
  temp<- subset(mollusca, mollusca$fad_age >= timeline$age_top[i] & mollusca$lad_age <= timeline$age_bottom[i])
  temp$ex <- 0
  temp$ex[temp$lad_age == timeline$age_bottom[i]] <- 1
  temp2 <- subset(temp, temp$class == 'Cephalopoda') 
  temp3 <- subset(temp, temp$class == 'Gastropoda') 

  myGlm <- glm(temp$ex ~ (log10(temp$calc_max_vol)), data = temp2, family = 'binomial', maxit = 100)
  boSi_ext$molluscaLog_Odds [i] <- summary(myGlm)$coefficients[2,1]
  boSi_ext$molluscaSE[i] <- summary(myGlm)$coefficients[2,2]
  
  myGlm <- glm(temp2$ex ~ (log10(temp2$calc_max_vol)), data = temp2, family = 'binomial', maxit = 100)
  boSi_ext$cephalapodLog_Odds [i] <- summary(myGlm)$coefficients[2,1]
  boSi_ext$cephalapodSE[i] <- summary(myGlm)$coefficients[2,2]
  
  myGlm <- glm(temp3$ex ~ (log10(temp3$calc_max_vol)), data = temp3, family = 'binomial', maxit = 100)
  boSi_ext$gastropodLog_Odds [i] <- summary(myGlm)$coefficients[2,1]
  boSi_ext$gastropodSE[i] <- summary(myGlm)$coefficients[2,2]
}






cor.test(timeline$SeaLevel, boSi_ext$Log_Odds)
cor(timeline$SeaLevel, boSi_ext$Log_Odds)
View(boSi_ext)

