##Oxygen x Extinction

ext_ox <- data.frame('Period' = timeline$interval_name, 'Number_Genera' = NA, 
  'Mean_Size' = NA, 'Mean_Oxygen_Overall' = mean(timeline$pO2), 'Fifth' = NA, 'NinetyFifth' = NA, 'Percent_Extinct' = NA, 
  'Number_Extinct' = NA, 'Age_Bottom' = NA, 'Age_Mid' = NA, 'Age_Top' = NA, 'oxygen' = NA)

for (i in 1:length(timeline)) {
  temp<- subset(bivalvia, bivalvia$fad_age >= timeline$age_top[i] & bivalvia$lad_age <= timeline$age_bottom[i])
  temp$ex <- 0
  temp$ex[temp$lad_age == timeline$age_bottom[i]] <- 1
  y <- temp$ex
  size <- log10(temp$calc_max_vol)
  ext_ox$Number_Genera[i] <- nrow(temp)
  ext_ox$Age_Bottom[i] <- timeline$age_bottom[i]
  ext_ox$Age_Mid[i] <- timeline$age_mid[i]
  ext_ox$Age_Top[i] <- timeline$age_top[i]
  ext_ox$Mean_Size[i] <- mean(size)
  ext_ox$NinetyFifth[i] <- quantile(size, 0.95)
  ext_ox$Fifth[i] <- quantile(size, 0.05)
  ext_ox$Number_Extinct[i] <- sum(y)
  ext_ox$Percent_Extinct[i] <- (nrow(temp[temp$ex == 1, ])/nrow(temp)) *100
  ext_ox$oxygen[i] <- timeline$pO2[i]
}

View(ext_ox)

plot(ext_ox$oxygen, ext_ox$Percent_Extinct, xlab = 'Percent Oxygen', ylab = 'Extinction Rate')
dev.new()
plot(ext_ox$oxygen, ext_ox$Mean_Size, xlab ='Percent Oxygen', ylab = 'Mean Size')
plot(ext_ox$oxygen, ext_ox$Fifth, xlab ='Percent Oxygen', ylab = 'Fifth Percentile Size')
plot(ext_ox$oxygen, ext_ox$NinetyFifth, xlab ='Percent Oxygen', ylab = 'Ninety Fifth Percentile Size')

lmPE <- lm(ext_ox$Percent_Extinct ~ ext_ox$oxygen, data= ext_ox)
lmMS <- lm(ext_ox$Mean_Size ~ ext_ox$oxygen, data= ext_ox)
lm5 <- lm(ext_ox$Fifth ~ ext_ox$oxygen, data= ext_ox)
lm95(ext_ox$NinetyFifth ~ ext_ox$oxygen, data= ext_ox)
summary(lmPE)
summary(lmMS)
summary(lm5)
summary(lm95)
coeffOx <- data.frame('Percent_Extinct' = NA, 'Mean_Size' = NA, 'Fifth' = NA, 'NinetyFifth' = NA)