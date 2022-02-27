sizeData <- read.delim(file='supplementary_data_file.txt')
dim(sizeData) # there are 17208 rows and 14 columns
head(sizeData)
timescale <- read.delim(file='timescale.txt')

chordata <- subset(sizeData, sizeData$phylum == 'Chordata')
arthropoda <- subset(sizeData, sizeData$phylum == 'Arthropoda')
mollusca <- subset(sizeData, sizeData$phylum == 'Mollusca')
brachiopoda <- subset(sizeData, sizeData$phylum == 'Brachiopoda')
echinodermata <- subset(sizeData, sizeData$phylum == 'Echinodermata')

plot(1, type="n", xlab="Geologic Time (MA)", ylab='Biovolume'~('log'[10]~'mm'^3), xlim=c(max(sizeData$fad_age), min(sizeData$lad_age)), 
     ylim=c(min(sizeData$log10_volume), max(sizeData$log10_volume)))
for (i in 1:nrow(chordata)) {
  segments(x0 = chordata$fad_age[i], y0 = chordata$log10_volume[i], x1 = chordata$lad_age[i], y1 = chordata$log10_volume[i], col = "purple")
}
for (i in 1:nrow(arthropoda)) {
  segments(x0 = arthropoda$fad_age[i], y0 = arthropoda$log10_volume[i], x1 = arthropoda$lad_age[i], y1 = arthropoda$log10_volume[i], col = "red")
}
for (i in 1:nrow(mollusca)) {
  segments(x0 = mollusca$fad_age[i], y0 = mollusca$log10_volume[i], x1 = mollusca$lad_age[i], y1 = mollusca$log10_volume[i], col = "skyblue")
}
for (i in 1:nrow(brachiopoda)) {
  segments(x0 = brachiopoda$fad_age[i], y0 = brachiopoda$log10_volume[i], x1 = brachiopoda$lad_age[i], y1 = brachiopoda$log10_volume[i], col = "yellow")
}
for (i in 1:nrow(echinodermata)) {
  segments(x0 = echinodermata$fad_age[i], y0 = echinodermata$log10_volume[i], x1 = echinodermata$lad_age[i], y1 = echinodermata$log10_volume[i], col = "gray")
}

#for (i in 1:nrow(sizeData)) {
 # segments(x0 = sizeData$fad_age[i], y0 = sizeData$log10_volume[i], x1 = sizeData$lad_age[i], y1 = sizeData$log10_volume[i], col = "black")
#}

meanSize <- vector(mode = 'numeric', length = nrow(timescale))
quantile5 <- vector(mode = 'numeric', length = nrow(timescale))
quantile95 <- vector(mode = 'numeric', length = nrow(timescale))
for (i in 1:nrow(timescale)) {
  tempSizes <- subset(mollusca, fad_age > timescale$age_mid[i] & lad_age < 
                        timescale$interval_name[i])
  meanSize[i] <- mean(tempSizes$log10_volume) 
  quantile5[i] <- quantile(tempSizes$log10_volume, 0.05)
  quantile95[i] <- quantile(tempSizes$log10_volume, 0.95)
}


trends <- data.frame(meanSize, quantile5, quantile95)

lines(timescale$age_bottom, meanSize, type = "solid", col = "black", lwd = 1.5)
lines(timescale$age_bottom, quantile5, type = "solid", col = "black", lwd = 1)
lines(timescale$age_bottom, quantile95, type = "solid", col = "black", lwd = 1)




