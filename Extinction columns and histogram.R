bivalvia$exDar <- 0
bivalvia$exSan <- 0
bivalvia$exKat <- 0
bivalvia$exHir<- 0
bivalvia$exOrdo <- 0
bivalvia$exBefore <- 0

for (i in 1:nrow(bivalvia)) {
   if (bivalvia$lad_age[i] <= 445.2 & bivalvia$lad_age[i] >= 443.4) {
    bivalvia$exHir[i] <- 1
   } 
  if (bivalvia$lad_age[i] <= 453.3 & bivalvia$lad_age[i] > 445.2) {
    bivalvia$exKat[i] <- 1
    bivalvia$exHir [i]<- NA
  }
  
  if (bivalvia$lad_age[i] <= 458.4 & bivalvia$lad_age[i] > 453.0) {
    bivalvia$exSan[i] <- 1
    bivalvia$exKat[i] <-NA
    bivalvia$exHir [i]<- NA
  } 
  
  if (bivalvia$lad_age[i] <= 467.3 & bivalvia$lad_age[i] > 458.4) {
    bivalvia$exDar[i] <- 1
    bivalvia$exSan[i] <- NA
    bivalvia$exKat[i] <- NA
    bivalvia$exHir [i] <- NA
  } 
  if (bivalvia$lad_age[i] >= 443.4 & bivalvia$lad_age[i] <= 467.3) {
    bivalvia$exOrdo[i] <- 1
  }
  if (bivalvia$lad_age[i] > 467.3 ) {
    bivalvia$exBefore[i] <- 1
    bivalvia$exDar[i] <- NA
    bivalvia$exSan[i] <- NA
    bivalvia$exKat[i] <- NA
    bivalvia$exHir[i] <- NA
  }
}
exData <- data.frame(bivalvia$exDar, bivalvia$exSan, bivalvia$exKat, bivalvia$exHir, 
                     bivalvia$exOrdo, bivalvia$exBefore)
hist(bivalviaStudied$lad_age, breaks=c(467.3, 458.4, 453.0, 445.2, 443.4), freq=TRUE, labels = c('Hi', 'Ka', 'Sa', 'Da'))