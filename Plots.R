##Plots
labelss <- c('Hir', 'Kat', 'San', 'Dar', 'Dap', 'Flo', 'Tre')
pdf(file = 'Bivalvia', height = 6, width = 6)
plot(boSi_ext$Log_Odds~timeline$age_mid, pch=16, main="Extinction Risk as a Function of Bivalvia Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Body Size", las=1,
     ylim=c(-3, 3), col="white",
     xlim=c(485,440))
abline(h=0, col="gray")
points(boSi_ext$Log_Odds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(boSi_ext$Log_Odds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, boSi_ext$Log_Odds+1.96*boSi_ext$SE,
       timeline$age_mid, boSi_ext$Log_Odds-1.96*boSi_ext$SE, 
       angle=90, length=0.001, lwd=1.5) 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend(465, 3, legend = c('Bivalves during the LOME', 'Bivalves prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = boSi_ext$Log_Odds[i], x1 = timeline$age_mid[i+1], 
         y1 = boSi_ext$Log_Odds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}
dev.off()

##Feeding
pdf(file = 'Feeding Plots', height = 6, width = 12)
par(mfrow=c(1,2))
#dev.new()
labelss <- c('Hir', 'Kat', 'San', 'Dar', 'Dap', 'Flo', 'Tre')
plot(analysismolluscs$FeedingvBDSOdds~timeline$age_mid, pch=16, main="Body Size as a Function of Mollusca Feeding Data",
     xaxt="n",xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Body size", las=1,
     ylim=c(-3, 3), col="white", xlim=c(485,440))
abline(h=0, col="gray")
points(analysismolluscs$FeedingvBDSOdds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$FeedingvBDSOdds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, analysismolluscs$FeedingvBDSOdds+1.96*analysismolluscs$FeedingvBDSOddsSE,
       timeline$age_mid, analysismolluscs$FeedingvBDSOdds-1.96*analysismolluscs$FeedingvBDSOddsSE, 
       angle=90, length=0.001, lwd=1.5, col = 'purple') 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('bottomright', inset = 0.02, legend = c('Molluscs during the LOME', 'Molluscs prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = analysismolluscs$FeedingvBDSOdds[i], x1 = timeline$age_mid[i+1], 
         y1 = analysismolluscs$FeedingvBDSOdds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

plot(analysismolluscs$FeedingvExtinctOdds~timeline$age_mid, pch=16, main="Extinction Risk as a Function of Mollusca Feeding Data",
     xaxt="n",xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Extinction Risk", las=1,
     ylim=c(-3, 3), col="white", xlim=c(485,440))
abline(h=0, col="gray")
points(analysismolluscs$FeedingvExtinctOdds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$FeedingvExtinctOdds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, analysismolluscs$FeedingvExtinctOdds+1.96*analysismolluscs$FeedingvExtinctOddsSE,
       timeline$age_mid, analysismolluscs$FeedingvExtinctOdds-1.96*analysismolluscs$FeedingvExtinctOddsSE, 
       angle=90, length=0.001, lwd=1.5, col = 'purple') 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('bottomright', inset = 0.02, legend = c('Molluscs during the LOME', 'Molluscs prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = analysismolluscs$FeedingvExtinctOdds[i], x1 = timeline$age_mid[i+1], 
         y1 = analysismolluscs$FeedingvExtinctOdds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

dev.off()


##Motility
pdf(file = 'Motility Plots', height = 6, width = 12)
par(mfrow=c(1,2))
#dev.new()
labelss <- c('Hir', 'Kat', 'San', 'Dar', 'Dap', 'Flo', 'Tre')
plot(analysismolluscs$MotilityvBDSOdds~timeline$age_mid, pch=16, main="Body Size as a Function of Mollusca Motility Data",
     xaxt="n",xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Body size", las=1,
     ylim=c(-3, 3), col="white", xlim=c(485,440))
abline(h=0, col="gray")
points(analysismolluscs$MotilityvBDSOdds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$MotilityvBDSOdds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, analysismolluscs$MotilityvBDSOdds+1.96*analysismolluscs$MotilityvBDSOddsSE,
       timeline$age_mid, analysismolluscs$MotilityvBDSOdds-1.96*analysismolluscs$MotilityvBDSOddsSE, 
       angle=90, length=0.001, lwd=1.5, col = 'purple') 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('bottomright', inset = 0.02, legend = c('Molluscs during the LOME', 'Molluscs prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = analysismolluscs$MotilityvBDSOdds[i], x1 = timeline$age_mid[i+1], 
         y1 = analysismolluscs$MotilityvBDSOdds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

plot(analysismolluscs$MotilityvExtinctOdds~timeline$age_mid, pch=16, main="Extinction Threat as a Function of Mollusca Motility Data",
     xaxt="n",xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Extinction Risk", las=1,
     ylim=c(-3, 3), col="white", xlim=c(485,440))
abline(h=0, col="gray")
points(analysismolluscs$MotilityvExtinctOdds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$MotilityvExtinctOdds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, analysismolluscs$MotilityvExtinctOdds+1.96*analysismolluscs$MotilityvExtinctOddsSE,
       timeline$age_mid, analysismolluscs$MotilityvExtinctOdds-1.96*analysismolluscs$MotilityvExtinctOddsSE, 
       angle=90, length=0.001, lwd=1.5, col = 'purple') 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('bottomright', inset = 0.02, legend = c('Molluscs during the LOME', 'Molluscs prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = analysismolluscs$MotilityvExtinctOdds[i], x1 = timeline$age_mid[i+1], 
         y1 = analysismolluscs$MotilityvExtinctOdds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}
dev.off()


###Combination Plot
pdf(file = 'Combination', height = 6, width = 12)
layout(matrix(c(1,1,1,2,3, 4), 2, 3, byrow = TRUE))
labelss <- c('Hir', 'Kat', 'San', 'Dar', 'Dap', 'Flo', 'Tre')
plot(boSi_ext$molluscaLog_Odds~timeline$age_mid, pch=16, main="Extinction Risk as a Function of Mollusca Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Extinction", las=1,
     ylim=c(-3, 3), col="white",
     xlim=c(485,440))
abline(h=0, col="gray")
points(boSi_ext$molluscaLog_Odds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(boSi_ext$molluscaLog_Odds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, boSi_ext$molluscaLog_Odds+1.96*boSi_ext$molluscaSE,
       timeline$age_mid, boSi_ext$molluscaLog_Odds-1.96*boSi_ext$molluscaSE, 
       angle=90, length=0.001, lwd=1.5) 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('topright', inset = 0.02, legend = c('Molluscs during the LOME', 'Molluscs prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = boSi_ext$molluscaLog_Odds[i], x1 = timeline$age_mid[i+1], 
         y1 = boSi_ext$molluscaLog_Odds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

plot(boSi_ext$Log_Odds~timeline$age_mid, pch=16, main="Extinction Risk as a Function of Bivalvia Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Extinction", las=1,
     ylim=c(-3, 3), col="white",
     xlim=c(485,440))
abline(h=0, col="gray")
points(boSi_ext$Log_Odds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(boSi_ext$Log_Odds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, boSi_ext$Log_Odds+1.96*boSi_ext$SE,
       timeline$age_mid, boSi_ext$Log_Odds-1.96*boSi_ext$SE, 
       angle=90, length=0.001, lwd=1.5, col = 'blue') 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('topright', inset = 0.02, legend = c('Bivalves during the LOME', 'Bivalves prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = boSi_ext$Log_Odds[i], x1 = timeline$age_mid[i+1], 
         y1 = boSi_ext$Log_Odds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

plot(boSi_ext$cephalapodLog_Odds~timeline$age_mid, pch=16, main="Extinction Risk as a Function of Cephalapoda Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Extinction", las=1,
     ylim=c(-3, 3), col="white",
     xlim=c(485,440))
abline(h=0, col="gray")
points(boSi_ext$cephalapodLog_Odds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(boSi_ext$cephalapodLog_Odds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, boSi_ext$cephalapodLog_Odds+1.96*boSi_ext$cephalapodSE,
       timeline$age_mid, boSi_ext$cephalapodLog_Odds-1.96*boSi_ext$cephalapodSE, 
       angle=90, length=0.001, lwd=1.5, col = 'red') 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('topright', inset = 0.02, legend = c('Cephalapods during the LOME', 'Cephalapods prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = boSi_ext$cephalapodLog_Odds[i], x1 = timeline$age_mid[i+1], 
         y1 = boSi_ext$cephalapodLog_Odds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

plot(boSi_ext$gastropodLog_Odds~timeline$age_mid, pch=16, main="Extinction Risk as a Function of Gastropoda Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="Regression Coefficient for Extinction", las=1,
     ylim=c(-3, 3), col="white",
     xlim=c(485,440))
abline(h=0, col="gray")
points(boSi_ext$gastropodLog_Odds[4:7]~timeline$age_mid[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(boSi_ext$gastropodLog_Odds[1:3]~timeline$age_mid[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(timeline$age_mid, boSi_ext$gastropodLog_Odds+1.96*boSi_ext$gastropodSE,
       timeline$age_mid, boSi_ext$gastropodLog_Odds-1.96*boSi_ext$gastropodSE, 
       angle=90, length=0.001, lwd=1.5, col = 'green') 
axis(side=1, at = timeline$age_mid, lab = labelss, las=1)
legend('topright', inset = 0.02, legend = c('Gastropods during the LOME', 'Gastropod prior to the LOME'), 
       col = c('black', 'black'), lty = 1, cex = 0.8, pch = c(17,2))
for (i in 1:(length(timeline)-1)) {
  arrows(x0 = timeline$age_mid[i], y0 = boSi_ext$gastropodLog_Odds[i], x1 = timeline$age_mid[i+1], 
         y1 = boSi_ext$gastropodLog_Odds[i+1], angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}
#dev.off()


