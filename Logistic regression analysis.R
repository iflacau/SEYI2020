###Logisitic Regression Analysis
bivalvia$biovol <- log10(bivalvia$calc_max_vol)
darBiv <- subset(bivalvia, bivalvia$exDar == 0 | bivalvia$exDar == 1)
sanBiv <- subset(bivalvia, bivalvia$exSan == 0 | bivalvia$exSan == 1)
katBiv <- subset(bivalvia, bivalvia$exKat == 0 | bivalvia$exKat == 1)
hirBiv <- subset(bivalvia, bivalvia$exHir == 0 | bivalvia$exHir == 1)
ordoBiv <- subset(bivalvia, bivalvia$exOrdo == 0 | bivalvia$exOrdo == 1)

bodySize.Extinction <- data.frame()

darReg <- glm((darBiv$exDar)~ (darBiv$biovol), family = binomial(logit), data = darBiv)

sanReg <- glm((sanBiv$exSan)~ (sanBiv$biovol), family = binomial(logit), data = sanBiv)

katReg <- glm((katBiv$exKat)~ (katBiv$biovol), family = binomial(logit), data = katBiv)

hirReg <- glm((hirBiv$exHir)~ (hirBiv$biovol), family = binomial(logit), data = hirBiv)

ordoReg <- glm((ordoBiv$exOrdo)~ (ordoBiv$biovol), family = binomial(logit), data = ordoBiv)

