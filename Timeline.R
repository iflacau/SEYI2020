##timeline
#install.packages("ggplot2")
pdf(file = 'timeline', height = 3.5, width = 10)
dfera<- data.frame('lab' = c('Paleozoic'), 
                   'x1' = c( 500.0), 'x2' = c(428),
                   'y1' =1, 'y2' = 1.5, 
                   'col' = c('firebrick1'))
dfperiod <- data.frame('lab' = c( 'Silurian', 'Ordovician', 'Cambrian'),
                    'x1' = c( 443.4, 485.4, 500.0),
                    'x2' = c(428, 443.4, 485.4), 
                    'y1' = 1.5, 'y2' = 2, 
                    'col' = c('coral2', 'coral3', 'coral4'))

dfordo <- data.frame('lab' = c('Hir', 'Kat', 'San', 'Dar', 'Dap', 'Flo', 'Tre'),
                     'x1' =timeline$age_bottom, 'x2' =timeline$age_top, 
                     'y1' = 2,'y2' = 2.5, 
                     'col' = c('pink', 'lightpink', 'pink1', 'lightpink1', 'lightpink2', 'pink2', 'pink3'))
ggplot() + 
  scale_x_reverse(name="Million Years Ago", expand = c(0,0)) + 
  scale_y_continuous(name="",  expand = c(0,0), breaks = c(1.25, 1.75, 2.25), labels = c('Era', 'Period', 'Stage')) +
  geom_rect(data=dfera, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill= dfera$col, color="black", alpha=0.5) +
  geom_text(data=dfera, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=lab), size=4) +
  geom_rect(data=dfperiod, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = dfperiod$col ,color="black", alpha=0.5) +
  geom_text(data=dfperiod, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=lab), size=4) +
  geom_rect(data=dfordo, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill= dfordo$col, color="black", alpha=0.5) +
  geom_text(data=dfordo, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=lab), size=4) +
  theme_classic() 
dev.off()


  

