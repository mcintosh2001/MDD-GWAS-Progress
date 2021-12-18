library("tidyverse")
library("readxl")
require("ggrepel")
setwd("/Users/mcintosh/Desktop/")

gwasMDD <- read_excel(path = "MDD-GWAS.xlsx" )

# Progress in psych genetics over time
pdf("MDDGWAS_progress.pdf")
ggplot(gwasMDD, aes(x=Ncases,y=Nhits,label=fauthor)) +
  theme(panel.background = element_rect(fill="white", 
                                        color="white", 
                                        linetype="solid"), 
        panel.grid.major.y = element_line(colour = "grey"), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()  ) +
  
  geom_point() +
  geom_smooth(method = "loess", formula = (y~x), se=FALSE) +
  
  geom_label_repel(min.segment.length = 0.5,
                   max.overlaps = Inf,
                   label.size=0,
                   label.padding=0.2,
                   label.r=0.2,
                   size=3) +
coord_cartesian(xlim=c(0,500000), ylim=c(0, 500)) +
  
labs(title = "MDD GWAS Progress with Increasing Case Numbers",
     subtitle=NULL,
     x="Number of Depressed Cases",
     y="Number of Significant Loci")
dev.off()

# Including PGC3
pdf("all.pdf")
ggplot(gwasMDD, aes(x=Ncases,y=Nhits )) +
  theme(panel.background = element_rect(fill="white", 
                                        color="white", 
                                        linetype="solid"), panel.grid.major.y = element_line(colour = "grey"), ) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette="Dark2") +
  geom_point(size=2, shape = 8) +
  geom_smooth(method = "loess", formula = (y~x), se=FALSE) +
  geom_text_repel(aes(label=fauthor), 
                  min.segment.length = unit(0, 'lines'),
                  force=1, 
                  nudge_x = 2,
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(1.5,'lines'))
dev.off()

