rm(list=ls())
library(wordcloud)
library(RColorBrewer)
library(dplyr)
setwd("/Users/chuanhong/Dropbox/Embedding selection shared with Ming/from_Chuan_0518/")
load("feature.select.res.Rdata")

set.seed(1234)
pdf(paste(file=paste0("wordcloud.pdf", sep="")),width=12,height=12)
wordcloud(words = c(res$feature_desc), freq = round(c(as.numeric(res$Coef))*100,0), random.order=F,
            colors=brewer.pal(8, "Dark2"),scale = c(4, 0.2),rot.per = 0)
dev.off()

 

