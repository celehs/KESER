rm(list = ls())

load("dat.Rdata")

embed <- list(
  cos.sim = all.cos,
  x.full = x.A, 
  x.train = x.sam,
  x.valid = x.sam2, 
  y.full = y.A, 
  y.train = y.sam, 
  y.valid = y.sam2)
str(embed)

saveRDS(embed, "embed.rds")
