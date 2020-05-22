# Rscript dictionary.R > dictionary.txt 2>&1

rm(list = ls())

load("dict_combine_uniform_molei.Rdata")

dictionary <- dict.combine[, 1:2]
colnames(dictionary) <- c("name", "description")

head(dictionary)
tail(dictionary)

saveRDS(dictionary, "dictionary.rds")

proc.time()
