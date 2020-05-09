# Rscript depression.R > depression.txt 2>&1

rm(list = ls())

load("data_example_depression.rda")

depression <- list(
  # cos_lst = cos_lst,
  X_full_lst = X_full_lst[1:2], 
  Y_full_lst = Y_full_lst[1:2],
  X_train_lst = X_train_lst[1:2], 
  Y_train_lst = Y_train_lst[1:2], 
  X_valid_lst = X_valid_lst[1:2], 
  Y_valid_lst = Y_valid_lst[1:2])

str(depression)

saveRDS(depression, "depression.rds")

proc.time()
