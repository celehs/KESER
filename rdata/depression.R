rm(list = ls())

load("data_example_depression.rda")

depression <- list(
  cos_lst = cos_lst,
  X_full_lst = X_full_lst, 
  Y_full_lst = Y_full_lst,
  X_train_lst = X_train_lst, 
  Y_train_lst = Y_train_lst, 
  X_valid_lst = X_valid_lst, 
  Y_valid_lst = Y_valid_lst)

str(depression)

saveRDS(depression, "depression.rds")

proc.time()
