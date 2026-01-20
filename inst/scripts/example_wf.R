devtools::load_all()

my_wf <- new_workflow()

save_as_zip(my_wf, file = "my_workflow.zip")

my_run_1 <- run(my_wf, from = 1, to = 6)

length(my_run_1$state$last_result) # 3 outputs from 3 urls

# print the first n_char of each of the last result
PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)

my_run_2 <- run(my_wf, from = 1, to = 5)

length(my_run_2$state$last_result) # 3 outputs from 3 urls

# print the first n_char of each of the last result
PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)

