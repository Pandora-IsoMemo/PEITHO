# download and install the repo from git
devtools::install_github("Pandora-IsoMemo/PEITHO")

# load PEITHO package
library(PEITHO)

# load with devtools only for development purposes
#devtools::load_all()

# Create a workflow object from files
# - Set a custom path to the workflow directory or
# - if an empty path is given use the example included with PEITHO
my_wf <- new_workflow(
  workflow_file_paths = workflow_file_paths(path = "")
)

# save the workflow as zip file
zipfile_path <- "./my_workflow.peitho"
save_as_zip(my_wf, file = zipfile_path)

# run the workflow from step 1 to 5
my_run_1 <- run(my_wf, from = 1, to = 5)

# see results
length(my_run_1$state$last_result) # 3 outputs from 3 urls

# print the first n_char of each of the last result
PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)

# import a workflow from zip file into the folder extract_dir
# - before importing one may change the steps in the commands.json and inputs.json files
# in the zip to try different workflows
extract_dir <- "./inst/scripts/peitho_files/imported"
res <- import_bundle_zip(
  zipfile = zipfile_path,
  extract_dir = extract_dir,
  keep_dir = TRUE
)

# setup workflow from imported files using the path to the extracted folder
my_wf_imported <- new_workflow(
  workflow_file_paths = workflow_file_paths(path = extract_dir)
)

# run the imported workflow now from step 1 to 4
my_run_2 <- run(my_wf_imported, from = 1, to = 4)

# see results
length(my_run_2$state$last_result) # 3 outputs from 3 urls

# print the first n_char of each of the last result
PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)

