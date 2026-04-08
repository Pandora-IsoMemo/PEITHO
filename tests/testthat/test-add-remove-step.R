mk_step <- function(entry, name) {
  new_workflowstep(
    entry = entry,
    command = "identity",
    name = name,
    label = name,
    comments = "",
    args = "",
    loop = ""
  )
}

test_that("add_step inserts at position and updates indices/current", {
  s1 <- mk_step(1L, "Step 1")
  s2 <- mk_step(2L, "Step 2")
  s3 <- mk_step(3L, "Step 3")

  wf <- suppressWarnings(new_workflow(
    name = "Add Step Test",
    steps = list(s1, s2, s3),
    input_list = list(),
    current = 2L,
    use_peitho_folder = FALSE
  ))

  new_s <- mk_step(99L, "Inserted")
  wf2 <- add_step(wf, new_step = new_s, position = 2L)

  expect_length(wf2$steps, 4)
  expect_equal(vapply(wf2$steps, function(s) s$entry, integer(1)), c(1L, 2L, 3L, 4L))
  expect_equal(vapply(wf2$steps, function(s) s$name, character(1)),
    c("Step 1", "Inserted", "Step 2", "Step 3")
  )
  expect_equal(wf2$current, 3L)
})

test_that("add_step validates input class and insertion position", {
  s1 <- mk_step(1L, "Step 1")
  wf <- suppressWarnings(new_workflow(
    name = "Add Step Validation",
    steps = list(s1),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  expect_error(
    add_step(wf, new_step = list(), position = 1L),
    "must be of class 'workflowstep'"
  )

  expect_error(
    add_step(wf, new_step = mk_step(2L, "Step 2"), position = 0L),
    "Position must be between"
  )

  expect_error(
    add_step(wf, new_step = mk_step(2L, "Step 2"), position = 3L),
    "Position must be between"
  )
})

test_that("remove_step removes step and updates indices/current", {
  s1 <- mk_step(1L, "Step 1")
  s2 <- mk_step(2L, "Step 2")
  s3 <- mk_step(3L, "Step 3")

  wf <- suppressWarnings(new_workflow(
    name = "Remove Step Test",
    steps = list(s1, s2, s3),
    input_list = list(),
    current = 3L,
    use_peitho_folder = FALSE
  ))

  wf2 <- remove_step(wf, position = 2L)

  expect_length(wf2$steps, 2)
  expect_equal(vapply(wf2$steps, function(s) s$entry, integer(1)), c(1L, 2L))
  expect_equal(vapply(wf2$steps, function(s) s$name, character(1)), c("Step 1", "Step 3"))
  expect_equal(wf2$current, 2L)

  wf3 <- remove_step(wf2, position = 2L)
  expect_true(is.na(wf3$current))
})

test_that("remove_step validates removal position", {
  s1 <- mk_step(1L, "Step 1")
  wf <- suppressWarnings(new_workflow(
    name = "Remove Step Validation",
    steps = list(s1),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  expect_error(remove_step(wf, position = 0L), "Step index must be between")
  expect_error(remove_step(wf, position = 2L), "Step index must be between")
})

test_that("update writes command changes to commands.json", {
  td <- tempfile("wf_update_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  jsonlite::write_json(list(alpha = "x"), file.path(td, "inputs.json"), auto_unbox = TRUE, pretty = TRUE)
  jsonlite::write_json(
    list(list(name = "Step 1", command = "identity", args = "x = @#*I*#@alpha@#*I*#@", loop = "no")),
    file.path(td, "commands.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  wf <- suppressWarnings(new_workflow(
    name = "Update Write Test",
    workflow_file_paths = workflow_file_paths(path = td)
  ))

  wf2 <- update(wf, step = 1, field = "Function", value = "toupper")
  expect_equal(wf2$steps[[1]]$command, "toupper")

  cmds <- jsonlite::fromJSON(file.path(td, "commands.json"), simplifyVector = FALSE)
  expect_equal(cmds[[1]]$command, "toupper")
})
