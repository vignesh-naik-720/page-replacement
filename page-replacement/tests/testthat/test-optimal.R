library(testthat)

test_that("initialize_frames correctly initializes frames", {
  num_frames <- 3
  frames <- initialize_frames(num_frames)

  expect_equal(length(frames), num_frames)
  expect_true(all(sapply(frames, is.null)))
})

test_that("is_page_in_frames correctly checks for page presence", {
  frames <- c(7, 0, 1)

  expect_true(is_page_in_frames(0, frames))
  expect_true(is_page_in_frames(7, frames))

  expect_false(is_page_in_frames(2, frames))
  expect_false(is_page_in_frames(3, frames))
})

test_that("find_optimal_index correctly finds the optimal page index", {
  frames <- c(7, 0, 1)
  reference_string <- c(7, 0, 1, 2, 3, 4, 5)
  current_index <- 3

  optimal_index <- find_optimal_index(frames, reference_string, current_index)

  expect_equal(optimal_index, 1)
})

test_that("simulate_optimal correctly handles page replacement", {

  reference_string <- c(7, 0, 1, 2)
  num_frames <- 3
  frames <- initialize_frames(num_frames)
  page_faults <- 0

  for (i in seq_along(reference_string)) {
    current_page <- reference_string[i]
    reason <- ""
    is_fault <- FALSE

    if (is_page_in_frames(current_page, frames)) {
      reason <- "Page already in frames."
    } else {
      page_faults <- page_faults + 1
      is_fault <- TRUE

      if (any(sapply(frames, is.null))) {
        empty_index <- which(sapply(frames, is.null))[1]
        frames[[empty_index]] <- current_page
        reason <- paste("Inserted page", current_page, "into an empty frame (frame", empty_index, ").")
      } else {
        optimal_index <- find_optimal_index(frames, reference_string, i)
        optimal_page <- frames[[optimal_index]]
        frames[[optimal_index]] <- current_page
        reason <- paste("Replaced page", optimal_page, "(frame", optimal_index, ") with page", current_page, "using Optimal algorithm.")
      }
    }
  }


  expect_equal(page_faults, 4)
s
  reference_string <- c(7, 0, 1, 2, 0, 3)
  num_frames <- 3
  frames <- initialize_frames(num_frames)
  page_faults <- 0

  for (i in seq_along(reference_string)) {
    current_page <- reference_string[i]
    reason <- ""
    is_fault <- FALSE

    if (is_page_in_frames(current_page, frames)) {
      reason <- "Page already in frames."
    } else {
      page_faults <- page_faults + 1
      is_fault <- TRUE

      if (any(sapply(frames, is.null))) {
        empty_index <- which(sapply(frames, is.null))[1]
        frames[[empty_index]] <- current_page
        reason <- paste("Inserted page", current_page, "into an empty frame (frame", empty_index, ").")
      } else {
        optimal_index <- find_optimal_index(frames, reference_string, i)
        optimal_page <- frames[[optimal_index]]
        frames[[optimal_index]] <- current_page
        reason <- paste("Replaced page", optimal_page, "(frame", optimal_index, ") with page", current_page, "using Optimal algorithm.")
      }
    }
  }

  expect_equal(page_faults, 5)
})

test_that("draw_frames correctly formats the frame output", {
  frames <- c(7, 0, 1)

  output <- capture.output(draw_frames(frames, step = 1, current_page = 7, is_fault = TRUE, reason = "Inserted page 7"))

  expect_true(any(grepl("Step:", output)))
  expect_true(any(grepl("Current Page:", output)))
  expect_true(any(grepl("Page Fault:", output)))
  expect_true(any(grepl("|  7  |", output)))
})
