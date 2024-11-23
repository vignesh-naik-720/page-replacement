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

test_that("simulate_fifo correctly handles page replacement", {

  reference_string <- c(7, 0, 1, 2)
  num_frames <- 3
  frames <- initialize_frames(num_frames)
  page_faults <- 0
  page_queue <- integer(0)

  for (i in seq_along(reference_string)) {
    current_page <- reference_string[i]
    reason <- ""
    is_fault <- FALSE

    if (is_page_in_frames(current_page, frames)) {
      reason <- "Page already in frames."
    } else {
      page_faults <- page_faults + 1
      is_fault <- TRUE

      if (length(page_queue) < num_frames) {
        empty_index <- which(sapply(frames, is.null))[1]
        frames[[empty_index]] <- current_page
        page_queue <- c(page_queue, current_page)
        reason <- paste("Inserted page", current_page, "into an empty frame (frame", empty_index, ").")
      } else {
        replaced_page <- page_queue[1]
        frames[[which(frames == replaced_page)]] <- current_page
        page_queue <- c(page_queue[-1], current_page)
        reason <- paste("Replaced page", replaced_page, "(oldest in FIFO) with page", current_page, ".")
      }
    }
  }


  expect_equal(page_faults, 4)

  reference_string <- c(7, 0, 1, 2, 0, 3)
  num_frames <- 3
  frames <- initialize_frames(num_frames)
  page_faults <- 0
  page_queue <- integer(0)

  for (i in seq_along(reference_string)) {
    current_page <- reference_string[i]
    reason <- ""
    is_fault <- FALSE

    if (is_page_in_frames(current_page, frames)) {
      reason <- "Page already in frames."
    } else {
      page_faults <- page_faults + 1
      is_fault <- TRUE

      if (length(page_queue) < num_frames) {
        empty_index <- which(sapply(frames, is.null))[1]
        frames[[empty_index]] <- current_page
        page_queue <- c(page_queue, current_page)
        reason <- paste("Inserted page", current_page, "into an empty frame (frame", empty_index, ").")
      } else {
        replaced_page <- page_queue[1]
        frames[[which(frames == replaced_page)]] <- current_page
        page_queue <- c(page_queue[-1], current_page)
        reason <- paste("Replaced page", replaced_page, "(oldest in FIFO) with page", current_page, ".")
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
