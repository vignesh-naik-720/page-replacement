
initialize_frames <- function(num_frames) {
  vector("list", num_frames)
}


is_page_in_frames <- function(page, frames) {
  page %in% frames
}


draw_frames <- function(frames, step, current_page, is_fault, reason) {
  cat("\nStep:", step, "| Current Page:", current_page, "| Page Fault:", ifelse(is_fault, "Yes", "No"), "\n")
  if (is_fault) cat("Reason:", reason, "\n")

  for (frame in frames) cat("+-----+ ", sep = "")
  cat("\n")

  for (frame in frames) {
    if (!is.null(frame)) cat(sprintf("| %-3d | ", frame), sep = "")
    else cat("|     | ", sep = "")
  }
  cat("\n")

  for (frame in frames) cat("+-----+ ", sep = "")
  cat("\n")
  Sys.sleep(0.5)
}


simulate_fifo <- function(reference_string, num_frames) {
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


    draw_frames(frames, step = i, current_page = current_page, is_fault = is_fault, reason = reason)
  }

  cat("\nSimulation Complete\n")
  cat("Total Page Faults:", page_faults, "\n")
}
fifo_info <- function() {
  cat("FIFO (First-In-First-Out) Page Replacement Algorithm\n")
  cat("----------------------------------------------------\n")
  cat("Description: FIFO is the simplest page replacement algorithm. In FIFO, the page that has been in memory the longest (the oldest) is replaced when a new page needs to be loaded.\n")
  cat("Invented by: This algorithm doesn't have a specific inventor, as it's a straightforward approach to managing memory.\n")
  cat("Algorithm Steps:\n")
  cat("1. Keep a queue of pages in memory.\n")
  cat("2. When a page needs to be loaded, replace the page that has been in memory the longest (the first in the queue).\n")
  cat("3. If there is a page fault, load the new page into memory, and move the oldest page out.\n")
}


