
initialize_frames <- function(num_frames) {
  vector("list", num_frames)
}


is_page_in_frames <- function(page, frames) {
  page %in% frames
}


find_lru_index <- function(frames, lru_order) {
  lru_page <- lru_order[1]
  which(sapply(frames, function(x) x == lru_page))
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


simulate_lru <- function(reference_string, num_frames) {
  frames <- initialize_frames(num_frames)
  lru_order <- integer(0)
  page_faults <- 0

  for (i in seq_along(reference_string)) {
    current_page <- reference_string[i]
    reason <- ""
    is_fault <- FALSE

    if (is_page_in_frames(current_page, frames)) {
      lru_order <- lru_order[lru_order != current_page]
      lru_order <- c(lru_order, current_page)
    } else {
      page_faults <- page_faults + 1
      is_fault <- TRUE

      if (any(sapply(frames, is.null))) {
        empty_index <- which(sapply(frames, is.null))[1]
        frames[[empty_index]] <- current_page
        reason <- paste("Inserted page", current_page, "into an empty frame (frame", empty_index, ").")
      } else {
        lru_index <- find_lru_index(frames, lru_order)
        lru_page <- frames[[lru_index]]
        frames[[lru_index]] <- current_page
        reason <- paste("Replaced least recently used page", lru_page,
                        "(frame", lru_index, ") with page", current_page,
                        "because it was the oldest in LRU order.")
        lru_order <- lru_order[-1]
      }

      lru_order <- c(lru_order, current_page)
    }

    draw_frames(frames, step = i, current_page = current_page, is_fault = is_fault, reason = reason)
  }

  cat("\nSimulation Complete\n")
  cat("Total Page Faults:", page_faults, "\n")
}
lru_info <- function() {
  cat("LRU (Least Recently Used) Page Replacement Algorithm\n")
  cat("----------------------------------------------------\n")
  cat("Description: LRU replaces the page that has not been used for the longest period of time. The page that is used least recently is replaced first when a page fault occurs.\n")
  cat("Invented by: The concept of LRU was first introduced by Peter J. Denning in 1968.\n")
  cat("Algorithm Steps:\n")
  cat("1. Track the order of page accesses. The page that is accessed least recently is the one to be replaced.\n")
  cat("2. When a page is referenced, mark it as recently used and update the order.\n")
  cat("3. When a page fault occurs and the memory is full, replace the least recently used page.\n")
}


