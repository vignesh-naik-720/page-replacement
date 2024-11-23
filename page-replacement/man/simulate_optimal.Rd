\name{simulate_optimal}
\alias{simulate_optimal}
\title{Simulate Optimal Page Replacement Algorithm}
\description{
  This function simulates the Optimal Page Replacement Algorithm. It takes a reference string and a number of frames and simulates the page loading process, replacing the page that will not be used for the longest time in the future when a page fault occurs.
}
\usage{
simulate_optimal(reference_string, num_frames)
}
\arguments{
  \item{reference_string}{A vector of integers representing the reference string (the sequence of page accesses).}
  \item{num_frames}{An integer specifying the number of frames (memory slots) available.}
}
\value{
  The function returns the total number of page faults that occurred during the simulation.
  The frames are displayed step by step, with page faults highlighted.
}
\details{
  The Optimal algorithm is the best possible page replacement algorithm because it replaces the page that will not be used for the longest period of time in the future. However, it requires knowledge of future page accesses, which is impractical in most real-world scenarios.
  The function simulates the algorithm by determining which page to replace based on the future reference string.
}
\examples{
reference_string <- c(7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2, 1, 2, 0, 1, 7, 0, 1)
num_frames <- 3
simulate_optimal(reference_string, num_frames)
}
\seealso{
  \code{\link{optimal_info}}
}
