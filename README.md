# Page Replacement Algorithm Package

This repository contains the `page-replacement` R package, which simulates page replacement algorithms commonly used in operating systems.

## Features

- Simulate popular page replacement algorithms:
  - **FIFO (First-In-First-Out)**
  - **LRU (Least Recently Used)**
  - **Optimal Page Replacement**
- Visualize page faults and memory frame updates step-by-step.
- Learn about the algorithms through included informational functions.

## Installation

To install the package, use the `.tar.gz` file provided in this repository. Run the following command in R:

```
install.packages("https://github.com/vignesh-naik-720/page-replacement/raw/main/PageReplacement_1.0.0.tar.gz", repos = NULL, type = "source")
```
```
library(page-replacement)
```
```
reference_string <- c(7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2)
num_frames <- 3
simulate_fifo(reference_string, num_frames)

reference_string <- c(7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2)
num_frames <- 3
simulate_lru(reference_string, num_frames)

reference_string <- c(7, 0, 1, 2, 0, 3, 0, 4, 2, 3, 0, 3, 2)
num_frames <- 3
simulate_optimal(reference_string, num_frames)
```
```
fifo_info()
lru_info()
optimal_info()
```
License
This project is licensed under the MIT License. See the LICENSE file for details.

Author
Created by Vignesh Naik
Email: 720co.main@gmail.com
