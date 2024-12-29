# Control Charts in R

This repository provides an implementation of statistical **Control Charts** using R. Control charts are an essential part of **Statistical Process Control (SPC)**, enabling process stability and quality improvement by monitoring variability.

## Features

- Implements a variety of control charts:
  - **R-Chart**: Monitors process variability.
  - **S-Chart**: An alternative to the R-Chart, using standard deviation.
  - **X̄-Chart**: Monitors process mean.
  - **p-Chart**: Tracks proportion nonconforming.
  - **np-Chart**: Tracks the number of nonconforming items.
  - **c-Chart**: Monitors the number of nonconformities.
  - **u-Chart**: Tracks nonconformities per unit.
  - **Demerit Charts**: Evaluates multiple defect classes.
  - **Low Defect Charts**: For processes with very low defect rates.
- Handles variable sample sizes.
- Provides customizable chart options, including control limits and sample properties.
- Includes visualizations using `ggplot2` for enhanced interpretation.

This project is built on established principles and methodologies in Statistical Process Control (SPC). Key references include:

1. Montgomery, D. C. (2019). *Introduction to Statistical Quality Control*. Wiley.  
   A comprehensive resource on SPC, process improvement techniques, and the use of control charts for monitoring and managing process quality.

2. Shewhart, W. A. (1931). *Economic Control of Quality of Manufactured Product*. ASQ Quality Press.  
   A foundational text introducing the concept of control charts and their role in quality management.
