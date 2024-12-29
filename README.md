# Control Charts in R

This repository provides an implementation of statistical **Control Charts** using R. Control charts are an essential part of **Statistical Process Control (SPC)**, enabling process stability and quality improvement by monitoring variability.

## Features

This repository offers a robust implementation of statistical control charts in R, with a focus on flexibility, accuracy, and clarity. Key features include:

### 1. **Comprehensive Control Charts**
The following control charts are implemented to monitor process variability and ensure quality control:
- **R-Chart**: Monitors process variability using the range of samples.
- **S-Chart**: An alternative to the R-Chart, based on sample standard deviations for more precise variability measurement.
- **X̄-Chart**: Tracks the process mean over time to detect shifts in the average quality level.
- **p-Chart**: Monitors the fraction of defective items in a process.
- **np-Chart**: Tracks the number of defective items per sample.
- **c-Chart**: Monitors the total number of defects in a process.
- **u-Chart**: Tracks defects per unit, useful for variable sample sizes.
- **Demerit Charts**: Analyzes defects by severity classes (e.g., critical, major, minor).
- **Low Defect Levels Chart**: Handles processes with extremely low defect rates by analyzing the time between defects.

### 2. **Dynamic Visualization**
All control charts are visualized using `ggplot2` for clear and intuitive interpretation. Key chart elements include:
- **Center Line (CL)**: Represents the expected average value of the process when it is in control.
- **Upper Control Limit (UCL)**: Indicates the maximum threshold, typically set at three standard deviations above the center line.
- **Lower Control Limit (LCL)**: Indicates the minimum threshold, typically set at three standard deviations below the center line.

These control limits ensure that any significant deviation from the process norm is flagged for investigation. The limits can be adjusted based on specific process requirements.

### 3. **Variable Sample Size Handling**
The repository supports control charts with varying sample sizes. Custom calculations are used to dynamically adjust control limits for each sample, ensuring accurate monitoring.

### 4. **Customizable Parameters**
Users can define key parameters such as:
- Constants for control limit calculations (e.g., `D3`, `D4`, `A2`, `B3`, `B4`).
- Sample size and observed values.
- Statistical assumptions like mean (`µ`) and standard deviation (`σ`).

### 5. **R Code Integration**
The repository includes an `R` implementation of all chart types, allowing users to:
- Create charts programmatically.
- Automate the monitoring process.
- Integrate charts into broader statistical analysis workflows.

### 6. **Error Handling and Validation**
The implementation validates input data and parameters to ensure that:
- Required columns are present in the dataset.
- Necessary parameters are provided.
- Calculations are performed only on valid data.

### 7. **Flexibility for Advanced Use Cases**
The repository supports advanced use cases such as:
- Multilevel defect classification using demerit charts.
- Handling nonconforming units with variable sample sizes.
- Monitoring rare events using charts for low defect levels.

By combining theoretical rigor with practical implementation, this repository is a valuable tool for statistical quality control in manufacturing, healthcare, and other domains where process stability and quality are critical.

This project is built on established principles and methodologies in Statistical Process Control (SPC). Key references include:

1. Montgomery, D. C. (2019). *Introduction to Statistical Quality Control*. Wiley.  
   A comprehensive resource on SPC, process improvement techniques, and the use of control charts for monitoring and managing process quality.

2. Shewhart, W. A. (1931). *Economic Control of Quality of Manufactured Product*. ASQ Quality Press.  
   A foundational text introducing the concept of control charts and their role in quality management.
