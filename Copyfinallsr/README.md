# LSR - Network Meta-Analysis Tool

An R Shiny application for automated Network Meta-Analysis (NMA) with interactive visualization and reporting capabilities.

## Overview

The aim of this MSc thesis is to develop an interactive tool using the RShiny framework, which will support living network meta-analysis for data related to Chronic Lymphocytic Leukemia (CLL). The tool will allow data input through manual upload. The data will be automatically standardized and transformed into the appropriate format for meta-analysis, and the tool will proceed to perform the analysis.
The expected results include the development of a fully functional tool that adheres to the principles of living meta-analysis and network meta-analysis. It will offer features such as inconsistency exploration, transitivity assessment, and the generation of visualizations including forest plots and funnel plots. Additionally, the tool will support functionalities for subgroup analysis and meta regression.
This thesis aspires to deliver an innovative and practical tool that enhances the transparency, reproducibility, and usability of evidence synthesis in the field of therapeutic decision-making for CLL.
It is not the latest edition so there may be some bugs.
## Features

### Analysis Models
- Classical Network Meta-Analysis (NMA)
- Component Network Meta-Analysis (cNMA) - Additive model
- Component Network Meta-Analysis (cNMA) - With interactions

### Visualizations
- **Forest plots** - Treatment effect comparisons
- **Network plots** - Study network visualization  
- **League tables** - Pairwise comparison summaries
- **Funnel plot** - Small study effect

### Statistical Features
- Inconsistency checks
- Treatment ranking (P-scores)
- Summary statistics
- Missing data analysis (% NA reporting)
- Node merging for component families

### Data Management
- Import from Excel (.xlsx)
- Import from CSV files
- REDCap API integration for direct data import
- Interactive data exploration
- Missing value detection and reporting

### Export Options
- HTML reports
- Word documents (.docx)

## Installation

```bash
git clone https://github.com/ch-damianidis/lsr.git
cd lsr
```

### R Dependencies

```r
# Install required packages
install.packages(c("shiny", "netmeta", "readxl", "openxlsx", 
                   "ggplot2", "plotly", "DT", "rmarkdown","igraph","meta"))
```

## Usage

### Running the Application

```r
# Clone the repository first
# git clone https://github.com/ch-damianidis/lsr.git

# Navigate to the project directory and run:
library(shiny)
shiny::runApp("app.R")

# Or if running from another directory:
shiny::runApp("path/to/lsr/app.R")
```

### Data Input Format

The tool accepts data in the following formats:
- Excel files (.xlsx) with treatment comparisons
- CSV files with standard NMA data structure
- REDCap API connection for automated data retrieval

### Example Workflow

1. **Import Data**: Upload Excel/CSV file or connect to REDCap
2. **Data Review**: Check for missing values and data quality
3. **Select Model**: Choose between classical NMA or component NMA
4. **Run Analysis**: Execute the meta-analysis
5. **Explore Results**: Interactive plots and statistics
6. **Export**: Generate HTML or Word reports

## System Requirements

- R version 4.0 or higher
- RStudio (recommended for development)
- Modern web browser for Shiny interface

## Output

The tool generates:
- Interactive dashboard with all analysis results
- Downloadable reports in HTML and Word formats
- Publication-ready plots and tables


## Contact

For questions, issues, or contributions, please open an issue on GitHub.
