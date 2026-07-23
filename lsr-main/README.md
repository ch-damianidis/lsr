# Living Network Meta-Analysis Tool (LSR)

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Framework-orange.svg)](https://shiny.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

An interactive R Shiny application for performing frequentist **Network Meta-Analysis (NMA)** with support for component models, inconsistency diagnostics, treatment ranking, and automated reporting.

Developed as part of an MSc thesis in **Health Statistics and Data Analytics** at the Aristotle University of Thessaloniki.

---

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Project Structure](#project-structure)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Data Format](#data-format)
- [Analysis Models](#analysis-models)
- [Node Merging](#node-merging)
- [Report Generation](#report-generation)
- [Testing](#testing)
- [Technical Notes](#technical-notes)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)
- [Citation](#citation)
- [License](#license)
- [Contact](#contact)

---

## Overview

This tool streamlines the process of conducting a frequentist network meta-analysis using study-level data. It provides an integrated workflow — from data import to model fitting, interactive visualization, and report generation — designed with reproducibility and transparency in mind.

The application is built around the [`netmeta`](https://cran.r-project.org/package=netmeta) R package and supports:

- **Classical NMA** for treatment-level comparisons
- **Component NMA (CNMA)** for combination treatments (additive and interaction models)
- **Disconnected network** handling via `discomb()`
- **File upload** (CSV and Excel) for user-provided datasets
- **Reference treatment** selection for customized comparisons

---

## Features

### Data Management

| Feature | Description |
|---|---|
| **File upload** | Import your own data via CSV (`.csv`) or Excel (`.xlsx`) upload |
| **Bundled dataset** | Example CLL pairwise data loaded by default |
| **Auto-detection** | Distinguishes arm-level from pairwise data automatically |
| **Pairwise conversion** | Arm-level data converted via `netmeta::pairwise()` |
| **Data preview** | Interactive table with up to 50 rows |
| **Summary statistics** | Study count, treatment count, arm count, missingness % |
| **Column validation** | Required columns checked before analysis runs |

### Analysis Models

| Model | Function | Description |
|---|---|---|
| **Simple NMA** | `netmeta()` | Classical network meta-analysis |
| **Additive CNMA** | `netcomb()` | Component NMA assuming additive treatment effects |
| **Interaction CNMA** | `netcomb()` + custom C-matrix | CNMA with all 2-way interaction terms |
| **Disconnected fallback** | `discomb()` | Additive CNMA for disconnected networks |

- **Random effects** and **fixed effect** models supported
- **Reference treatment** selector for customized comparisons

### Visualizations & Diagnostics

| Output | Description |
|---|---|
| **Network plot** | Treatment network graph with study counts and edge thickness |
| **Forest plot** | Treatment effect comparisons (HR scale) |
| **League table** | Pairwise comparison matrix with confidence intervals |
| **Funnel plot** | Comparison-adjusted funnel plot with contour lines |
| **Treatment ranking** | P-scores for treatment hierarchy |
| **Node-splitting** | Local inconsistency: direct vs. indirect evidence |
| **Design-by-treatment** | Global inconsistency: Q decomposition |
| **Model summary** | Full model output with interaction term details |

### Node Merging

- Merge treatment components into a single node (e.g., combine drug families)
- Inverse-variance weighted pooling of duplicate comparisons
- Self-loop prevention and single-arm study detection
- Reset to original data at any time

### Export

- **HTML reports** — Self-contained downloadable report with all analysis outputs
- **Report history** — Persistent log of generated reports

---

## Project Structure

```
lsr/
├── app.R                         # Entry point
├── global.R                      # Library loading and module sourcing
├── ui.R                          # UI layout (calls module UIs)
├── server.R                      # Server orchestrator (~140 lines)
│
├── modules/                      # Shiny modules
│   ├── mod_data.R                # File upload, data preview, validation
│   ├── mod_merge.R               # Node merging logic
│   ├── mod_results.R             # All results: plots, summary, ranking, inconsistency
│   └── mod_export.R              # Report export and history
│
├── utils/                        # Utility functions
│   ├── netmeta_pipeline.R        # NMA/CNMA model fitting (build_nm, run_cnma_analysis)
│   ├── redcap_connect.R          # Data loading function
│   └── data_processing.R         # Pairwise conversion and data summarization
│
├── data/
│   └── HR_data_pairs_connected_fixed.xlsx   # Bundled example dataset
│
├── reports/
│   └── report_template.Rmd       # Parameterized R Markdown report template
│
├── tests/                        # Unit tests
│   ├── testthat.R                # Test runner
│   └── testthat/
│       ├── test-data_processing.R
│       └── test-netmeta_pipeline.R
│
├── .gitignore
├── LICENSE                       # MIT License
├── CHANGELOG.md                  # Version history
└── README.md
```

### Architecture

The application follows a **modular Shiny architecture**:

- **`server.R`** acts as an orchestrator — it manages shared reactive state (models, analysis options) and wires modules together
- **Modules** handle specific UI sections and their server logic independently
- **Utilities** provide reusable statistical functions separate from Shiny reactivity
- **Report template** is parameterized R Markdown that receives model objects and data

```
┌─────────────┐     ┌──────────────┐     ┌───────────────┐
│  mod_data   │────▶│  server.R    │────▶│ mod_results   │
│ (upload/    │     │ (orchestrator│     │ (plots/tables │
│  preview)   │     │  models)     │     │  ranking)     │
└─────────────┘     └──────┬───────┘     └───────────────┘
                           │
┌─────────────┐            │             ┌───────────────┐
│  mod_merge  │◀───────────┤             │  mod_export   │
│ (node merge)│            └────────────▶│ (report/hist) │
└─────────────┘                          └───────────────┘
```

---

## Requirements

- **R** ≥ 4.0
- **RStudio** (recommended for development)
- A modern web browser

### R Package Dependencies

| Package | Purpose | Required by |
|---|---|---|
| `shiny` | Web application framework | Core |
| `netmeta` | Network meta-analysis engine | Core |
| `meta` | General meta-analysis utilities | Core |
| `shinythemes` | UI theming (Flatly) | Core |
| `DT` | Interactive data tables | Core |
| `igraph` | Network connectivity checks | Core |
| `ggplot2` | Plotting | Core |
| `dplyr` | Data manipulation | Core |
| `readxl` | Excel file import | Data module |
| `rmarkdown` | Report generation | Export module |
| `knitr` | Report rendering | Report template |
| `htmltools` | HTML utilities | Report template |
| `tibble` | Data frames | Report template |
| `testthat` | Unit testing | Tests (dev only) |

---

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/ch-damianidis/lsr.git
cd lsr
```

### 2. Install R dependencies

```r
install.packages(c(
  "shiny", "netmeta", "meta", "shinythemes",
  "DT", "igraph", "ggplot2", "dplyr",
  "readxl", "rmarkdown", "knitr",
  "htmltools", "tibble"
))

# For development (testing):
install.packages("testthat")
```

### 3. Verify installation

```r
# Check all packages load successfully
sapply(c("shiny", "netmeta", "meta", "shinythemes", "DT",
         "igraph", "ggplot2", "dplyr", "readxl", "rmarkdown"),
       requireNamespace, quietly = TRUE)
```

---

## Usage

### Running the Application

```r
library(shiny)
shiny::runApp()
```

Or from another directory:

```r
shiny::runApp("path/to/lsr")
```

### Workflow

1. **Data Overview** — Upload your own CSV/Excel file or use the bundled dataset. Review data preview, summary statistics, and data format detection.

2. **Set up Analysis** — Choose:
   - **Model type**: Simple NMA, Additive CNMA, or Interaction CNMA
   - **Effect model**: Random effects or Fixed effect
   - **Reference treatment**: Select comparator or leave as Auto
   - **Node merging** (optional): Merge treatment components

3. **Run Analysis** — Click "Run Analysis". Data validation runs automatically.

4. **Explore Results** — Navigate tabs:
   - **Plots**: Network graph, forest plot
   - **Summary**: Full model output
   - **Funnel Plot**: Publication bias assessment
   - **Inconsistency**: Node-splitting (local) + design-by-treatment (global)
   - **Ranking**: League table + P-scores

5. **Export** — Generate and download a self-contained HTML report.

---

## Data Format

### Pairwise contrast-level data (preferred)

| Column | Type | Description |
|---|---|---|
| `study` | character | Study identifier |
| `treat1` | character | Treatment in arm 1 |
| `treat2` | character | Treatment in arm 2 |
| `logHR` | numeric | Log hazard ratio |
| `selogHR` | numeric | Standard error of log(HR) |

### Arm-level data (auto-converted)

| Column | Type | Description |
|---|---|---|
| `study` | character | Study identifier |
| `treatment` | character | Treatment name |
| `mean` | numeric | Mean outcome |
| `sd` | numeric | Standard deviation |
| `n` | integer | Sample size |

Arm-level data is automatically converted to pairwise format using `netmeta::pairwise()`.

### Combination treatments

Use ` + ` (space-plus-space) separator for combination treatments:

```
"Drug A + Drug B"
"Ibrutinib + Venetoclax"
```

---

## Analysis Models

### Simple NMA

Standard network meta-analysis via `netmeta()`. Compares individual treatments directly.

### Additive CNMA

Component NMA via `netcomb()` with `sep.trts = " + "`. Decomposes combination treatments into additive components.

### Interaction CNMA

Component NMA with a custom C-matrix that includes all 2-way interaction terms, generated via `combn()` and `createC()`. Allows testing whether treatment combinations have synergistic or antagonistic effects.

### Disconnected Networks

If the treatment network is disconnected, the tool automatically falls back to `discomb()` (additive model only). Interaction models are not available for disconnected networks — a warning is displayed.

---

## Node Merging

1. Go to **Set up Analysis**
2. Select ≥ 2 components from the dropdown
3. Enter a new node name
4. Click **Apply Node Merge**

**How it works:**
- Selected components are replaced in all treatment labels (e.g., `"A + B"` where `B` is merged becomes `"A + NewName"`)
- Duplicate comparisons within a study are pooled using **inverse-variance weighting**
- Self-loops (A vs A) are automatically detected and prevented
- Single-arm studies after merge trigger a warning
- Click **Reset to Original Data** to undo all merges

---

## Report Generation

Reports are generated from a parameterized R Markdown template ([`reports/report_template.Rmd`](reports/report_template.Rmd)) and include:

| Section | Contents |
|---|---|
| Data | Preview table, summary statistics |
| Methods | Model specification |
| Network plot | Treatment network graph |
| Forest plot | Effect estimates |
| League table | Pairwise comparisons |
| Ranking | P-scores |
| Inconsistency | Node-splitting + design-by-treatment |
| Funnel plot | Publication bias |
| Appendix | Full `summary()` output, session info |

---

## Testing

Run unit tests with:

```r
# From the project root directory
testthat::test_dir("tests/testthat")

# Or from tests/ directory
source("tests/testthat.R")
```

Tests cover:
- `summarize_data()` — pairwise and arm-level data, missing data detection
- `convert_to_pairwise()` — passthrough for pairwise data
- `build_nm()` — column validation, model construction
- `run_cnma_analysis()` — connected/disconnected network handling

---

## Technical Notes

- **Effect measure**: Hazard Ratio (HR); internal computations use log(HR) scale
- **Column naming**: `logHR`/`selogHR` names are used by convention throughout the pipeline, but the tool works with any contrast-level effect size
- **Disconnected networks**: Automatically detected via `igraph::is.connected()` and handled with `discomb()`
- **Interaction terms**: Generated as all 2-way combinations of unique components using `combn()`
- **Funnel plot**: Uses `netmeta::netfunnel()` when available, falls back to `netmeta:::funnel.netmeta()`
- **Error handling**: All model fitting calls are wrapped in `tryCatch()` with user-visible error notifications via `showNotification()`
- **Shiny modules**: The application uses the Shiny modules pattern for separation of concerns — each functional area (data, merge, results, export) is encapsulated in its own module

---

## Troubleshooting

| Problem | Solution |
|---|---|
| "Missing required columns" error | Ensure your data has `study`, `treat1`, `treat2`, `logHR`, `selogHR` columns (or arm-level equivalents) |
| Network plot is empty | Click "Run Analysis" first — plots require a fitted model |
| "Interaction model not available" | Your network is disconnected; use Simple NMA or Additive CNMA |
| Report download fails | Ensure `rmarkdown` and `knitr` are installed; check R console for errors |
| League table shows "not applicable" | Network needs ≥ 3 treatments and must be connected |
| Funnel plot shows "not enough studies" | At least 5 studies are required for a reliable funnel plot |
| File upload doesn't work | Ensure your file is `.csv` or `.xlsx` format; check for encoding issues |

---

## Contributing

Contributions are welcome! To contribute:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Commit your changes (`git commit -m 'Add my feature'`)
4. Push to the branch (`git push origin feature/my-feature`)
5. Open a Pull Request

Please ensure your changes pass existing tests before submitting.

---

## Citation

If you use this tool in your research, please cite:

```
Damianidis, C. (2026). Living Network Meta-Analysis Tool: An R Shiny Application
for Automated Evidence Synthesis. MSc Thesis, Aristotle University of Thessaloniki.
```

---

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

---

## Contact

**Charalampos Damianidis**
- Email: [charalampos.damianidis@gmail.com](mailto:charalampos.damianidis@gmail.com)
- GitHub: [github.com/ch-damianidis](https://github.com/ch-damianidis)

For questions, issues, or contributions, please [open an issue](https://github.com/ch-damianidis/lsr/issues) on GitHub.
