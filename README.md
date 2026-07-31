# Polaris-An Automated Literature Retrieval System
POLARIS is an R (Shiny) application designed to streamline multi-database literature searches and export results in RIS format, fully compatible with tools such as Zotero, Mendeley, and EndNote. Polaris enhances the audibility and transparency of the evidence synthesis process, thereby improving the traceability of the literature review workflow.

 Source code

The complete source code is available in the `src/` directory of this repository.

To launch the application:

```r
setwd("src")
shiny::runApp()
```
POLARIS automatically checks for and installs any required R packages during the first execution.

🔎 Multi-database search
Run queries across major academic databases, including PubMed, Scopus, ScienceDirect, and Semantic Scholar.

📄 Standardized export
All results are exported as .ris files, ensuring seamless integration with reference managers such as Zotero, Mendeley, and EndNote.

📊 Search logging
Each search generates a structured results_log.csv, allowing you to track queries, monitor results, and document execution status for reproducibility.

⚡ Performance-focused execution
POLARIS is designed for efficiency, with batch processing, built-in rate limiting, retry handling, and a background worker system that keeps the Shiny interface responsive during long runs.

🎯 Use Cases
POLARIS is particularly useful for:
All types of literature reviews

⚙️ Requirements
To run POLARIS, you will need:

R > 4.5.1.
RStudio (recommended)
Elsevier API Key and Institutional Token (required for Scopus and ScienceDirect)
Why POLARIS?
Literature searches are often fragmented, manual, and difficult to reproduce—especially when combining multiple databases, each with its own interface, limitations, and export formats.

POLARIS addresses these challenges by bringing everything into a single environment. It unifies search workflows, automates large-scale query execution, and ensures reproducibility through structured logging and standardized outputs.

Instead of switching between platforms and manually consolidating results, POLARIS allows you to run, monitor, and export your entire search strategy from one place, making it especially powerful for systematic reviews and data-intensive research projects.

