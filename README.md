# POLARIS

**Platform for Online Literature Analysis, Retrieval, and Integrated Search**

**Version 1.0**

Developed by **Leonardo Prado**  
School of Geographical Sciences and Urban Planning  
Arizona State University

---

## 1. Introduction

### 1.1 Overview

POLARIS (Platform for Online Literature Analysis, Retrieval, and Integrated Search) is an R/Shiny-based application designed to support bibliographic retrieval for evidence synthesis and literature review workflows through integrated searches across multiple scientific databases.

The platform was developed to streamline the literature search process by providing a unified interface for querying multiple databases, monitoring search progress in real time, organizing retrieved records, and automatically exporting results in a format compatible with reference management and screening software.

POLARIS reduces the need to perform independent searches across multiple databases and promotes transparency, reproducibility, and efficiency during the bibliographic retrieval stage of literature reviews.

### 1.2 Objectives

The primary objectives of POLARIS are:

- Facilitate comprehensive literature searches.
- Integrate multiple bibliographic databases into a single retrieval workflow.
- Support the bibliographic retrieval stage of literature reviews and evidence synthesis.
- Improve transparency and reproducibility of literature searches.
- Reduce manual effort during search and retrieval.
- Generate exportable reference files for downstream screening and analysis.

### 1.3 Intended Users

POLARIS is intended for:

- Researchers
- Graduate students
- Faculty members
- Evidence synthesis teams
- Systematic review practitioners
- Public health researchers
- Environmental and climate scientists
- Social scientists

---

## 2. System Architecture

POLARIS was developed using the R programming language and the Shiny framework.

The application combines:

- Graphical user interface (GUI)
- Background worker execution
- API-based bibliographic retrieval
- Monitoring dashboard
- Automatic RIS export
- Search logging

Searches are executed through dedicated background worker processes, allowing the graphical interface to remain responsive while records are being retrieved. Database queries are processed sequentially within the retrieval workflow.

---

## 3. Supported Databases

POLARIS currently supports bibliographic retrieval from four scientific databases.

### 3.1 PubMed

PubMed is accessed through the National Center for Biotechnology Information (NCBI) E-utilities API.

Retrieved bibliographic information may include:

- Title
- Authors
- Journal
- Abstract
- Publication year
- DOI
- URL

### 3.2 Scopus

Scopus records are retrieved through the Elsevier API.

Retrieved bibliographic information may include:

- Title
- Authors
- Journal
- Abstract
- Publication year
- DOI
- URL

Scopus access requires users to provide their own Elsevier API credentials and, when required, an institutional token.

### 3.3 ScienceDirect

ScienceDirect retrieval is performed through the Elsevier API.

Retrieved bibliographic information may include:

- Title
- Authors
- Journal
- Abstract
- Publication year
- DOI
- URL

ScienceDirect uses the same Elsevier authentication mechanism as Scopus.

### 3.4 Semantic Scholar

Semantic Scholar searches are performed through the Semantic Scholar Academic Graph API using the bulk search endpoint.

Retrieved bibliographic information may include:

- Title
- Authors
- Venue
- Abstract
- Publication year
- DOI
- URL

---

## 4. Starting POLARIS

### 4.1 Launching the Application

After installing the required dependencies, launch POLARIS from R/RStudio.

The application will open in the default web browser or RStudio viewer.

The application will open in the default web browser or RStudio viewer.

### 4.2 Main Interface

The POLARIS interface provides:

Search configuration
Query-file selection
Database selection
Elsevier credential fields when required
Search execution controls
Run preview
Real-time monitoring dashboard
Automatic RIS export
## 5. Conducting a Literature Search
### 5.1 Preparing Search Queries

POLARIS does not provide an interactive query builder. Instead, it executes user-defined search strategies prepared externally.

Search queries should be written in a plain-text (.txt) file, with one query per line, or provided through the corresponding file path in the graphical interface.

Users are responsible for constructing their search strategies using syntax appropriate for the target databases, including Boolean operators, quotation marks, parentheses, and field expressions where supported.

During execution, POLARIS applies database-specific adaptations when required. For example, Scopus queries are wrapped in TITLE-ABS-KEY(), while Semantic Scholar Boolean expressions are converted to the syntax supported by its API.

### 5.2 Selecting Databases

Users may select one or more supported databases for a search run:

PubMed
Scopus
ScienceDirect
Semantic Scholar

Database selection should reflect the objectives, disciplinary scope, and search strategy of the literature review.

### 5.3 Running Searches

After providing the query file and selecting the databases:

Configure the search parameters.
Provide Elsevier credentials when Scopus or ScienceDirect is selected.
Review the search configuration.
Start the search.
POLARIS submits the queries to the selected databases.
Records are retrieved according to each database's API procedures.
Search progress is displayed in real time.
Retrieved records are automatically exported as RIS files.
### 6. Monitoring Dashboard

The monitoring dashboard provides real-time information about search execution.

### 6.1 Search Status

Displayed information may include:

Database currently being processed
Number of retrieved records
Search progress
Query status
Completion status
Retrieval errors
### 6.2 Logging

POLARIS maintains execution logs to support transparency, troubleshooting, and reproducibility.

The logs record information such as:

Execution date and time
Source database
Search query
Number of retrieved records
Generated RIS filename
Execution status
Error or status messages

A master log summarizes searches across databases, while database-specific logs retain information for each individual data source.

### 7. Retrieval Workflow

POLARIS follows the general workflow below:

The user provides externally prepared search queries.
Empty queries are skipped.
Database-specific requests are generated.
The corresponding APIs are contacted.
Bibliographic records are retrieved.
Pagination or continuation procedures are applied when required.
Retrieved metadata are standardized.
Missing abstracts from eligible Scopus and ScienceDirect records may be enriched through Semantic Scholar using DOI matching.
Results are exported separately for each query and database as RIS files.
Search information is recorded in the execution logs.
## 8. Organizing Results

POLARIS automatically stores retrieved records in database-specific result directories.

Outputs are organized according to the selected database and search configuration, allowing users to maintain separate records of individual search executions.

Each successful query can generate a corresponding RIS file containing the retrieved bibliographic records.

## 9. Exporting Records
### 9.1 RIS Export

POLARIS automatically exports retrieved bibliographic records in Research Information Systems (.ris) format.

The standardized RIS output may contain:

Title
Authors
Publication year
Journal or venue
DOI
URL
Abstract

RIS files can subsequently be imported into compatible reference-management and evidence-synthesis software, such as:

Zotero
EndNote
Mendeley
Rayyan
Covidence

POLARIS does not currently perform cross-database deduplication. Deduplication and screening can therefore be conducted using dedicated downstream tools.

## 10. Reproducibility Features

POLARIS supports transparent and auditable bibliographic retrieval by recording:

Search date and time
Original search query
Source database
Retrieved record count
Generated RIS filename
Execution status
Error and status messages

These records can support documentation and reporting of the bibliographic retrieval stage of evidence-synthesis workflows.

## 11. Best Practices

To maximize search quality and reproducibility:

Develop and document the search strategy before running POLARIS.
Use controlled vocabulary where appropriate.
Apply Boolean operators carefully.
Consider differences in syntax and indexing among databases.
Search multiple databases when appropriate for the review question.
Preserve the original query files.
Maintain the exported RIS files and execution logs.
Document important search decisions.
## 12. Limitations

Users should be aware that:

API availability depends on external provider services.
Database access may depend on institutional subscriptions and user credentials.
Provider-imposed retrieval and rate limits may affect the number or speed of retrieved records.
Search results may change over time as bibliographic databases are updated.
Metadata completeness varies across databases.
Database APIs and public web interfaces may implement different retrieval mechanisms.
POLARIS does not currently perform cross-database deduplication.
POLARIS supports bibliographic retrieval rather than the complete systematic-review workflow.
## 13. Future Developments

Planned enhancements include:

Integrated search-validation tools
Precision and recall assessment
User-centred usability evaluation
Additional bibliographic databases
Expanded export formats
Advanced search analytics
Integration with downstream evidence-synthesis workflows
## 14. Citation

If POLARIS contributes to a publication, please cite the software as:

Prado, L., Vanos, J., & Rosales Chavez, J. B. (2026). Polaris – An Automated Literature Retrieval System [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.21724915
## 15. Support

For questions, bug reports, or feature requests, please contact:

Leonardo Prado
School of Geographical Sciences and Urban Planning
Arizona State University
Email: ldoprado@asu.edu
