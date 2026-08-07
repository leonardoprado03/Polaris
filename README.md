=POLARIS 
Version 1.0
Developed by Leonardo Prado
School of Geographical Sciences and Urban Planning
Arizona State University
 
1. Introduction
1.1 Overview
POLARIS (Platform for Online Literature Analysis, Retrieval, and Integrated Search) is an R/Shiny-based application designed to support evidence synthesis and systematic literature reviews through integrated searches across multiple scientific databases.
The platform was developed to streamline the literature search process by providing a unified interface capable providing a unified interface for querying multiple databases, monitoring search progress in real time, organizing retrieved records, and exporting results in formats compatible with reference management software and review platforms.
POLARIS reduces the need to perform independent searches in multiple databases and promotes transparency, reproducibility, and efficiency in literature review workflows.
1.2 Objctives
The primary objectives of POLARIS are:
•	Facilitate comprehensive literature searches.
•	Integrate multiple bibliographic databases into a single workflow.
•	Support systematic reviews and evidence synthesis.
•	Improve transparency and reproducibility.
•	Reduce manual effort during search and retrieval stages.
•	Generate exportable reference files for downstream screening and analysis.
1.3 Intended Users
POLARIS is intended for:
•	Researchers
•	Graduate students
•	Faculty members
•	Evidence synthesis teams
•	Systematic review practitioners
•	Public health researchers
•	Environmental and climate scientists
•	Social scientists
 
2. System Architecture
POLARIS was developed using the R programming language and the Shiny framework.
The application combines:
•	Graphical user interface (GUI)
•	Background worker execution
•	API-based retrieval
•	Export management system
•	Monitoring dashboard
•	Automatic RIS export
Searches are executed through dedicated worker processes, allowing users to continue interacting with the application while records are being retrieved.
 
3. Supported Databases
3.1 PubMed
PubMed is accessed through the National Center for Biotechnology Information (NCBI) API.
Retrieved information may include:
•	Title
•	Authors
•	Journal
•	Abstract
•	Publication date
•	PMID
•	DOI
3.2 Scopus
Scopus records are retrieved through Elsevier APIs.
Available metadata may include:
•	Title
•	Authors
•	Journal
•	Abstract
•	DOI
3.3 ScienceDirect
ScienceDirect retrieval is performed through Elsevier services.
Retrieved records may include:
•	Title
•	Authors
•	Journal
•	Abstract
•	DOI
3.4 Semantic Scholar
Semantic Scholar searches are performed through the Semantic Scholar Academic Graph API.
Available metadata may include:
•	Title
•	Authors
•	Venue
•	Abstract
•	Publication year
•	DOI
 
4. Starting POLARIS
4.1 Launching the Application
After installing all required dependencies, launch POLARIS by executing:
shiny::runApp()
The application will open in the default web browser.
4.2 Home Screen
The main interface contains:
•	Search configuration panel
•	Database selection menu
•	Search execution controls
•	Monitoring dashboard
•	Search monitoring dashboard
 
5. Conducting a Literature Search
5.1 Preparing search queries 
Instead of providing an interactive query builder, POLARIS executes user-defined search strategies prepared externally. Search queries should be written in a plain-text (.txt) file, with one query per line, or provided through the corresponding file path in the graphical user interface. Users are responsible for constructing their own search strategies using the syntax supported by the target databases (e.g., Boolean operators, quotation marks, parentheses, and field tags where applicable). During execution, POLARIS submits each query to the selected databases and applies only the database-specific syntax adaptations required by the corresponding APIs (e.g., TITLE-ABS-KEY for Scopus and Boolean syntax conversion for Semantic Scholar), while preserving the logical structure of the original search strategy.
5.2 Selecting Databases
Users may choose one or more databases simultaneously.
Recommended practice:
•	Broad searches: all databases
•	Medical topics: PubMed + Scopus
•	Interdisciplinary topics: all databases
•	Rapid searches: Semantic Scholar + PubMed
5.3 Running Searches
After providing the query file and selecting the databases, click Search.
1.	POLARIS initializes retrieval workers.
2.	Queries are submitted to selected databases.
3.	Results are retrieved incrementally.
4.	Progress is displayed in real time.
 
6. Monitoring Dashboard
The monitoring dashboard provides live feedback regarding search execution.
6.1 Search Status
Displayed information may include:
•	Database currently being queried
•	Number of retrieved records
•	Execution progress
•	Completion status
6.2 Runtime Logs
The logging system records:
•	Search initiation
•	API requests
•	Pagination events
•	Export events
•	Warnings
•	Errors
Logs facilitate troubleshooting and reproducibility.
 
7. Retrieval Workflow
POLARIS follows the workflow below:
1.	User submits query.
2.	Database-specific requests are generated.
3.	APIs are contacted.
4.	Records are retrieved.
5.	Pagination procedures are executed when required.
6.	Results are exported separately for each query and database
7.	Data are stored locally.
 
8. Organizing Results
POLARIS automatically stores search outputs in designated folders.
Folder organization may include:
•	Search date
•	Database source
•	Search identifier
•	Export type
This structure supports project management and reproducibility.
 
9. Exporting Records
9.1 RIS Export
POLARIS exports bibliographic records in RIS format.
RIS files can be imported into:
•	Zotero
•	EndNote
•	Mendeley
•	Rayyan
•	Covidence
 
10. Reproducibility Features
POLARIS was designed to support transparent evidence synthesis.
The platform records:
•	Search date
•	Search query
•	Selected databases
•	Retrieved record counts
•	Export RIS filename
•	Runtime logs
These records facilitate reporting according to systematic review guidelines.
 
11. Best Practices
To maximize search quality:
•	Use controlled vocabulary when available.
•	Apply Boolean operators carefully.
•	Search multiple databases.
•	Save search strategies.
•	Maintain exported RIS files.
•	Document all search decisions.
 
12. Limitations
Users should be aware that:
•	API availability depends on provider services.
•	Search results may change over time as databases are updated.
•	Rate limits imposed by providers may affect retrieval speed.
•	Metadata completeness varies among databases.
 
13. Future Developments
Planned enhancements include:
•	Search validation module
•	Precision and recall assessment
•	Screening workflow integration
•	Expanded export formats
•	Additional bibliographic databases
•	Advanced search analytics
 
14. Citation
If POLARIS contributes to a publication, users should cite the software as:
Prado, L., Vanos, J.& Rosales Chavez, J. B. (2026). Polaris - A multi-database literature review system [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.21724915 
15. Support
For questions, bug reports, or feature requests, please contact the software developer.
Developer: Leonardo Prado (ldoprado@asu.edu)
Institution: Arizona State University
School of Geographical Sciences and Urban Planning

<img width="468" height="645" alt="image" src="https://github.com/user-attachments/assets/9f1baf2d-a59b-49a8-b2b9-918e0dace7b7" />
