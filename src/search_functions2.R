# search_functions.R
# Multi-database search (PubMed | Scopus | ScienceDirect | Semantic Scholar)
# -------------------------------------------------------------------------

# === PACKAGES ===
required <- c("httr", "jsonlite", "dplyr", "stringr", "purrr", "xml2", "zip", "plotly")
inst <- required[!(required %in% installed.packages()[, "Package"])]
if (length(inst)) install.packages(inst, repos = "https://cloud.r-project.org")
invisible(lapply(required, library, character.only = TRUE))

# === USER FLAGS (keep inside file if you want toggle) ===
delay_pubmed <- 0.4
delay_between_bases <- 1.2  # General delay between requests

use_pubmed  <- TRUE
use_scopus  <- TRUE
use_scidir  <- TRUE
use_semantic_scholar <- TRUE

# === PATHS  ===
root_path <- file.path(normalizePath(getwd()), "results")
dir.create(root_path, recursive = TRUE, showWarnings = FALSE)

base_dirs <- list(
  pubmed         = file.path(root_path, "results_pubmed"),
  scopus         = file.path(root_path, "results_scopus"),
  scidir         = file.path(root_path, "results_sciencedirect"),
  semantic_scholar = file.path(root_path, "results_semantic_scholar")
)

# === HELPERS ===
`%||%` <- function(a, b) if (!is.null(a)) a else b

sanitize_filename <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

ensure_dirs <- function(groups = NULL) {
  for (d in unlist(base_dirs)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!is.null(groups)) for (g in groups) dir.create(file.path(d, g), recursive = TRUE, showWarnings = FALSE)
  }
}

detect_group <- function(q) {
  qs <- tolower(q)
  if (grepl("street hawker", qs)) return("street hawker")
  if (grepl("sidewalk vendor", qs)) return("sidewalk vendor")
  if (grepl("street vendor", qs)) return("street vendors")
  if (grepl("informal outdoor worker", qs)) return("informal outdoor worker")
  if (grepl("informal labor", qs)) return("informal labor")
  "all"
}

ensure_abstract_col <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!"abstract" %in% names(df)) df$abstract <- ""
  df$abstract <- ifelse(is.na(df$abstract), "", df$abstract)
  df
}

clean_text_line <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  x <- as.character(x)
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
read_queries_user <- function(mode, file_obj, path_text) {
  if (mode == "upload") {
    if (is.null(file_obj)) return(character(0))
    return(trimws(readLines(file_obj$datapath, warn = FALSE)))
  } else {
    path_text <- gsub('["\']', '', path_text)
    if (path_text == "" || !file.exists(path_text)) return(character(0))
    return(trimws(readLines(path_text, warn = FALSE)))
  }
}
# === ELSEVIER (SCOPUS / SCIENCEDIRECT) AUTH HELPERS ===
elsevier_headers <- function(api_key = NULL, inst_token = NULL) {
  # Prioritize values provided by the user interface; otherwise, use environment variables.
  api_key <- if (!is.null(api_key) && nzchar(api_key)) api_key else Sys.getenv("ELSEVIER_API_KEY")
  inst    <- if (!is.null(inst_token) && nzchar(inst_token)) inst_token else Sys.getenv("ELSEVIER_INST_TOKEN")
  
  if (!nzchar(api_key)) stop("ELSEVIER_API_KEY not found.")
  if (!nzchar(inst))    stop("ELSEVIER_INST_TOKEN not found.")
  
  httr::add_headers(
    "X-ELS-APIKey"    = api_key,
    "X-ELS-Insttoken" = inst,
    "Accept"          = "application/json"
  )
}
# === POLARIS RESULT HELPERS ===
make_polaris_result <- function(df = data.frame(), status = "Success", message = "") {
  if (is.null(df)) df <- data.frame()
  attr(df, "polaris_status")  <- status
  attr(df, "polaris_message") <- message
  df
}

get_polaris_meta <- function(x) {
  list(
    status  = attr(x, "polaris_status")  %||% "Success",
    message = attr(x, "polaris_message") %||% ""
  )
}

elsevier_error_message <- function(code) {
  switch(
    as.character(code),
    "401" = "Elsevier authentication failed (401): invalid or expired API key.",
    "403" = "Elsevier access denied (403): invalid institutional token or no access to this endpoint.",
    "429" = "Elsevier rate limit reached (429): too many requests.",
    paste0("Elsevier HTTP error (", code, ").")
  )
}
# === CROSSREF ABSTRACT RECOVERY ===
get_abstract_from_crossref <- function(doi) {
  if (is.null(doi) || doi == "" || is.na(doi)) return("")
  
  # Avoid Erro 400
  doi_clean <- gsub("^doi:", "", trimws(doi), ignore.case = TRUE)
  doi_clean <- URLencode(doi_clean, reserved = TRUE)
  url <- paste0("https://api.crossref.org/works/", doi_clean)
  
  res <- tryCatch({
    # Avoid Error 401 (Elsevier keys)
    httr::GET(url, 
              httr::add_headers(Accept = "application/json"),
              httr::user_agent("PolarisSearch/1.0 (mailto:seu_email@asu.edu)"),
              httr::timeout(10))
  }, error = function(e) return(NULL))
  
  if (!is.null(res) && httr::status_code(res) == 200) {
    payload <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    raw_abstract <- payload$message$abstract %||% ""
    # Remove XML/JATS tags from the CrossRef abstract
    return(gsub("<[^>]*>", "", raw_abstract)) 
  }
  return("")
}
# === DOI CACHE (Crossref) ===
.doi_cache <- new.env(parent = emptyenv())

lookup_doi <- function(title) {
  if (is.null(title) || nchar(title) < 5) return("")
  key <- tolower(trimws(title))
  if (!is.null(.doi_cache[[key]])) return(.doi_cache[[key]])
  
  url <- paste0("https://api.crossref.org/works?query.title=", URLencode(title), "&rows=1")
  res <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(res) || length(res$message$items) == 0) {
    .doi_cache[[key]] <- ""
    return("")
  }
  doi <- res$message$items$doi[1] %||% ""
  .doi_cache[[key]] <- doi
  doi
}

# === RIS HELPERS ===
clean_field <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  x <- as.character(x)
  x <- gsub("[\r\n\t]+", " ", x) 
  trimws(x)
}

make_ris <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  
  df[] <- lapply(df, as.character)
  
  cols_necessarias <- c("title", "authors", "year", "journal", "doi", "url", "abstract")
  for(c in cols_necessarias) {
    if(!(c %in% names(df))) df[[c]] <- ""
  }
  
  ris_list <- c()
  

  for (i in 1:nrow(df)) {
    # Safe extraction
    t  <- clean_field(df$title[i])
    auth_raw <- clean_field(df$authors[i])
    auth_vec <- trimws(unlist(strsplit(auth_raw, "[,;]")))
    auth_vec <- auth_vec[nchar(auth_vec) > 0]
    a_block  <- if(length(auth_vec) > 0) paste0("AU  - ", auth_vec, collapse = "\n") else "AU  - Unknown"
    
    y  <- clean_field(df$year[i])
    j  <- clean_field(df$journal[i])
    d  <- clean_field(df$doi[i])
    u  <- clean_field(df$url[i])
    ab <- clean_field(df$abstract[i])
    
    block <- paste0(
      "TY  - JOUR\n",
      "TI  - ", t, "\n",
      a_block, "\n",
      "PY  - ", y, "\n",
      "JO  - ", j, "\n",
      if(nchar(d) > 0) paste0("DO  - ", d, "\n") else "",
      if(nchar(u) > 0) paste0("UR  - ", u, "\n") else "",
      if(nchar(ab) > 5) paste0("AB  - ", ab, "\n") else "",
      "ER  - "
    )
    ris_list <- c(ris_list, block)
  }
  return(ris_list)
}

write_ris_file <- function(ris_vec, path) {
  if(length(ris_vec) == 0) return(NULL)
  
  ris_text <- paste(ris_vec, collapse = "\n\n")
  
  con <- file(path, open = "wb")
  writeBin(charToRaw(ris_text), con)
  close(con)
}
# ==========================
# PUBMED
# ==========================
fetch_pubmed_abstracts <- function(ids) {
  if (length(ids) == 0) return(list())
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  
  res <- tryCatch({
    httr::GET(url, query = list(db = "pubmed", id = paste(ids, collapse = ","), 
                                rettype = "abstract", retmode = "xml"),
              httr::timeout(60))
  }, error = function(e) return(NULL))
  
  if (is.null(res) || httr::status_code(res) != 200) return(list())
  
  xml <- xml2::read_xml(httr::content(res, "text", encoding = "UTF-8"))
  articles <- xml2::xml_find_all(xml, ".//PubmedArticle")
  
  abs_map <- list()
  for (a in articles) {
    pmid <- xml2::xml_text(xml2::xml_find_first(a, ".//PMID"))
    abs_text <- xml2::xml_text(xml2::xml_find_all(a, ".//AbstractText"))
    abs_map[[pmid]] <- paste(abs_text, collapse = " ")
  }
  return(abs_map)
}
get_pubmed_results <- function(query) {
  q_clean <- trimws(gsub("[[:space:]]+", " ", query))
  message("📡 Synchronizing PubMed: ", q_clean)
  
  # 1. First call, find the total number of papers
  res_total <- httr::GET(
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    query = list(db = "pubmed", term = q_clean, retmode = "json", rettype = "count")
  )
  
  if (httr::status_code(res_total) != 200) return(data.frame())
  
  count_total <- as.numeric(jsonlite::fromJSON(httr::content(res_total, "text"))$esearchresult$count)
  
  if (count_total == 0) {
    message("ℹ️ Zero query results")
    return(data.frame())
  }
  message(" ✅ Total PubMed records found: ", count_total)

  res_ids <- httr::GET(
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    query = list(
      db = "pubmed",
      term = q_clean,
      retmax = count_total, 
      retmode = "json",
      usehistory = "y"
    )
  )
  
  ids <- jsonlite::fromJSON(httr::content(res_ids, "text"))$esearchresult$idlist
  batch_size <- 300
  all_recs <- list()
  
  chunks <- split(ids, ceiling(seq_along(ids) / batch_size))
  for (i in seq_along(chunks)) {
    message(sprintf("📥  Downloading batch %d of %d...", i, length(chunks)))
    
    current_ids <- paste(chunks[[i]], collapse = ",")
    attempt <- 1
    df_batch <- NULL
    
    while(attempt <= 3 && is.null(df_batch)) {
      try({
        sum_res <- httr::GET(
          url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
          query = list(db="pubmed", id=current_ids, retmode="json"),
          httr::config(http_version = 1.1),
          httr::timeout(60)
        )
        
        abs_map <- fetch_pubmed_abstracts(chunks[[i]]) 
        
        if (httr::status_code(sum_res) == 200) {
          js_raw <- httr::content(sum_res, "text", encoding = "UTF-8")
          js <- jsonlite::fromJSON(js_raw)$result
          recs <- js[names(js) != "uids"]
          df_batch <- purrr::map_df(recs, function(r) {
            auths <- if(!is.null(r$authors)) paste(r$authors$name, collapse=", ") else "Unknown"
            
            data.frame(
              title = r$title %||% "No Title",
              authors = auths,
              year = substr(r$pubdate %||% "", 1, 4),
              journal = r$fulljournalname %||% "",
              doi = if(!is.null(r$elocationid)) gsub("doi: ", "", r$elocationid) else "",
              url = paste0("https://pubmed.ncbi.nlm.nih.gov/", r$uid),
              abstract = if(!is.null(abs_map[[as.character(r$uid)]])) abs_map[[as.character(r$uid)]] else "",
              stringsAsFactors = FALSE
            )
          })
        }
      }, silent = FALSE)
      
      if (is.null(df_batch)) {
        message(sprintf("⚠️ Connection error in batch %d. Attempt %d of 3. Retrying......", i, attempt))
        Sys.sleep(5 * attempt) 
        attempt <- attempt + 1
      }
    }
    
    if (!is.null(df_batch)) {
      all_recs[[i]] <- df_batch
    }
    
    # avoid error 419
    Sys.sleep(0.6) 
  }

  return(dplyr::bind_rows(all_recs))
}
# ==========================
# SCOPUS
# ==========================
get_scopus_results <- function(query, fetch_abstracts_scopus = FALSE, api_key = NULL, inst_token = NULL) {
  message("🔍 Scopus ", query)
  
  scopus_query <- paste0("TITLE-ABS-KEY(", query, ")")
  
  all_dfs <- list()
  current_cursor <- "*" 
  total_baixado <- 0
  
  repeat {
    res <- tryCatch({
      httr::GET(
        "https://api.elsevier.com/content/search/scopus",
        elsevier_headers(api_key, inst_token),
        query = list(
          query = scopus_query,
          cursor = current_cursor,
          count = 100, 
          view = "STANDARD"
        ),
        httr::timeout(60)
      )
    }, error = function(e) {
      message("\n[!] Connection error: ", e$message)
      return(NULL)
    })
    
    if (is.null(res)) {
      return(make_polaris_result(
        data.frame(),
        status = "ERROR_CONNECTION",
        message = "Connection error while calling Scopus."
      ))
    }
    
    status <- httr::status_code(res)
    
    if (status == 429) {
      wait_s <- suppressWarnings(as.numeric(httr::headers(res)[["retry-after"]]))
      if (is.na(wait_s) || wait_s <= 0) wait_s <- 60
      message("\n[!] ", elsevier_error_message(429), " Waiting ", wait_s, "s...")
      Sys.sleep(wait_s)
      next
    }
    
    if (status %in% c(401, 403)) {
      msg <- elsevier_error_message(status)
      message("\n[!] ", msg)
      return(make_polaris_result(
        data.frame(),
        status = paste0("ERROR_", status),
        message = msg
      ))
    }
    
    if (status != 200) {
      msg <- paste0("Scopus HTTP error: ", status)
      message("\n[!] ", msg)
      return(make_polaris_result(
        data.frame(),
        status = paste0("ERROR_", status),
        message = msg
      ))
    }
    
    payload <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    entries <- payload$`search-results`$entry
    
    if (is.null(entries) || length(entries) == 0) break
    
    df_batch <- purrr::map_df(entries, function(item) {
      data.frame(
        title = item$`dc:title` %||% "",
        authors = item$`dc:creator` %||% "",
        year = substr(item$`prism:coverDate` %||% "", 1, 4),
        journal = item$`prism:publicationName` %||% "",
        doi = item$`prism:doi` %||% "",
        url = (if(!is.null(item$link)) item$link[[1]]$href else "") %||% "",
        abstract = item$`dc:description` %||% "",
        stringsAsFactors = FALSE
      )
    })
    
    all_dfs[[length(all_dfs) + 1]] <- df_batch
    total_baixado <- total_baixado + nrow(df_batch)
    
    message(sprintf("Scopus | downloading: %s | Cursor: %s", total_baixado, current_cursor))
    
    # Cursor
    links <- payload$`search-results`$link
    next_link_obj <- Filter(function(x) x$`@ref` == "next", links)
    
    if (length(next_link_obj) > 0) {
      full_next_url <- next_link_obj[[1]]$`@href`
      new_cursor <- sub(".*cursor=([^&]+).*", "\\1", full_next_url)
      if (new_cursor == current_cursor) break
      current_cursor <- URLdecode(new_cursor)
    } else {
      break
    }
    
    Sys.sleep(0.5)
    if (total_baixado %% 1000 == 0) gc() 
  }
  
  if (length(all_dfs) == 0) return(data.frame())
  return(dplyr::bind_rows(all_dfs))
}

# ==========================
# SCIENCEDIRECT
# ==========================
get_sciencedirect_test <- function(query, batch_size = 100, api_key = NULL, inst_token = NULL) {
  message("🔍 ScienceDirect", query)
  
  # 1.  Configure authentication keys (prefer values provided by the application; otherwise, use environment variables)
  ak <- if (!is.null(api_key) && nzchar(api_key)) api_key else Sys.getenv("ELSEVIER_API_KEY")
  it <- if (!is.null(inst_token) && nzchar(inst_token)) inst_token else Sys.getenv("ELSEVIER_INST_TOKEN")
  
  all_dfs <- list()
  start_idx <- 0
  
  repeat {
    res <- tryCatch({
      httr::GET(
        "https://api.elsevier.com/content/search/sciencedirect",
        httr::add_headers("X-ELS-APIKey" = ak, "X-ELS-Insttoken" = it, "Accept" = "application/json"),
        query = list(query = query, start = start_idx, count = batch_size),
        httr::timeout(30)
      )
    }, error = function(e) {
      message("⚠ connection failure: ", e$message)
      return(NULL)
    })
    
    if (is.null(res)) {
      return(make_polaris_result(
        data.frame(),
        status = "ERROR_CONNECTION",
        message = "Connection error while calling ScienceDirect."
      ))
    }
    
    status <- httr::status_code(res)
    
    if (status == 429) {
      wait_s <- suppressWarnings(as.numeric(httr::headers(res)[["retry-after"]]))
      if (is.na(wait_s) || wait_s <= 0) wait_s <- 60
      message("\n[!] ", elsevier_error_message(429), " Waiting ", wait_s, "s...")
      Sys.sleep(wait_s)
      next
    }
    
    if (status %in% c(401, 403)) {
      msg <- elsevier_error_message(status)
      message("\n[!] ", msg)
      return(make_polaris_result(
        data.frame(),
        status = paste0("ERROR_", status),
        message = msg
      ))
    }
    
    if (status != 200) {
      msg <- paste0("ScienceDirect HTTP error: ", status)
      message("\n[!] ", msg)
      return(make_polaris_result(
        data.frame(),
        status = paste0("ERROR_", status),
        message = msg
      ))
    }
    payload <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    entries <- payload$`search-results`$entry
    
    if (is.null(entries) || length(entries) == 0) break
  
    df_batch <- purrr::map_df(entries, function(item) {
      raw_auths <- item$authors %||% item$`dc:creator` %||% "Unknown"
      
      auth_final <- if (is.list(raw_auths)) {
        sapply(raw_auths, function(x) {
          if (is.list(x)) return(x$name %||% x$`$` %||% "Unknown")
          return(as.character(x))
        }) %>% paste(collapse = ", ")
      } else {
        as.character(raw_auths)
      }
      
      data.frame(
        title = as.character(item$`dc:title` %||% "No Title"),
        authors = auth_final,
        year = substr(item$`prism:coverDate` %||% "", 1, 4),
        journal = as.character(item$`prism:publicationName` %||% ""),
        doi = as.character(item$`prism:doi` %||% ""),
        url = as.character(item$`prism:url` %||% ""),
        abstract = as.character(item$`dc:description` %||% ""),
        stringsAsFactors = FALSE
      )
    })
    
    all_dfs[[length(all_dfs) + 1]] <- df_batch
    total_baixado <- sum(sapply(all_dfs, nrow))
    message(sprintf("ScienceDirect | collected: %s", total_baixado))
    
    # Pagination
    total_disponivel <- as.numeric(payload$`search-results`$`opensearch:totalResults` %||% 0)
    start_idx <- start_idx + batch_size
    
    if (start_idx >= total_disponivel || start_idx >= 6000) break
    Sys.sleep(0.5) # avoid error 419
  }
  
  if (length(all_dfs) == 0) {
    return(make_polaris_result(data.frame(), status = "Success", message = "No results found."))
  }
  
  return(make_polaris_result(dplyr::bind_rows(all_dfs), status = "Success", message = ""))
}

#########################
# Semantic Scholar 
#########################
get_semantic_results <- function(query,
                                 batch_size = 100,
                                 max_pages = Inf,
                                 api_key = NULL,
                                 sort = NULL,
                                 verbose = TRUE,
                                 target_n = 300) {
  
  api_key <- NULL
  batch_size <- 100
  max_pages <- 9999 
  if (is.null(target_n) || is.na(target_n) || target_n <= 0) {
    target_n <- 300
  }
  
  normalize_s2_query <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("")
    x <- as.character(x)
    x <- gsub("\\s*\\bOR\\b\\s*", "|", x, ignore.case = TRUE)
    x <- gsub("\\s*\\|\\s*", "|", x)
    x <- gsub("\\s*\\bAND\\b\\s*", "+", x, ignore.case = TRUE)
    x <- gsub("\\s*\\+\\s*", "+", x)
    x <- gsub("\\s*\\bNOT\\b\\s*", " -", x, ignore.case = TRUE)
    x <- gsub("\\(\\s+", "(", x)
    x <- gsub("\\s+\\)", ")", x)
    return(trimws(gsub("\\s+", " ", x)))
  }
  
  clean_query <- normalize_s2_query(query)
  if (is.null(clean_query) || !nzchar(clean_query)) {
    clean_query <- trimws(as.character(query))
  }
  
  if (verbose) message("🚀 [Shiny Backend] Searching Semantic Scholar (BULK): ", clean_query)
  
  base_url_bulk <- "https://api.semanticscholar.org/graph/v1/paper/search/bulk"
  all_results <- list()
  next_token <- NULL
  page <- 0
  
  repeat {
    page <- page + 1
    if (page > max_pages) break
    
    params <- list(
      query = clean_query,
      limit = 100,
      fields = "title,authors.name,year,abstract,url,venue,externalIds"
    )
    
    if (!is.null(next_token)) {
      params$token <- next_token
    }
    
    res <- tryCatch({
      httr::GET(
        base_url_bulk, 
        query = params, 
        httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
        httr::timeout(30)
      )
    }, error = function(e) NULL)
    
    if (is.null(res)) break
    status <- httr::status_code(res)
    
    if (status == 429 || status == 403) {
      if (verbose) message("⏳  waiting public windoows request 15s)...")
      Sys.sleep(15)
      next
    }
    
    if (status != 200) break
    
    data <- tryCatch({
      jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    }, error = function(e) NULL)
    
    if (is.null(data) || is.null(data$data) || length(data$data) == 0) break
    
    df_page <- purrr::map_df(data$data, function(r) {
      authors_str <- "Unknown"
      if (!is.null(r$authors) && length(r$authors) > 0) {
        extracted <- sapply(r$authors, function(a) {
          if (is.list(a) && !is.null(a$name)) return(as.character(a$name))
          return("")
        })
        extracted <- extracted[extracted != ""]
        if (length(extracted) > 0) authors_str = paste(extracted, collapse = ", ")
      }
      
      doi_str <- ""
      if (!is.null(r$externalIds) && is.list(r$externalIds) && !is.null(r$externalIds$DOI)) {
        doi_str <- as.character(r$externalIds$DOI)
      }
      
      data.frame(
        paperId  = if (!is.null(r$paperId)) as.character(r$paperId) else NA_character_,
        title    = if (!is.null(r$title)) as.character(r$title) else "No Title",
        authors  = authors_str,
        year     = if (!is.null(r$year)) as.integer(r$year) else NA_integer_,
        abstract = if (!is.null(r$abstract)) as.character(r$abstract) else "",
        url      = if (!is.null(r$url)) as.character(r$url) else "",
        venue    = if (!is.null(r$venue)) as.character(r$venue) else "",
        doi      = doi_str,
        stringsAsFactors = FALSE
      )
    })
    
    if (!is.null(df_page) && nrow(df_page) > 0) {
      all_results[[length(all_results) + 1]] <- df_page
    }
    
    total_so_far <- sum(sapply(all_results, nrow))
    if (verbose) message("   📥 Progress: ", total_so_far, " papers downloaded.")
    
    if (total_so_far >= target_n) break
    
    next_token <- data$token
    if (is.null(next_token) || next_token == "") break
    
    Sys.sleep(1.5)
  }
  
  if (length(all_results) == 0) return(data.frame())
  
  final_df <- dplyr::bind_rows(all_results)
  
  if (nrow(final_df) > 0) {
    unique_keys <- paste0(tolower(trimws(final_df$title)), final_df$year)
    final_df <- final_df[!duplicated(unique_keys), ]
    
    if (nrow(final_df) > target_n) {
      final_df <- final_df[seq_len(target_n), , drop = FALSE]
    }
  }
  
  final_df$journal <- final_df$venue
  return(final_df)
}
#==========================
# SKIP MODE + LOG HELPERS
# ==========================
read_queries <- function(path) trimws(readLines(path, warn = FALSE))

should_skip_query <- function(src, query) {
  log_path <- file.path(base_dirs[[src]], "results_log.csv")
  if (!file.exists(log_path)) return(FALSE)
  log <- tryCatch(read.csv(log_path, stringsAsFactors = FALSE), error = function(e) data.frame())
  any(log$query == query & log$status == "Success")
}

append_log <- function(log, query, n, src, status = "Success") {
  rbind(log, data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    source = src,
    group = detect_group(query),
    query = query,
    n_results = n,
    status = status,
    stringsAsFactors = FALSE
  ))
}

# ==========================
# MAIN RUNNER
# ==========================
run_search_for_source <- function(src, queries, results_dir = root_path,
                                  grouping_mode = "none", single_group = "all_queries",
                                  rules_df = NULL, default_group = "other",
                                  skip_done = TRUE, per_query_delay = 1.2,
                                  fetch_abstracts_scopus = FALSE,
                                  api_key = NULL, inst_token = NULL) {
  
  # 1. Log file paths
  dir_src <- file.path(results_dir, paste0("results_", src))
  dir.create(dir_src, recursive = TRUE, showWarnings = FALSE)
  
  log_individual <- file.path(dir_src, paste0("log_", src, ".csv"))
  log_geral      <- file.path(results_dir, "MASTER_LOG.csv")
  
  message("🚀 Loading database: ", src)
  
  for (q in queries) {
    
    # 🛑 # Skip empty or null search queries
    if (is.null(q) || !nzchar(trimws(q))) {
      next
    }
    
    grp <- if (grouping_mode == "single") single_group else detect_group(q)
    outdir <- file.path(dir_src, grp)
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    
    q_safe <- sanitize_filename(q)
    if(nchar(q_safe) > 100) q_safe <- substr(q_safe, 1, 100)
    path_ris <- file.path(outdir, paste0(q_safe, ".ris"))
    
    # Skip queries with existing output when skip_done is enabled
    already_exists <- file.exists(path_ris)
    
    if (skip_done && already_exists) {
      message("⏩ Skipping (existing RIS): ", q_safe)
      next
    }
    
    message("📡 searching query: ", q)
    
    recs <- tryCatch({
      switch(src,
             pubmed = get_pubmed_results(q),
             scopus = get_scopus_results(q, api_key = api_key, inst_token = inst_token, fetch_abstracts_scopus = fetch_abstracts_scopus),
             scidir = get_sciencedirect_test(q, api_key = api_key, inst_token = inst_token),
             semantic_scholar = get_semantic_results(q),
             data.frame())
    }, error = function(e) {
      message("❌ Query error: ", e$message)
      return(make_polaris_result(data.frame(), status = "ERROR_RUNTIME", message = e$message))
    })
    
    meta <- get_polaris_meta(recs)
    recs <- if (is.null(recs)) data.frame() else recs
    n_found <- nrow(recs)
    
    # --- LOG creation ---
    new_entry <- data.frame(
      data_execucao = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      base_dados    = src,
      grupo_pasta   = grp,
      query         = q,
      total_obtido  = n_found,
      arquivo_ris   = basename(path_ris),
      status        = meta$status,
      message       = meta$message,
      stringsAsFactors = FALSE
    )
    
    # 2. Write source-specific log
    write.table(new_entry, log_individual, sep = ",", row.names = FALSE, 
                col.names = !file.exists(log_individual), append = file.exists(log_individual))
    
    # 3. Update the master log
    write.table(new_entry, log_geral, sep = ",", row.names = FALSE, 
                col.names = !file.exists(log_geral), append = file.exists(log_geral))
    
    # --- POST-PROCESSING (standard mode) ---
    if (n_found > 0 && fetch_abstracts_scopus) {
      # 1.# 1. Identify records with missing abstracts 
      indices_vazios <- which((is.na(recs$abstract) | recs$abstract == "" | nchar(recs$abstract) < 20) & 
                                (!is.na(recs$doi) & recs$doi != ""))
      
      if (length(indices_vazios) > 0) {
        message(sprintf("🔄 Enriching %d abstracts via CrossRef..", length(indices_vazios)))
        
        # 2. Process records in batches
        for (idx in indices_vazios) {
          # Retrieve abstract from CrossRef
          abs_rec <- get_abstract_from_crossref(recs$doi[idx])
          
          if (nzchar(abs_rec)) {
            recs$abstract[idx] <- abs_rec
          }
          
          # Display progress every 50 records
          if (which(indices_vazios == idx) %% 50 == 0) {
            message(sprintf("  [Proc: %d/%d]", which(indices_vazios == idx), length(indices_vazios)))
          }
        }
      }
    }
    
    # 4. Write records to a RIS file
    if (n_found > 0) {
      ris_data <- make_ris(recs)
      if (length(ris_data) > 0) {
        write_ris_file(ris_data, path_ris)
        message("✅ ", n_found, " references saved at: ", grp)
      }
    }
    
    # Save result metadata for the worker/UI
    last_result <- list(
      n_found = n_found,
      status = meta$status,
      message = meta$message
    )
    
    Sys.sleep(per_query_delay)
  }
  
  return(last_result %||% list(
    n_found = 0,
    status = "Success",
    message = ""
  ))
}