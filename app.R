# README (quick start)
# - Project tree expected:
#     SNQ Pipeline/
#       app.R
#       www/            # <- put your lab logo as www/logo.png (optional)
#                        # <- put your Qualtrics file as www/snq.qsf
#       data/           # intake & issue logs will be saved here
# - Run with: shiny::runApp()
# - If www/snq.qsf is missing, a minimal placeholder QSF is created on first run.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(jsonlite)
  library(readr)
  library(readxl)
  library(writexl)
  library(ggplot2)
  library(stringr)
  library(shinyjs)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# Helpers: write/copy only when content changes to avoid Shiny file-watcher loops
# ------------------------------------------------------------------------------
safe_write_lines <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "safe_write_", fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lines, tmp, useBytes = TRUE)
  if (!file.exists(path) || !identical(readBin(tmp, what = "raw", n = file.info(tmp)$size),
                                       readBin(path, what = "raw", n = if (file.exists(path)) file.info(path)$size else 0))) {
    file.copy(tmp, path, overwrite = TRUE)
  }
}

safe_copy_if_changed <- function(from, to, overwrite = TRUE) {
  if (!file.exists(from)) return(FALSE)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(to)) return(file.copy(from, to, overwrite = overwrite))
  src <- readBin(from, what = "raw", n = file.info(from)$size)
  dst <- readBin(to,   what = "raw", n = file.info(to)$size)
  if (!identical(src, dst)) return(file.copy(from, to, overwrite = overwrite))
  TRUE
}

# Stage Data Dictionary assets into www/ so images resolve in-browser
try({
  dd_src_html <- "/Users/beiniou/Desktop/STEEG/www/DataDictionary.html"
  # Point directly to the images folder so we don't nest images/images
  dd_src_img_dir <- "/Users/beiniou/Desktop/STEEG/www/data_dictionary/images"
  dd_www_dir <- file.path("www")
  dd_www_img_dir <- file.path("www", "data_dictionary", "images")
  if (!dir.exists(dd_www_img_dir)) dir.create(dd_www_img_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(dd_src_img_dir)) {
    src_files <- list.files(dd_src_img_dir, full.names = TRUE)
    for (src in src_files) {
      dest <- file.path(dd_www_img_dir, basename(src))
      safe_copy_if_changed(src, dest, overwrite = TRUE)
    }
  }
  if (file.exists(dd_src_html)) {
    h <- readLines(dd_src_html, warn = FALSE)
    h <- gsub('src="images/', 'src="data_dictionary/images/', h)
    h <- gsub("src='images/", "src='data_dictionary/images/", h)
    safe_write_lines(h, file.path(dd_www_dir, "DataDictionary.html"))
  }
}, silent = TRUE)

# Stage Qualtrics Help HTML into www/ so it can load in iframe
try({
  help_src_dir <- "/Users/beiniou/Desktop/Child Social Network Questionnaire Qualtrics Survey Help Sheet /images"
  help_src_html <- file.path(help_src_dir, "ChildSocialNetworkQuestionnaireQualtricsSurve.html")
  help_www_dir <- file.path("www", "qualtrics_help")
  dir.create(help_www_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(help_src_dir)) {
    src_files <- list.files(help_src_dir, full.names = TRUE)
    for (src in src_files) {
      dest <- file.path(help_www_dir, basename(src))
      safe_copy_if_changed(src, dest, overwrite = TRUE)
    }
  }
}, silent = TRUE)

# SNQ Data Processing Function (embedded directly)
process_snq_data_simple <- function(df) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  cat("Processing SNQ data...\n")
  
  # Find the actual data rows (skip header rows)
  data_start <- which(grepl("Testing", df[[18]]))
  if (length(data_start) > 0) {
    df <- df[data_start[1]:nrow(df), ]
    cat("Data rows found starting at row:", data_start[1], "\n")
    cat("Selected rows:", nrow(df), "\n")
  }
  
  # Process ego-level data
  ego_level_network_summary <- df %>%
    mutate(
      childcare_type = ifelse(!is.na(childcare_type_8_TEXT) & childcare_type_8_TEXT != '',
                              ifelse(childcare_type == '' | is.na(childcare_type),
                                     childcare_type_8_TEXT,
                                     paste(childcare_type, childcare_type_8_TEXT, sep = ', ')),
                              childcare_type),
      child_gender = ifelse(!is.na(child_gender_3_TEXT) & trimws(child_gender_3_TEXT) != '',
                            trimws(child_gender_3_TEXT), child_gender),
      child_race = ifelse(!is.na(child_race_6_TEXT) & trimws(child_race_6_TEXT) != '',
                          ifelse(child_race == '' | is.na(child_race),
                                 trimws(child_race_6_TEXT),
                                 paste(child_race, trimws(child_race_6_TEXT), sep = ', ')),
                          child_race)
    ) %>%
    select(ChildID, child_name, EndDate, Finished, birthdate_1, age_in_months_1, child_gender, child_race, child_race_detail, child_zipcode, childcare_yn, childcare_type, name_filloutsurvey_1, child_lang) %>%
    rename(child_birthdate = birthdate_1, child_age_in_months = age_in_months_1, survey_respondent = name_filloutsurvey_1, survey_enddate = EndDate, survey_completion = Finished)
  
  cat("Ego level summary created - Rows:", nrow(ego_level_network_summary), "\n")
  
  # Process node-level data (simplified)
  node_level_long <- data.frame(
    ChildID = character(0),
    ego_age_in_months = numeric(0),
    ego_gender = character(0),
    ego_race = character(0),
    ego_language = character(0),
    node_type = character(0),
    node_index = integer(0),
    node_name = character(0),
    node_relationship = character(0),
    node_liveathome_or_not = character(0),
    stringsAsFactors = FALSE
  )
  
  # Extract all node types
  node_patterns <- list(
    "sibling" = "Sibling",
    "liveathome" = "Liveathome", 
    "teacher" = "Teacher",
    "schoolkid" = "Schoolkid",
    "caregiver" = "Caregiver",
    "ext_fam" = "Extended family",
    "othernode" = "Other node",
    "act1_kid" = "Act1Kid",
    "act1_adult" = "Act1Adult"
  )
  
  for (i in 1:nrow(df)) {
    child_id <- df$ChildID[i]
    child_data <- df[i, ]
    
    # Extract nodes for each pattern
    for (node_prefix in names(node_patterns)) {
      pattern_cols <- grep(paste0("^", node_prefix, "_[0-9]+_[0-9]+"), names(child_data), value = TRUE)
      
      if (length(pattern_cols) > 0) {
        for (col in pattern_cols) {
          if (!is.na(child_data[[col]]) && child_data[[col]] != "") {
            # Extract node index from column name
            node_index <- as.integer(str_extract(col, paste0(node_prefix, "_([0-9]+)_"), group = 1))
            field_num <- as.integer(str_extract(col, "_([0-9]+)$", group = 1))
            
            if (field_num == 1) {  # Name field
              node_name <- child_data[[col]]
              
              # Look for corresponding relationship field
              rel_col <- gsub("_1$", "_2", col)
              node_relationship <- if (rel_col %in% names(child_data)) child_data[[rel_col]] else ""
              
              # Determine if lives at home
              node_liveathome_or_not <- if (node_prefix %in% c("sibling", "liveathome")) "yes" else "no"
              
              # Add to node data
              new_node <- data.frame(
                ChildID = child_id,
                ego_age_in_months = ego_level_network_summary$child_age_in_months[match(child_id, ego_level_network_summary$ChildID)],
                ego_gender = ego_level_network_summary$child_gender[match(child_id, ego_level_network_summary$ChildID)],
                ego_race = ego_level_network_summary$child_race[match(child_id, ego_level_network_summary$ChildID)],
                ego_language = ego_level_network_summary$child_lang[match(child_id, ego_level_network_summary$ChildID)],
                node_type = node_patterns[[node_prefix]],
                node_index = node_index,
                node_name = node_name,
                node_relationship = node_relationship,
                node_liveathome_or_not = node_liveathome_or_not,
                stringsAsFactors = FALSE
              )
              node_level_long <- rbind(node_level_long, new_node)
            }
          }
        }
      }
    }
  }
  
  cat("Node level long created - Rows:", nrow(node_level_long), "\n")
  
  # Process activity-level data (simplified)
  activity_level_long <- df %>%
    select(ChildID, starts_with("act1_")) %>%
    filter(!is.na(act1_otherkids) & act1_otherkids != "") %>%
    mutate(
      activity_id = 1,
      activity_label = "act1",
      activity_name = "Activity 1",
      activity_otherkids_yn = act1_otherkids,
      activity_otherkids_n = suppressWarnings(as.numeric(act1_otherkids_2_TEXT)),
      activity_child_age = suppressWarnings(as.numeric(act1_child_age)),
      activity_gender = act1_gender,
      activity_race = act1_race,
      activity_contact = act1_contact,
      activity_lang = act1_lang,
      activity_adults = act1_adults,
      activity_kids = act1_kids
    ) %>%
    select(
      ChildID, activity_id, activity_label, activity_name, activity_otherkids_yn,
      activity_otherkids_n, activity_child_age, activity_gender, activity_race,
      activity_contact, activity_lang, activity_adults, activity_kids
    )
  
  cat("Activity level long created - Rows:", nrow(activity_level_long), "\n")
  
  return(list(
    ego_level_network_summary = ego_level_network_summary,
    node_level_long = node_level_long,
    activity_level_long = activity_level_long
  ))
}

# ============== Admin Configuration ==============
# Use environment variable for admin password, fallback to default for development
ADMIN_PASSWORD <- Sys.getenv("SNQ_ADMIN_PASSWORD", "woodwardlab")

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

# Null-coalescer helper
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ------------------------------------------------------------------------------
# Ensure folders exist
# ------------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("www",  showWarnings = FALSE, recursive = TRUE)

# If a project-root image.png exists, copy it to www/logo.png so it is served
if (file.exists("image.png") && !file.exists(file.path("www","logo.png"))) {
  file.copy("image.png", file.path("www","logo.png"), overwrite = TRUE)
}

# Expose external dashboard assets via URL without modifying their CSS/JS
shiny::addResourcePath("snq_dash", "/Users/beiniou/Desktop/STEEG")

# ------------------------------------------------------------------------------
# Ensure there is a QSF to serve; if missing, write a minimal placeholder
# ------------------------------------------------------------------------------
canonical_qsf_path <- file.path("www", "snq.qsf")
user_canonical_qsf <- "/Users/beiniou/Desktop/STEEG/Social_Network_Questionnaire_-_Bethany_working (1).qsf"
if (file.exists(user_canonical_qsf)) {
  # Prefer user's provided canonical file
  file.copy(user_canonical_qsf, canonical_qsf_path, overwrite = TRUE)
}
if (!file.exists(canonical_qsf_path)) {
  minimal_qsf <- list(
    SurveyEntry = list(SurveyName = "SNQ Canonical (Minimal)", SurveyID = "SV_PLACEHOLDER"),
    SurveyElements = list(
      list(
        Element = "SQ",
        PrimaryAttribute = "QID_INTRO",
        Payload = list(
          QuestionID    = "QID_INTRO",
          QuestionType  = "DB",
          DataExportTag = "intro_1",
          QuestionText  = "{{INTRO_PARAGRAPH}}"
        )
      ),
      list(
        Element = "SQ",
        PrimaryAttribute = "QID_CHILDID",
        Payload = list(
          QuestionID    = "QID_CHILDID",
          QuestionType  = "TE",
          DataExportTag = "child_id",
          QuestionText  = "Please enter child identifier: {{CHILD_ID_LABEL}}"
        )
      )
    )
  )
  writeLines(toJSON(minimal_qsf, auto_unbox = TRUE, pretty = TRUE), canonical_qsf_path)
}

# ------------------------------------------------------------------------------
# Access Survey Module
# ------------------------------------------------------------------------------

# UI Function
accessSurveyUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Custom CSS matching UChicago style
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600;700&display=swap');
        
        body {
          font-family: 'Source Sans Pro', sans-serif;
          background-color: #f8f9fa;
        }
        
        .page-header {
          text-align: center;
          margin-bottom: 3rem;
        }
        
        .logo-container {
          margin-bottom: 2rem;
        }
        
        .logo-image {
          max-width: 100%;
          height: auto;
        }
        
        .main-content {
          background: white;
          border-radius: 8px;
          padding: 3rem;
          margin-bottom: 2rem;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        
        .page-title {
          color: #333;
          font-size: 3rem;
          font-weight: 700;
          margin-bottom: 0.5rem;
          line-height: 1.2;
        }
        
        .page-subtitle {
          color: #333;
          font-size: 2.5rem;
          font-weight: 700;
          margin-bottom: 2rem;
          line-height: 1.2;
        }
        
        .title-underline {
          width: 80px;
          height: 4px;
          background: #8B0000;
          margin: 0 auto 2rem auto;
        }
        
        .download-section {
          background: #f8f9fa;
          border: 2px solid #8B0000;
          border-radius: 8px;
          padding: 2rem;
          text-align: center;
          margin: 2rem 0;
        }
        
        .download-title {
          color: #8B0000;
          font-size: 1.5rem;
          font-weight: 600;
          margin-bottom: 1rem;
        }
        
        .download-description {
          color: #666;
          font-size: 1.1rem;
          margin-bottom: 1.5rem;
          line-height: 1.6;
        }
        
        .download-btn {
          background: #8B0000;
          color: white;
          border: none;
          padding: 12px 30px;
          border-radius: 4px;
          font-size: 1.1rem;
          font-weight: 600;
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        
        .download-btn:hover {
          background: #A00000;
          color: white;
          transform: translateY(-1px);
        }
        
        .instructions-section {
          margin-top: 3rem;
        }
        
        .section-title {
          color: #333;
          font-size: 2rem;
          font-weight: 600;
          margin-bottom: 2rem;
          text-align: center;
        }
        
        .instruction-step {
          background: white;
          border: 1px solid #e0e0e0;
          border-radius: 12px;
          padding: 2rem;
          margin-bottom: 1.5rem;
          transition: box-shadow 0.3s ease;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        
        .instruction-step:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }
        
        .step-header {
          display: flex;
          align-items: center;
          margin-bottom: 1rem;
          gap: 2rem;
        }
        
        .step-number {
          background: #8B0000;
          color: white;
          width: 40px;
          height: 40px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: 700;
          flex-shrink: 0;
          font-size: 1.2rem;
        }
        
        .step-title {
          color: #333;
          font-size: 1.4rem;
          font-weight: 600;
          margin: 0;
          flex: 1;
        }
        
        .step-content {
          color: #666;
          font-size: 1rem;
          line-height: 1.6;
          margin-left: 0;
          padding-left: 0;
        }
        
        .info-callout {
          background: #e3f2fd;
          border-left: 4px solid #2196f3;
          padding: 1rem;
          margin: 1rem 0;
          border-radius: 0 4px 4px 0;
        }
        
        .warning-callout {
          background: #fff3e0;
          border-left: 4px solid #ff9800;
          padding: 1rem;
          margin: 1rem 0;
          border-radius: 0 4px 4px 0;
        }
        
        .feature-list {
          list-style: none;
          padding: 0;
          margin: 1rem 0;
        }
        
        .feature-list li {
          padding: 0.6rem 0;
          position: relative;
          padding-left: 1.8rem;
          color: #555;
          line-height: 1.5;
        }
        
        .feature-list li:before {
          content: '•';
          position: absolute;
          left: 0;
          color: #8B0000;
          font-weight: bold;
          font-size: 1.3rem;
          top: 0.6rem;
        }
      "))
    ),
    
    # Page Header
    div(class = "page-header",
      # Title Section
      h1(class = "page-title", "Access Survey"),
      div(class = "title-underline")
    ),
    
    # Main Content Container
    div(class = "container-fluid",
      div(class = "main-content",
        # Download Section
        div(class = "download-section",
          h3(class = "download-title", "Download CSNQ Survey File"),
          p(class = "download-description", 
            "Download the Child Social Network Questionnaire (.qsf) file for direct import into your Qualtrics account."),
          downloadButton(ns("downloadSurvey"), 
                        "Download CSNQ.qsf File", 
                        class = "download-btn",
                        icon = icon("download"))
        )
      ),
      
      # Instructions Section
      div(class = "main-content instructions-section",
        h2(class = "section-title", "Qualtrics Implementation Guide"),
        
        # Step 1
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "1"),
            h3(class = "step-title", "Download Survey File")
          ),
          div(class = "step-content",
            p("Click the download button above to obtain the CSNQ (.qsf) file. This file contains the complete survey structure ready for Qualtrics import.")
          )
        ),
        
        # Step 2
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "2"),
            h3(class = "step-title", "Access Qualtrics")
          ),
          div(class = "step-content",
            p("Log into your Qualtrics account and ensure you have permissions to create new surveys and import .qsf files."),
            div(class = "info-callout",
              strong("Note: "), "Contact your institution's Qualtrics administrator if you need import permissions."
            )
          )
        ),
        
        # Step 3
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "3"),
            h3(class = "step-title", "Import to Qualtrics")
          ),
          div(class = "step-content",
            p("Create a new project and import the survey file:"),
            tags$ul(class = "feature-list",
              tags$li("Click 'Create Project' in Qualtrics"),
              tags$li("Select 'Survey' as project type"),
              tags$li("Choose 'From a QSF file' option"),
              tags$li("Upload the downloaded CSNQ.qsf file"),
              tags$li("Complete the import wizard")
            )
          )
        ),
        
        # Step 4
        div(class = "instruction-step",
          div(class = "step-header",
            div(class = "step-number", "4"),
            h3(class = "step-title", "Review and Deploy")
          ),
          div(class = "step-content",
            p("After import, review the survey structure and configure for your research needs:"),
            div(class = "warning-callout",
              strong("Important: "), "Review all question logic and validation rules before deploying to participants."
            )
          )
        ),
        
        # What's Included Section
        div(class = "instruction-step",
          h3(class = "step-title", style = "margin-bottom: 1rem;", "Survey Components"),
          div(class = "step-content", style = "margin-left: 0;",
            p("The CSNQ includes:"),
            tags$ul(class = "feature-list",
              tags$li("Social network mapping questions"),
              tags$li("Relationship quality assessments"),
              tags$li("Demographic measures"),
              tags$li("Pre-configured logic and validation"),
              tags$li("Mobile-responsive design")
            )
          )
        ),
        
        # Support Section
        div(class = "instruction-step",
          h3(class = "step-title", style = "margin-bottom: 1rem;", "Need Help?"),
          div(class = "step-content", style = "margin-left: 0;",
            p("For technical support:"),
            tags$ul(class = "feature-list",
              tags$li("Use the form under Help & Contact page to submit a request"),
              tags$li("Contact your institutional Qualtrics support team"),
              tags$li(tags$a(href = "#qualtrics_help", "Review the Qualtrics Help Doc", 
                            onclick = "Shiny.setInputValue('nav', 'qualtrics_help', {priority: 'event'}); return false;",
                            style = "color: #8B0000; text-decoration: none; font-weight: 600;",
                            " - Complete walkthrough of survey logic and flow"))
            )
          )
        )
      )
    )
  )
}

# Server Function
accessSurveyServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Download handler for the survey file
    output$downloadSurvey <- downloadHandler(
      filename = function() {
        "Child_Social_Network_Questionnaire.qsf"
      },
        content = function(file) {
          # Always serve the canonical file we've ensured in www/
          if (file.exists(canonical_qsf_path)) {
            file.copy(canonical_qsf_path, file, overwrite = TRUE)
          } else {
            writeLines("Error: canonical QSF not found at www/snq.qsf.", file)
          }
        },
      contentType = "application/octet-stream"
    )
    
    # Handle survey access request
    observeEvent(input$submitRequest, {
      # Validate inputs
      if (input$requestName == "" || input$requestEmail == "") {
        showNotification("Please provide both name and email address.", type = "warning")
        return()
      }
      
      # Validate email format
      if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$requestEmail)) {
        showNotification("Please enter a valid email address.", type = "warning")
        return()
      }
      
      # Prepare email content
      subject <- paste("QSF Request from", input$requestName)
      body <- paste(
        "Survey Access Request",
        "========================",
        "",
        "Name:", input$requestName,
        "Email:", input$requestEmail,
        "Institution:", ifelse(input$requestInstitution == "", "Not provided", input$requestInstitution),
        "",
        "This person is requesting access to the Child Social Network Questionnaire (CSNQ) due to Qualtrics import limitations.",
        "",
        "Next steps:",
        "1. Share the survey with their Qualtrics account",
        "2. Grant them edit permissions",
        "3. Instruct them to duplicate the survey",
        "4. Remove access after they confirm successful duplication",
        "",
        "Generated automatically from the CSNQ Access Survey page.",
        sep = "\n"
      )
      
      # Try to send email (Note: This requires email configuration in your Shiny server)
      tryCatch({
        # You'll need to configure email sending in your server environment
        # This is a placeholder - replace with your preferred email method
        
        # For now, we'll show a confirmation and you can manually check logs
        showNotification(
          paste("Request submitted successfully for", input$requestName, ". You will be contacted within 24-48 hours."),
          type = "default",
          duration = 10
        )
        
        # Log the request (you can check server logs)
        cat("\n=== QSF REQUEST ===\n")
        cat("Subject:", subject, "\n")
        cat("Body:\n", body, "\n")
        cat("Timestamp:", Sys.time(), "\n")
        cat("==================\n")
        
        # Clear form
        updateTextInput(session, "requestName", value = "")
        updateTextInput(session, "requestEmail", value = "")
        updateTextInput(session, "requestInstitution", value = "")
        
      }, error = function(e) {
        showNotification("There was an error submitting your request. Please try again later.", type = "error")
        cat("Email sending error:", e$message, "\n")
      })
    })
  })
}

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
badge <- function(text, cls = "bg-secondary") tags$span(class = paste("badge", cls), text)

# Logging function for debugging
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] [", level, "] ", message)
  
  # Write to console
  cat(log_entry, "\n")
  
  # Write to log file if it exists
  log_file <- file.path("data", "app.log")
  if (dir.exists("data")) {
    write(log_entry, file = log_file, append = TRUE)
  }
}

# Safe file operations with error handling
safe_file_operation <- function(operation, error_message = "File operation failed") {
  tryCatch({
    operation()
  }, error = function(e) {
    log_message(paste(error_message, ":", conditionMessage(e)), "ERROR")
    stop(error_message, ": ", conditionMessage(e))
  })
}

# Process SNQ data using your converted R script
process_snq_data <- function(raw_data, raw_filename) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    stop("No data provided for processing")
  }
  
  tryCatch({
    # Create a clean temporary environment
    temp_dir <- tempdir()
    old_wd <- getwd()
    
    # Ensure we return to original directory even if error occurs
    on.exit(setwd(old_wd))
    
    # Set working directory to temp for processing
    setwd(temp_dir)
    
     # Save the input data with the exact name your script expects
     file_extension <- tolower(tools::file_ext(raw_filename %||% "csv"))
     if (file_extension == "xlsx") {
      # For Excel files, save as Excel
      writexl::write_xlsx(raw_data, "input_data.xlsx")
    } else {
      # For CSV files, save as CSV
      write_csv(raw_data, "input_data.csv")
    }
    
    # Create a wrapper script that sources your pipeline
    wrapper_script <- tempfile(fileext = ".R")
    
    wrapper_code <- paste0('
# Set up environment
suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(lubridate)
})

# Read the input data - make sure this matches what your script expects
if (file.exists("input_data.xlsx")) {
  df <- readxl::read_excel("input_data.xlsx")
} else {
  df <- read_csv("input_data.csv", show_col_types = FALSE)
}

# Source your converted R script
source("/Users/beiniou/Desktop/STEEG/snq_pipeline.R.bak")

# Your script should create these objects:
# - node_level_long
# - ego_level_network_summary  
# - activity_level_long

# Save the results
if (exists("node_level_long")) {
  write_csv(node_level_long, "node_level_long.csv")
} else {
  stop("node_level_long was not created by the pipeline")
}

if (exists("ego_level_network_summary")) {
  write_csv(ego_level_network_summary, "ego_level_network_summary.csv")  
} else {
  stop("ego_level_network_summary was not created by the pipeline")
}

if (exists("activity_level_long")) {
  write_csv(activity_level_long, "activity_level_long.csv")
} else {
  stop("activity_level_long was not created by the pipeline")
}
    ')
    
    writeLines(wrapper_code, wrapper_script)
    
    # Run the wrapper script
    result <- system2("Rscript", args = wrapper_script, 
                     stdout = TRUE, stderr = TRUE, 
                     wait = TRUE)
    
    # Check for execution errors
    if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
      error_msg <- paste("R script execution failed:", paste(result, collapse = "\n"))
      stop(error_msg)
    }
    
    # Verify output files exist
    output_files <- c("node_level_long.csv", "ego_level_network_summary.csv", "activity_level_long.csv")
    missing_files <- output_files[!file.exists(output_files)]
    
    if (length(missing_files) > 0) {
      stop("Pipeline did not generate expected files: ", paste(missing_files, collapse = ", "))
    }
    
    # Read the results
    node_level_long <- read_csv("node_level_long.csv", show_col_types = FALSE)
    ego_level_network_summary <- read_csv("ego_level_network_summary.csv", show_col_types = FALSE)
    activity_level_long <- read_csv("activity_level_long.csv", show_col_types = FALSE)
    
    # Clean up temporary files
    unlink(c(wrapper_script, output_files, "input_data.csv", "input_data.xlsx"))
    
    return(list(
      node_level_long = node_level_long,
      ego_level_network_summary = ego_level_network_summary,
      activity_level_long = activity_level_long
    ))
    
  }, error = function(e) {
    # Ensure we return to original directory on error
    setwd(old_wd)
    stop("Error processing SNQ data: ", conditionMessage(e))
  })
}

# ------------------------------------------------------------------------------
# THEME (vibrant)
# ------------------------------------------------------------------------------
theme <- bs_theme(
  version = 5,
    bootswatch = "minty",
  primary = "#0a7ac4",
    secondary = "#6c757d",
    "font-size-base" = "0.875rem",
    "h1-font-size" = "2rem",
    "h2-font-size" = "1.75rem", 
    "h3-font-size" = "1.5rem",
    "h4-font-size" = "1.25rem",
    "h5-font-size" = "1.1rem",
    "h6-font-size" = "1rem",
    "line-height-base" = 1.4,
    "font-size-sm" = "0.75rem",
    "font-size-lg" = "1.125rem"
)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
# Increase upload size limit to 50MB
options(shiny.maxRequestSize = 50*1024*1024)

ui <- page_navbar(
  id    = "nav",
  title = "SNQ Distribution Hub",
  theme = theme,

  header = tagList(
    useShinyjs(),
    # Font Awesome for crisp Retina icons
     tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
     # Ensure Bootstrap JS is available
    tags$style(HTML("
        /* Desktop scaling fixes */
        html { font-size: 0.875rem; }
        body { 
          background: linear-gradient(160deg, #f7f7f7 0%, #eaf3fb 100%); 
          font-size: 0.875rem;
          line-height: 1.4;
        }
        
        /* Control heading sizes */
        h1, .h1 { font-size: 2rem !important; font-weight: 700; }
        h2, .h2 { font-size: 1.75rem !important; font-weight: 600; }
        h3, .h3 { font-size: 1.5rem !important; font-weight: 600; }
        h4, .h4 { font-size: 1.25rem !important; font-weight: 600; }
        h5, .h5 { font-size: 1.1rem !important; font-weight: 600; }
        h6, .h6 { font-size: 1rem !important; font-weight: 600; }
        
        /* Control card and container sizes */
        .card { 
          font-size: 14px;
          border-radius: 8px;
        }
        .card-body { padding: 1.25rem; }
        .card-header { 
          padding: 0.75rem 1.25rem; 
          font-size: 1.1rem;
          font-weight: 600;
        }
        
        /* Control button sizes */
        .btn { 
          font-size: 0.875rem;
          padding: 0.5rem 1rem;
          border-radius: 0.375rem;
        }
        .btn-lg { 
          font-size: 1rem;
          padding: 0.75rem 1.5rem;
        }
        .btn-sm { 
          font-size: 0.75rem;
          padding: 0.25rem 0.5rem;
        }
        
        /* Control form elements */
        .form-control, .form-select {
          font-size: 0.875rem;
          padding: 0.5rem 0.75rem;
          border-radius: 0.375rem;
        }
        .form-label {
          font-size: 0.875rem;
          font-weight: 600;
          margin-bottom: 0.5rem;
        }
        
        /* Control spacing */
        .mb-1 { margin-bottom: 0.25rem !important; }
        .mb-2 { margin-bottom: 0.5rem !important; }
        .mb-3 { margin-bottom: 1rem !important; }
        .mb-4 { margin-bottom: 1.5rem !important; }
        .mb-5 { margin-bottom: 3rem !important; }
        
        .mt-1 { margin-top: 0.25rem !important; }
        .mt-2 { margin-top: 0.5rem !important; }
        .mt-3 { margin-top: 1rem !important; }
        .mt-4 { margin-top: 1.5rem !important; }
        .mt-5 { margin-top: 3rem !important; }
        
        .p-1 { padding: 0.25rem !important; }
        .p-2 { padding: 0.5rem !important; }
        .p-3 { padding: 1rem !important; }
        .p-4 { padding: 1.5rem !important; }
        .p-5 { padding: 3rem !important; }
        
        /* Control navbar */
        .navbar-brand { 
          font-weight: 700; 
          letter-spacing: 0.3px;
          font-size: 1.25rem;
        }
        .navbar-nav .nav-link {
          font-size: 0.875rem;
          padding: 0.5rem 1rem;
        }
        
        /* Control container max-widths */
        .container-fluid { max-width: 1400px; margin: 0 auto; }
        .container { max-width: 1200px; }
        
        /* Control grid spacing */
        .row { margin-left: -0.75rem; margin-right: -0.75rem; }
        .row > * { padding-left: 0.75rem; padding-right: 0.75rem; }
        
        /* Control alert sizes */
        .alert {
          font-size: 0.875rem;
          padding: 0.75rem 1rem;
          border-radius: 0.375rem;
        }
        
        /* Control table sizes */
        .table {
          font-size: 0.875rem;
        }
        
        /* Control modal sizes */
        .modal-dialog { max-width: 600px; }
        .modal-header { padding: 1rem 1.5rem; }
        .modal-body { padding: 1.5rem; }
        .modal-footer { padding: 1rem 1.5rem; }
        
        /* Responsive adjustments */
        @media (min-width: 1200px) {
          .container-fluid { max-width: 1600px; }
        }
        
        @media (max-width: 768px) {
          html { font-size: 0.8125rem; }
          body { font-size: 0.8125rem; }
        }
      .welcome-hero { background: #f8f9fa; border-radius: 8px; }
      .welcome-card {
        max-width: 900px; margin: 0 auto; background: #ffffff; border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1); padding: 3rem;
      }
      .welcome-title { font-weight: 700; font-size: 3rem; line-height: 1.2; margin-bottom: 0.5rem; text-align: center; color: #333; }
      .page-title {
        color: #333; font-size: 3rem; font-weight: 700; margin-bottom: 0.5rem; line-height: 1.2; text-align: center;
      }
      .page-subtitle {
        color: #333; font-size: 2.5rem; font-weight: 700; margin-bottom: 2rem; line-height: 1.2; text-align: center;
      }
      .accent-bar { height: 4px; width: 80px; background: #8B0000; border-radius: 3px; margin: 0 auto 2rem auto; }
      .title-underline {
        width: 80px; height: 4px; background: #8B0000; margin: 0 auto 2rem auto;
      }
      .welcome-hero h2, .welcome-hero h3 { font-weight: 800; color: #4a4a4a; }
      .welcome-hero h3 { color: #6b2a2a; }
      .welcome-hero p, .welcome-hero li { line-height: 1.7; font-size: 1.06rem; }
      .welcome-logo { display:flex; justify-content:center; align-items:center; margin-bottom: 12px; }
      .welcome-logo img { height: 160px; max-width: 100%; object-fit: contain; }
      .section-gap { margin-top: 1.25rem; }
      .disabled { pointer-events: auto !important; cursor:not-allowed; opacity:.6; }
      
      /* Enhanced Navbar Styling */
      .navbar-nav { 
        display: flex; 
        flex-wrap: nowrap; 
        overflow-x: auto; 
        overflow-y: hidden; 
        -webkit-overflow-scrolling: touch;
        scrollbar-width: none;
        -ms-overflow-style: none;
      }
      .navbar-nav::-webkit-scrollbar { display: none; }
      .nav-item { flex-shrink: 0; }
      .nav-link { 
        display: flex; 
        align-items: center; 
        gap: 0.5rem; 
        white-space: nowrap; 
        padding: 0.75rem 1rem;
        transition: all 0.3s ease;
        position: relative;
      }
      .nav-link.active { 
        font-weight: 700; 
        border-bottom: 3px solid #0d6efd;
        background-color: rgba(13, 110, 253, 0.1);
      }
      .nav-link:hover { 
        background-color: rgba(13, 110, 253, 0.05); 
        transform: translateY(-1px);
      }
      .nav-icon { 
        width: 1.2em; 
        height: 1.2em; 
        flex-shrink: 0;
        filter: drop-shadow(0 1px 2px rgba(0,0,0,0.1));
      }
      
      /* Scroll indicators */
      .nav-scroll-container { position: relative; }
      .nav-scroll-indicator { 
        position: absolute; 
        top: 50%; 
        transform: translateY(-50%); 
        z-index: 10; 
        background: rgba(255,255,255,0.9); 
        border: 1px solid #dee2e6; 
        border-radius: 50%; 
        width: 32px; 
        height: 32px; 
        display: flex; 
        align-items: center; 
        justify-content: center; 
        cursor: pointer; 
        transition: all 0.3s ease;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .nav-scroll-indicator:hover { 
        background: #0d6efd; 
        color: white; 
        transform: translateY(-50%) scale(1.1);
      }
      .nav-scroll-left { left: 10px; }
      .nav-scroll-right { right: 10px; }
      .nav-scroll-indicator.hidden { opacity: 0; pointer-events: none; }
      
      /* Access page styling */
      .access-title { 
        font-weight: 700; 
        color: #2c3e50; 
        font-size: 2.5rem;
      }
      .access-subtitle { 
        font-size: 1.1rem; 
        max-width: 600px; 
        margin: 0 auto;
      }
      .access-form-card { 
        border-radius: 20px; 
        background: linear-gradient(145deg, #ffffff 0%, #f8f9fa 100%);
        border: 1px solid rgba(0,123,255,0.1);
      }
      .form-title { 
        color: #495057; 
        font-weight: 600; 
        font-size: 1.4rem;
      }
      .form-description { 
        font-size: 1rem; 
        line-height: 1.5;
      }
      .form-group { 
        position: relative; 
      }
      .form-label { 
        color: #495057; 
        margin-bottom: 0.5rem; 
        font-size: 1rem;
      }
      .form-control-lg { 
        border-radius: 12px; 
        border: 2px solid #e9ecef; 
        padding: 0.75rem 1rem; 
        font-size: 1rem;
        transition: all 0.3s ease;
      }
      .form-control-lg:focus { 
        border-color: #0d6efd; 
        box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.15); 
        transform: translateY(-1px);
      }
      .form-text { 
        font-size: 0.9rem; 
        margin-top: 0.5rem; 
        line-height: 1.4;
      }
       .btn-primary { 
         background: #8B2323; 
         border: none; 
         border-radius: 12px; 
         font-weight: 600; 
         font-size: 1.1rem;
         transition: all 0.3s ease;
       }
       .btn-primary:hover { 
         background: #6B1A1A; 
         transform: translateY(-2px); 
         box-shadow: 0 8px 25px rgba(139, 35, 35, 0.3);
       }
      .privacy-note { 
        background: #f8f9fa; 
        padding: 1rem; 
        border-radius: 12px; 
        border-left: 4px solid #6c757d;
      }
      .form-check { 
        border-radius: 12px; 
        transition: all 0.3s ease;
      }
      .form-check:hover { 
        background-color: #f8f9fa !important;
      }
      .form-check-input:checked { 
        background-color: #198754; 
        border-color: #198754;
      }
      
      /* Responsive adjustments */
      @media (max-width: 991.98px) {
        .navbar-nav { padding: 0 40px; }
        .nav-link { padding: 0.5rem 0.75rem; font-size: 0.9rem; }
      }
      
      @media (min-width: 992px) {
        .navbar-toggler {
          display: none;
        }
      }
    ")),
    # Enhanced JS for tab management, scrolling, and URL routing
    tags$script(HTML("
      // Tab locking functionality
      Shiny.addCustomMessageHandler('lockTabs', function(msg){
        (msg.ids||[]).forEach(function(id){
          var sel = 'a[data-bs-toggle=\"tab\"][data-value=\"'+id+'\"]';
          var el = document.querySelector(sel);
          if(!el) return;
          if(msg.disabled){ el.classList.add('disabled'); el.setAttribute('aria-disabled','true'); }
          else { el.classList.remove('disabled'); el.removeAttribute('aria-disabled'); }
          el.onclick = function(e){ if(el.classList.contains('disabled')){ e.preventDefault(); e.stopPropagation(); } };
        });
      });
      
      // Mobile menu collapse function using Bootstrap 5 Collapse API
      function collapseMobileMenu() {
        const nc = document.querySelector('.navbar .navbar-collapse');
        if (!nc) return;
        
        // Check if Bootstrap is available
        if (typeof bootstrap !== 'undefined' && bootstrap.Collapse) {
          // Use Bootstrap 5 Collapse API
          const inst = bootstrap.Collapse.getOrCreateInstance(nc, { toggle: false });
          inst.hide();
        } else {
          // Fallback: manually hide the collapse
          nc.classList.remove('show');
          const toggler = document.querySelector('.navbar-toggler');
          if (toggler) {
            toggler.setAttribute('aria-expanded', 'false');
          }
        }
      }
      
      // URL routing and deep linking
      function updateURL(tabValue) {
        const url = new URL(window.location);
        url.hash = tabValue;
        history.pushState(null, '', url);
      }
      
      function getTabFromURL() {
        return window.location.hash.substring(1) || 'home';
      }
      
      // Initialize URL routing
      $(document).ready(function() {
        // Handle browser back/forward
         window.addEventListener('popstate', function() {
           const tabValue = getTabFromURL();
           if (tabValue) {
             const el = document.querySelector('a[data-value=\"' + tabValue + '\"]');
             if (el && window.bootstrap && bootstrap.Tab) {
               bootstrap.Tab.getOrCreateInstance(el).show();
             }
             collapseMobileMenu(); // Collapse menu on navigation
           }
         });
        
        // Handle tab clicks - use event delegation for dynamic content
        $(document).on('click', 'a[data-bs-toggle=\"tab\"], .nav-link, .navbar-nav a', function(e) {
          const tabValue = $(this).attr('data-value') || $(this).attr('data-bs-value');
          
          // Always try to update URL if we can find a value
          if (tabValue) {
            updateURL(tabValue);
          }
        });
        
        // Close the menu on any nav selection
        $(document).on('click', '.navbar .nav-link, .navbar .dropdown-item, a[data-bs-toggle=\"tab\"]', function () {
          collapseMobileMenu();
        });
        
         // Set initial tab from URL
         const initialTab = getTabFromURL();
         if (initialTab) {
           const el = document.querySelector('a[data-value=\"' + initialTab + '\"]');
           if (el && window.bootstrap && bootstrap.Tab) {
             bootstrap.Tab.getOrCreateInstance(el).show();
           }
         }
      });
      
      // Navbar scrolling functionality
      function initNavbarScrolling() {
        const nav = document.querySelector('.navbar-nav');
        if (!nav) return;
        
        const container = nav.parentElement;
        const leftIndicator = document.createElement('div');
        const rightIndicator = document.createElement('div');
        
        leftIndicator.className = 'nav-scroll-indicator nav-scroll-left hidden';
        leftIndicator.innerHTML = '<i class=\"fas fa-chevron-left\"></i>';
        rightIndicator.className = 'nav-scroll-indicator nav-scroll-right hidden';
        rightIndicator.innerHTML = '<i class=\"fas fa-chevron-right\"></i>';
        
        container.style.position = 'relative';
        container.appendChild(leftIndicator);
        container.appendChild(rightIndicator);
        
        function updateScrollIndicators() {
          const scrollLeft = nav.scrollLeft;
          const maxScroll = nav.scrollWidth - nav.clientWidth;
          
          leftIndicator.classList.toggle('hidden', scrollLeft <= 0);
          rightIndicator.classList.toggle('hidden', scrollLeft >= maxScroll - 1);
        }
        
        leftIndicator.addEventListener('click', function() {
          nav.scrollBy({ left: -200, behavior: 'smooth' });
        });
        
        rightIndicator.addEventListener('click', function() {
          nav.scrollBy({ left: 200, behavior: 'smooth' });
        });
        
        nav.addEventListener('scroll', updateScrollIndicators);
        window.addEventListener('resize', updateScrollIndicators);
        updateScrollIndicators();
      }
      
      // Initialize scrolling when DOM is ready
      $(document).ready(initNavbarScrolling);
      
      
      // Listen for Shiny tab changes
      $(document).on('shown.bs.tab', function(e) {
        // Collapse mobile menu when tab changes
        collapseMobileMenu();
      });
      
      // Custom message handlers for state management
      Shiny.addCustomMessageHandler('updateTabState', function(data) {
        // Update URL when tab changes (handled by existing URL routing)
      });
      
      Shiny.addCustomMessageHandler('restoreTabState', function(data) {
        // Restore tab state on page load (handled by existing URL routing)
      });
      
      // Mobile menu collapse handler
      Shiny.addCustomMessageHandler('collapseMobileMenu', function(data) {
        collapseMobileMenu();
      });
    "))
  ),

   # --------------------------- HOME ------------------------------------------
  # Replace your existing HOME nav_panel section with this exact code:

   nav_panel(
     title = span(
       icon("house", class = "nav-icon", `aria-hidden` = "true"),
       " Home",
       `aria-label` = "Home page - Overview and introduction"
     ), 
     value = "home",
  
  # Add this CSS to your existing header section
  tags$style(HTML("
    .home-container {
      min-height: calc(100vh - 100px);
      padding: 2rem 0;
    }
    
    .hero-section {
      background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
      border-radius: 20px;
      box-shadow: 0 10px 40px rgba(0,0,0,0.08);
      margin-bottom: 2rem;
      overflow: hidden;
      position: relative;
    }
    
    .hero-section::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      height: 5px;
      background: linear-gradient(90deg, #8B0000 0%, #A00000 50%, #8B0000 100%);
    }
    
    .hero-content {
      padding: 3rem 2rem;
      width: 100%;
      max-width: none;
    }
    
    .logo-section {
      text-align: center;
      margin-bottom: 2.5rem;
    }
    
    .logo-container {
      display: inline-block;
      padding: 1rem;
      background: rgba(139, 0, 0, 0.03);
      border-radius: 15px;
      transition: all 0.3s ease;
    }
    
    .logo-container:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 25px rgba(139, 0, 0, 0.15);
    }
    
    .logo-image {
      max-height: 180px;
      width: auto;
    }
    
    .hero-title {
      font-size: 2.5rem;
      font-weight: 700;
      color: #2c3e50;
      margin-bottom: 0.5rem;
      line-height: 1.2;
      text-align: center;
    }
    
    .hero-subtitle {
      font-size: 1.1rem;
      color: #6c757d;
      text-align: center;
      margin-bottom: 2rem;
      font-weight: 400;
      line-height: 1.5;
    }
    
    .accent-divider {
      width: 100px;
      height: 4px;
      background: linear-gradient(90deg, #8B0000 0%, #A00000 100%);
      margin: 0 auto 2.5rem auto;
      border-radius: 2px;
    }
    
    .content-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 3rem;
      margin-bottom: 2.5rem;
      width: 100%;
      max-width: none;
    }
    
    .content-section {
      background: white;
      padding: 2rem;
      border-radius: 15px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.06);
      transition: all 0.3s ease;
      border: 1px solid rgba(139, 0, 0, 0.1);
      margin-bottom: 1.75rem;
    }
    
    .content-section:hover {
      transform: translateY(-3px);
      box-shadow: 0 8px 30px rgba(0,0,0,0.12);
      border-color: rgba(139, 0, 0, 0.2);
    }
    
    .section-icon {
      width: 60px;
      height: 60px;
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      border-radius: 15px;
      display: flex;
      align-items: center;
      justify-content: center;
      color: white;
      font-size: 1.8rem;
      margin-bottom: 1.5rem;
    }
    
    .section-title {
      font-size: 1.4rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 1rem;
    }
    
    .section-text {
      color: #495057;
      line-height: 1.7;
      font-size: 1.1rem;
    }
    
    .metrics-list {
      list-style: none;
      padding: 0;
      margin: 1rem 0;
    }
    
    .metrics-list li {
      padding: 0.7rem 0;
      position: relative;
      padding-left: 1.8rem;
      color: #495057;
      font-weight: 500;
      font-size: 1.05rem;
    }
    
    .metrics-list li::before {
      content: '▶';
      position: absolute;
      left: 0;
      color: #8B0000;
      font-size: 0.8rem;
    }
    
    .workflow-steps {
      background: white;
      padding: 2rem;
      border-radius: 15px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.06);
      margin-bottom: 2rem;
      border: 1px solid rgba(139, 0, 0, 0.1);
    }
    
    .workflow-title {
      font-size: 2.2rem;
      font-weight: 700;
      color: #2c3e50;
      margin-bottom: 2rem;
      text-align: center;
    }
    
    .steps-container {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      gap: 1.5rem;
    }
    
    .step-card {
      background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      padding: 1.5rem;
      border-radius: 12px;
      transition: all 0.3s ease;
      text-align: center;
    }
    .step-card.clickable { cursor: pointer; }
    
    .step-card:hover {
      border-color: #8B0000;
      transform: translateY(-2px);
      box-shadow: 0 6px 20px rgba(139, 0, 0, 0.15);
    }
    
    .step-number {
      width: 40px;
      height: 40px;
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      color: white;
      font-weight: 700;
      font-size: 1.2rem;
      margin: 0 auto 1rem auto;
    }
    
    .step-title {
      font-size: 1.3rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 0.7rem;
    }
    
    .step-description {
      font-size: 1.05rem;
      color: #495057;
      line-height: 1.5;
    }
    /* Ensure all text inside step-card is centered */
    .step-card .step-title,
    .step-card .step-description,
    .step-card .step-cta { text-align: center; }
    .step-link { display: block; color: inherit; text-decoration: none; }
    .step-cta { margin-top: 0.75rem; color: #8B0000; font-weight: 600; font-size: 0.95rem; }
    
    .cta-section {
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      padding: 2.5rem;
      border-radius: 15px;
      text-align: center;
      color: white;
      margin-bottom: 2rem;
      position: relative;
      overflow: hidden;
    }
    
    .cta-title {
      font-size: 2.5rem;
      font-weight: 700;
      margin-bottom: 1rem;
      position: relative;
      z-index: 1;
      color: white;
    }
    
    .cta-subtitle {
      font-size: 1.1rem;
      margin-bottom: 2rem;
      opacity: 1;
      color: #ffffff;
      position: relative;
      z-index: 1;
    }
    
    .cta-button {
      background: white;
      color: #8B0000;
      border: none;
      padding: 1rem 2.5rem;
      border-radius: 50px;
      font-size: 1.1rem;
      font-weight: 700;
      transition: all 0.3s ease;
      position: relative;
      z-index: 1;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    
    .cta-button:hover {
      background: #f8f9fa;
      color: #8B0000;
      transform: translateY(-2px);
      box-shadow: 0 8px 25px rgba(0,0,0,0.2);
    }
    
    @media (max-width: 1200px) {
      .hero-title { font-size: 2.5rem; }
      .section-title { font-size: 1.5rem; }
      .workflow-title { font-size: 2rem; }
      .logo-image { max-height: 150px; }
    }
    
    @media (max-width: 1024px) {
      .content-grid { grid-template-columns: 1fr; gap: 1.5rem; }
      .steps-container { grid-template-columns: repeat(2, 1fr); }
      .hero-title { font-size: 2.2rem; }
      .cta-title { font-size: 2.2rem; }
    }
    
    @media (max-width: 900px) {
      .steps-container { grid-template-columns: 1fr; }
      .logo-image { max-height: 130px; }
    }
    
    @media (max-width: 768px) {
      .hero-title { font-size: 2rem; }
      .cta-title { font-size: 1.8rem; }
      .logo-image { max-height: 110px; }
    }
  ")),
  
  div(class = "home-container",
    div(class = "container-fluid",
      div(class = "hero-section",
        div(class = "hero-content",
          div(class = "logo-section",
            div(class = "logo-container",
                if (file.exists("www/logo.png")) {
                tags$img(src = "logo.png", class = "logo-image", alt = "Laboratory Logo")
                } else {
                tags$div(class = "text-muted", icon("image"), " Drop your lab logo at ", tags$code("www/logo.png"))
              }
            )
          ),
          h1(class = "hero-title", "Child Social Network Questionnaire"),
          p(class = "hero-subtitle", "Mapping early social experiences to understand development"),
          div(class = "accent-divider"),
          div(class = "content-grid",
            div(class = "content-section",
              div(class = "section-icon", icon("chart-line")),
              h3(class = "section-title", "What CSNQ Measures"),
              tags$ul(class = "metrics-list",
                tags$li("Network size – how many individuals a child regularly interacts with"),
                tags$li("Network composition – family member, caregivers, school teacher, peers, adults; diversity in language and race"),
                tags$li("Network structure – network density, racial and linguistic entropy, racial and linguistic EI Index")
              )
            ),
            div(class = "content-section",
              div(class = "section-icon", icon("lightbulb")),
              h3(class = "section-title", "Why It Matters"),
              p(class = "section-text",
                "Early social experience relates to neural, cognitive, and social development. Sensitivity to categories like race and language emerges early, and exposure to diversity can shape social cognition. Mapping children's social networks helps test how experience with diversity relates to outcomes like perspective-taking, bias, and emotion regulation."
              )
            )
          )
        )
      ),
      # Development Team moved up and full-width
      div(class = "content-section",
        div(class = "section-icon", icon("university")),
        h3(class = "section-title", "Development Team"),
        p(class = "section-text",
          HTML("The <strong>Child Social Network Questionnaire (CSNQ)</strong> was developed in the <strong>Infant Learning and Development Laboratory (Woodward Lab)</strong> at the University of Chicago (<a href='https://woodwardlab.uchicago.edu/' target='_blank'>woodwardlab.uchicago.edu</a>). The initial CSNQ was created by <strong>Nicole Burke and colleagues</strong> (Burke, Brezack, & Woodward, 2022). Since then, <strong>Yiyi Wang, Bethany Ou</strong>, and collaborators have updated the survey and built this comprehensive research hub."))
      ),
      div(class = "workflow-steps",
        h2(class = "workflow-title", "Research Workflow"),
        div(class = "steps-container",
          div(class = "step-card clickable",
            actionLink(
              "wf_access",
              label = tagList(
                div(class = "step-number", "1"),
                h4(class = "step-title", "Access Survey"),
                p(class = "step-description", "Download the canonical CSNQ file from this hub"),
                div(class = "step-cta", "Open Access Survey page →")
              ),
              class = "step-link"
            )
          ),
          div(class = "step-card",
            div(class = "step-number", "2"),
            h4(class = "step-title", "Collect Data"),
            p(class = "step-description", "Deploy in Qualtrics to gather data from parents/caregivers")
          ),
          div(class = "step-card clickable",
            actionLink(
              "wf_process",
              label = tagList(
                div(class = "step-number", "3"),
                h4(class = "step-title", "Process Data"),
                p(class = "step-description", "Clean Qualtrics exports into three standardized tables"),
                div(class = "step-cta", "Open Process Data page →")
              ),
              class = "step-link"
            )
          ),
          div(class = "step-card clickable",
            actionLink(
              inputId = "go_plotting",
              label = tagList(
                div(class = "step-number", "4"),
                h4(class = "step-title", "Analyze Results"),
                p(class = "step-description", "Explore data with network plotting tools"),
                div(class = "step-cta", "Open Network Plotting →")
              ),
              class = "step-link"
            )
          )
        )
      ),
      div(class = "cta-section",
        h2(class = "cta-title", "Ready to Get Started?"),
        p(class = "cta-subtitle", "Join us in using the CSNQ to understand children's social development"),
        actionButton("btn_interested", "Start Requesting Access", class = "cta-button", icon = icon("rocket"))
      ),
      
      tags$div(class = "content-section",
        h3(class = "section-title", "Key References"),
        tags$ul(style = "list-style: none; padding: 0;",
          tags$li(style = "padding: 0.7rem 0; color: #495057; border-bottom: 1px solid #f1f3f4; font-size: 1.05rem;",
            tags$a(
              href = "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2022.1009422/full",
              target = "_blank",
              rel = "noopener noreferrer",
              style = "text-decoration: none; color: inherit;",
              "Burke, N. M., Brezack, N., & Woodward, A. L. (2022). Frontiers in Psychology."
            )
          ),
          tags$li(style = "padding: 0.7rem 0; color: #495057; font-size: 1.05rem;",
            tags$a(
              href = "https://www.frontiersin.org/journals/developmental-psychology/articles/10.3389/fdpys.2023.1221056/full",
              target = "_blank",
              rel = "noopener noreferrer",
              style = "text-decoration: none; color: inherit;",
              "Burke, N. M., Brezack, N., & Meyer, M. (2023). Frontiers in Developmental Psychology."
            )
          )
          )
        )
      )
    )
  ),

   # --------------------------- GET ACCESS ------------------------------------
# Replace your GET ACCESS nav_panel section with this:

   nav_panel(
     title = span(
       icon("key", class = "nav-icon", `aria-hidden` = "true"),
       " Get Access",
       `aria-label` = "Get Access - Request survey access"
     ), 
     value = "access",
  
  # Add improved styling
  tags$style(HTML("
    .access-page {
      background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      min-height: calc(100vh - 100px);
      padding: 2rem 0;
    }
    
    .access-hero {
      background: transparent;
      padding: 2rem 2rem 1rem 2rem;
      margin-bottom: 1rem;
      text-align: center;
      position: relative;
    }
    
    .logo-section {
      text-align: center;
      margin-bottom: 2rem;
    }
    
    .logo-container {
      display: inline-block;
      padding: 1rem;
      background: rgba(139, 0, 0, 0.03);
      border-radius: 15px;
      transition: all 0.3s ease;
    }
    
    .logo-container:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 25px rgba(139, 0, 0, 0.15);
    }
    
    .logo-image {
      max-height: 180px;
      width: auto;
    }
    
     .access-title {
       font-size: 2.2rem;
       font-weight: 700;
       color: #2c3e50;
       margin-bottom: 1rem;
       line-height: 1.2;
     }
    
    .access-subtitle {
      font-size: 1.1rem;
      color: #6c757d;
      margin-bottom: 2rem;
      line-height: 1.4;
    }
    
    .form-container {
      background: transparent;
      padding: 1.5rem 2.5rem 2.5rem 2.5rem;
      position: relative;
      margin-top: 0;
    }
    
    .form-fields {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 2rem;
    }
    
    .form-group.full-width {
      grid-column: 1 / -1;
    }
    
    
    .form-header {
      text-align: center;
      margin-bottom: 1.5rem;
      padding-top: 0;
    }
    
     .form-title {
       font-size: 1.6rem;
       font-weight: 600;
       color: #2c3e50;
       margin-bottom: 1rem;
     }
    
    .form-description {
      font-size: 1.1rem;
      color: #6c757d;
      line-height: 1.6;
    }
    
    .form-group {
      margin-bottom: 2rem;
    }
    
    .form-label {
      font-size: 1.1rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 0.7rem;
      display: block;
    }
    
    .form-control {
      border: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 0.75rem 1rem;
      font-size: 1rem;
      transition: all 0.3s ease;
      background: white;
    }
    
    .form-control:focus {
      border-color: #8B0000;
      box-shadow: 0 0 0 0.2rem rgba(139, 0, 0, 0.15);
      background: white;
    }
    
    .form-text {
      font-size: 0.95rem;
      color: #6c757d;
      margin-top: 0.5rem;
      line-height: 1.5;
    }
    
    .form-check {
      background: transparent;
      border: none;
      padding: 1rem 0;
      transition: all 0.3s ease;
    }
    
    .form-check:hover {
      background: transparent;
    }
    
    .form-check-input {
      width: 1.2rem;
      height: 1.2rem;
      border: 2px solid #8B0000;
      border-radius: 4px;
      background-color: white;
      margin-right: 0.75rem;
      cursor: pointer;
      transition: all 0.3s ease;
      position: relative;
    }
    
    .form-check-input:hover {
      border-color: #A00000;
      box-shadow: 0 0 0 0.2rem rgba(139, 0, 0, 0.15);
    }
    
    .form-check-input:checked {
      background-color: #8B0000;
      border-color: #8B0000;
    }
    
    .form-check-input:checked::after {
      content: '✓';
      color: white;
      font-weight: bold;
      font-size: 0.9rem;
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    }
    
    .form-check-input:focus {
      border-color: #8B0000;
      box-shadow: 0 0 0 0.2rem rgba(139, 0, 0, 0.25);
    }
    
    .form-check-label {
      font-size: 1.05rem;
      color: #495057;
      line-height: 1.5;
      cursor: pointer;
      display: flex;
      align-items: flex-start;
    }
    
    .btn-primary {
      background: linear-gradient(135deg, #8B0000 0%, #A00000 100%);
      border: none;
      border-radius: 12px;
      padding: 1.2rem 2rem;
      font-size: 1.2rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      transition: all 0.3s ease;
      box-shadow: 0 4px 15px rgba(139, 0, 0, 0.2);
    }
    
    .btn-primary:hover {
      background: linear-gradient(135deg, #A00000 0%, #8B0000 100%);
      transform: translateY(-2px);
      box-shadow: 0 8px 25px rgba(139, 0, 0, 0.3);
    }
    
    .privacy-note {
      background: transparent;
      padding: 1.5rem 0;
      margin-top: 2rem;
      text-align: center;
    }
    
    .text-danger {
      color: #dc3545;
      font-weight: 600;
    }
    
    .text-success {
      color: #198754;
    }
    
    .alert {
      border-radius: 12px;
      padding: 1rem 1.5rem;
      margin-top: 1rem;
      border: none;
      font-size: 1.05rem;
    }
    
    .alert-success {
      background: linear-gradient(135deg, #d1e7dd 0%, #a3d9a3 100%);
      color: #0f5132;
      border-left: 4px solid #198754;
    }
    
    .alert-danger {
      background: linear-gradient(135deg, #f8d7da 0%, #f5c2c7 100%);
      color: #721c24;
      border-left: 4px solid #dc3545;
    }
    
    /* Responsive design for laptops */
    @media (max-width: 1200px) {
      .access-title { font-size: 2.5rem; }
      .form-title { font-size: 2rem; }
      .form-container { padding: 2.5rem; }
    }
    
    @media (max-width: 1024px) {
      .access-title { font-size: 2.2rem; }
      .access-subtitle { font-size: 1.2rem; }
      .form-container { padding: 2rem; }
      .form-fields { grid-template-columns: 1fr; gap: 1.5rem; }
    }
    
    @media (max-width: 768px) {
      .access-page { padding: 1.5rem 0; }
      .access-hero { padding: 2rem 1.5rem; }
      .access-title { font-size: 2rem; }
      .form-container { padding: 1.5rem; }
      .form-fields { grid-template-columns: 1fr; gap: 1rem; }
    }
  ")),
  
  div(class = "access-page",
    div(class = "container-fluid",
      div(class = "row",
        div(class = "col-lg-10 col-xl-8 mx-auto",
           # Hero section
          div(class = "access-hero",
            div(class = "page-header",
              tags$h1(class = "page-title", "Request Access"),
              div(class = "title-underline")
            )
          ),
          
          # Main form container
          div(class = "form-container",
               # Form header
            div(class = "form-header",
              tags$h2(class = "form-title", "Researcher Information"),
              tags$p(class = "form-description", 
                        "When the required fields are valid, you will be directed to the Access Survey page.")
               ),
               
               # Form fields
               div(class = "form-fields",
                 # Institution field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "institution",
                     "Institution", tags$span(class = "text-danger", " *")
                   ),
                 textInput("institution", "", placeholder = "e.g., University of Chicago"),
                div(class = "form-text",
                     icon("info-circle", class = "me-1"),
                     "Official organization name. If none, enter \"Independent researcher.\""
                   ),
                   uiOutput("institution_error")
                 ),
                 
                 # Lab field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "lab",
                     "Lab", tags$span(class = "text-danger", " *")
                   ),
                 textInput("lab", "", placeholder = "e.g., Woodward Lab"),
                div(class = "form-text",
                     icon("users", class = "me-1"),
                     "Your lab or research group. If none, enter \"Independent researcher.\""
                   ),
                   uiOutput("lab_error")
                 ),
                 
                 # Contact Email field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "contact_email",
                     "Contact Email", tags$span(class = "text-danger", " *")
                   ),
                 textInput("contact_email", "", placeholder = "researcher@university.edu"),
                div(class = "form-text",
                     icon("envelope", class = "me-1"),
                     "We send access and update notices here. Institutional email is preferred."
                   ),
                   uiOutput("email_error")
                 ),
                 
                 # Phone Number field
              div(class = "form-group",
                tags$label(class = "form-label", `for` = "phone_number",
                     "Phone Number", tags$span(class = "text-muted", " (optional)")
                   ),
                 textInput("phone_number", "", placeholder = "+1-555-123-4567"),
                div(class = "form-text",
                     icon("phone", class = "me-1"),
                     "Used only for urgent troubleshooting. Leave blank if you prefer email only."
                   ),
                   uiOutput("phone_error")
                 ),
                 
                 # Purpose field
              div(class = "form-group full-width",
                tags$label(class = "form-label", `for` = "purpose",
                     "Purpose", tags$span(class = "text-muted", " (optional)")
                   ),
                 textAreaInput("purpose", "", rows = 3, 
                               placeholder = "One to two sentences on how you plan to use the survey or pipeline."), 
                div(class = "form-text",
                     icon("lightbulb", class = "me-1"),
                     "One to two sentences on how you plan to use the survey or pipeline."
                   ),
                   uiOutput("purpose_error")
                 ),
                 
                 # Consent checkbox
              div(class = "form-group full-width",
                div(class = "form-check",
                     tags$input(type = "checkbox", class = "form-check-input", id = "consent_checkbox"),
                  tags$label(class = "form-check-label", `for` = "consent_checkbox",
                       icon("shield", class = "me-2 text-success"),
                       "I understand my information is used only for access control and support; no marketing and no sharing outside the team."
                     )
                   ),
                   uiOutput("consent_error")
                 ),
                 
                 # Submit button
              div(class = "form-group full-width",
                     actionButton("submit_intake", 
                           tagList(icon("paper-plane"), " Submit and Continue"), 
                           class = "btn btn-primary w-100",
                                 title = "Submits the form and, if valid, directs you to the Access Survey page.")
                   ),
                   uiOutput("submit_status")
                 ),
                 
                 # Privacy note
            div(class = "privacy-note",
                   div(class = "small text-muted",
                     icon("lock", class = "me-1"),
                     "We log submissions for audit and support. Entries are retained for up to two years and then deleted."
                   ),
                   div(class = "mt-2",
                     tags$a(href = "#", "Why we ask for affiliation and purpose", 
                           class = "btn btn-link btn-sm text-decoration-none")
                 )
               )
             )
           )
         )
       )
     )
   ),

   # --------------------------- ACCESS SURVEY -----------------------------------
   nav_panel(
     title = span(
       icon("download", class = "nav-icon", `aria-hidden` = "true"),
       " Access Survey",
       `aria-label` = "Access Survey - Download survey files"
     ), 
     value = "download",
     accessSurveyUI("access_survey")
  ),

   # --------------------------- QUALTRICS HELP DOC ------------------------------
  nav_panel(
    title = span(
       icon("book", class = "nav-icon", `aria-hidden` = "true"),
       " Qualtrics Help Doc",
       `aria-label` = "Qualtrics Help Doc - Survey logic and flow walkthrough"
     ), 
     value = "qualtrics_help",
     div(class = "container-fluid",
       div(class = "main-content",
         h1(class = "page-title", "Qualtrics Help Doc"),
         div(class = "title-underline"),
         tags$iframe(
           src = "qualtrics_help/ChildSocialNetworkQuestionnaireQualtricsSurve.html",
           width = "100%",
           height = "800px",
           style = "border: 1px solid #dee2e6; border-radius: 8px; margin-top: 1rem;",
           title = "Qualtrics Survey Help Documentation"
             )
           )
         )
       ),
       

   # --------------------------- EXPLORE DATA (AFTER CLEANING) ------------------
  # Explore Data page removed per request

   # --------------------------- PROCESS DATA ------------------------------------
  nav_panel(
    title = span(
      icon("cogs", class = "nav-icon", `aria-hidden` = "true"),
      " Process Data",
      `aria-label` = "Process Data - Upload and process Qualtrics data"
    ), 
    value = "processdata",
    
    # ==== Scoped CSS (only affects this page) ====
    tags$style(HTML("
      .process-hero {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 12px;
        padding: 2rem;
        margin-bottom: 2rem;
        border: 1px solid #dee2e6;
      }
      
      .process-hero h2 {
        color: #8B0000;
        font-weight: 700;
        margin-bottom: 0.5rem;
      }
      
      .process-hero p {
        color: #6c757d;
        font-size: 1.1rem;
        margin-bottom: 0;
      }
      
      .step-box {
        background: white;
        border: 2px solid #e9ecef;
        border-radius: 12px;
        padding: 1.5rem;
        margin-bottom: 1.5rem;
        transition: all 0.3s ease;
      }
      
      .step-box:hover {
        border-color: #8B0000;
        box-shadow: 0 4px 12px rgba(139, 0, 0, 0.1);
      }
      
      .step-box h4 {
        color: #8B0000;
        font-weight: 600;
        margin-bottom: 1rem;
      }
      
      .status-text {
        font-weight: 600;
        padding: 0.5rem 1rem;
        border-radius: 6px;
        margin-top: 0.5rem;
        display: inline-block;
      }
      
      .status-success {
        background: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      
      .status-error {
        background: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
      
      .status-info {
        background: #d1ecf1;
        color: #0c5460;
        border: 1px solid #bee5eb;
      }
      
      .preview-table {
        margin-top: 1rem;
      }
      
      .download-section {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 1rem;
        margin-top: 1rem;
      }
      
      .btn-process {
        background: #8B0000;
        border-color: #8B0000;
        color: white;
        font-weight: 600;
        padding: 0.75rem 2rem;
        border-radius: 8px;
        transition: all 0.3s ease;
      }
      
      .btn-process:hover {
        background: #A00000;
        border-color: #A00000;
        transform: translateY(-1px);
      }
      
      .btn-process:disabled {
        background: #6c757d;
        border-color: #6c757d;
        transform: none;
      }
      
      .dm h3{margin-top:1.1rem;font-weight:800}
      .dm h4{margin-top:.9rem;font-weight:700}
      .dm code{background:#f6f8fa;padding:.08rem .3rem;border-radius:4px}
      .callout{background:#eef7ff;border-left:4px solid #0a7ac4;padding:.75rem 1rem;border-radius:8px}
      .mono{font-family:ui-monospace, Menlo, Consolas, monospace}
    ")),
    
    # ==== HERO Section ====
    div(class = "process-hero",
      h2("Social Network Questionnaire Data Processing"),
      p("Upload your Qualtrics data to generate analysis-ready datasets using the SNQ pipeline")
    ),
    
    # ==== File Upload Section ====
    div(class = "step-box",
      h4(icon("upload"), " Upload Qualtrics Data"),
      fileInput("process_file", "Choose CSV/Excel File from Qualtrics",
               accept = c(".csv", ".xlsx", ".xls"), width = "100%"),
      helpText("Upload your Qualtrics export file (CSV or Excel format)"),
      br(),
      actionButton("process_btn", "Process Data", 
                  class = "btn btn-process btn-lg", disabled = TRUE),
      br(), br(),
      conditionalPanel(
        condition = "output.show_download",
        downloadButton("download_all", "Download All Results (ZIP)", 
                      class = "btn btn-success btn-lg")
      )
    ),
    
    # ==== Pipeline Script Download ====
    div(class = "step-box",
      h4(icon("file-code"), " SNQ Pipeline Script (.Rmd)"),
      p("Download the full processing script used to generate the datasets."),
      downloadButton("download_pipeline_rmd", "Download snq_pipeline.rmd", class = "btn btn-secondary")
    ),
    
    # ==== Download Individual Files Section ====
    conditionalPanel(
      condition = "output.show_individual_downloads",
      div(class = "row",
        div(class = "col-md-4",
          div(class = "step-box",
            h4(icon("users"), " Node Level Data"),
            p("Alter-level data: one row per relationship (alter)"),
            downloadButton("download_node", "Download Node Data", class = "btn btn-info btn-block")
          )
        ),
        div(class = "col-md-4",
          div(class = "step-box",
            h4(icon("user"), " Ego Level Summary"),
            p("Child-level data: one row per child (ego)"),
            downloadButton("download_ego", "Download Ego Data", class = "btn btn-info btn-block")
          )
        ),
        div(class = "col-md-4",
          div(class = "step-box",
            h4(icon("running"), " Activity Level Data"),
            p("Activity-level data: one row per activity (per child)"),
            downloadButton("download_activity", "Download Activity Data", class = "btn btn-info btn-block")
        )
      )
    ),
      br(),
    div(class = "step-box",
        h4(icon("table"), " Preview: Ego Level Summary"),
        DT::dataTableOutput("preview_ego")
    ),
    div(class = "step-box",
        h4(icon("table"), " Preview: Node Level Long"),
        DT::dataTableOutput("preview_node")
      ),
      div(class = "step-box",
        h4(icon("table"), " Preview: Activity Level Long"),
        DT::dataTableOutput("preview_activity")
      )
    ),
    
    # Data Model Overview removed from Process page per request
    
    # ==== Field Dictionary (embedded) ====
        div(class = "step-box",
      h4(icon("table"), " Field Dictionary"),
      tags$style(HTML('
        .dd-wrapper{display:flex;gap:1.5rem}
        .dd-toc{position:sticky;top:80px;max-height:calc(100vh - 120px);overflow:auto;min-width:260px;width:260px;background:#fff;border:1px solid #e9ecef;border-radius:8px;padding:12px}
        .dd-toc h5{margin:.25rem 0 .5rem 0;font-weight:700;color:#495057}
        .dd-toc ul{list-style:none;margin:0;padding-left:0}
        .dd-toc li{margin:.15rem 0}
        .dd-toc a{color:#0d6efd;text-decoration:none}
        .dd-toc a:hover{text-decoration:underline}
        .dd-toc li.toc-h1 a{font-weight:900;color:#198754;font-size:1.1rem}
        .dd-toc li.toc-h2 a{font-weight:800;color:#0d6efd;font-size:1.0rem}
        .dd-toc li.toc-h3{margin-left:1rem}
        .dd-content{flex:1}
      ')),
      div(class = "dd-wrapper",
        div(class = "dd-toc",
          tags$h5("Contents"),
          tags$ul(id = "dd-toc-list")
        ),
        div(class = "dd-content", id = "dd-content",
          htmltools::includeHTML("www/DataDictionary.html")
        )
      ),
      tags$script(HTML("\n        (function(){\n          function slugify(text){return (text||'').toLowerCase().replace(/[^a-z0-9\\s-]/g, '').trim().replace(/\\s+/g, '-');}\n          var cont = document.getElementById('dd-content'); if(!cont) return;\n          var tocList = document.getElementById('dd-toc-list'); if(!tocList) return;\n          var heads = cont.querySelectorAll('h1, h2, h3');\n          heads.forEach(function(h){\n            var id = h.id || slugify(h.textContent||h.innerText||'section');\n            if(!h.id) h.id = id;\n            var li = document.createElement('li');\n            if(h.tagName.toLowerCase()==='h1') li.className = 'toc-h1';\n            if(h.tagName.toLowerCase()==='h2') li.className = 'toc-h2';\n            if(h.tagName.toLowerCase()==='h3') li.className = 'toc-h3';\n            var a = document.createElement('a'); a.href = '#'+id; a.textContent = h.textContent||h.innerText||id;\n            li.appendChild(a); tocList.appendChild(li);\n          });\n        })();\n      "))
    )
  ),

  # --------------------------- NETWORK PLOTTING --------------------------------
  nav_panel(
    title = span(
      icon("project-diagram", class = "nav-icon", `aria-hidden` = "true"),
      " Network Plotting",
      `aria-label` = "Network Plotting - Visualize cleaned data"
    ),
    value = "plotting",
    
    # Embed the external SNQ Network Plotting Dashboard HTML as-is
    tags$iframe(
      src = "snq_dash/dashboard.html",
      style = "width: 100%; height: calc(100vh - 140px); border: none;",
      `aria-label` = "Embedded SNQ Network Plotting Dashboard"
    )
  ),

   # --------------------------- ADMIN -------------------------------------------
   nav_panel(
     title = span(
       icon("shield", class = "nav-icon", `aria-hidden` = "true"),
       " Admin",
       `aria-label` = "Admin - Administrative dashboard and data management"
     ), 
     value = "admin",
     div(class = "page-header",
       h1(class = "page-title", "Admin Dashboard"),
       div(class = "title-underline")
     ),
     conditionalPanel(
       condition = "!output.admin_authenticated",
       div(class = "container-fluid py-4",
         div(class = "row justify-content-center",
           div(class = "col-lg-6 col-xl-4",
             card(
               card_header("Admin Login"),
               passwordInput("admin_password", "Admin Password", placeholder = "Enter admin password"),
               actionButton("admin_login", "Login", class = "btn btn-warning"),
               uiOutput("admin_status")
             )
           )
         )
       )
     ),
     conditionalPanel(
       condition = "output.admin_authenticated",
       div(class = "container-fluid py-4",
         # Data Management Tabs
         tabsetPanel(
           id = "admin_tabs",
           
           # Intake Data Tab
           tabPanel("Intake Data",
         card(
           card_header("Intake Data Management"),
           p("View and download intake submissions in real-time."),
           div(class = "d-grid gap-2 d-md-block mb-3",
             actionButton("refresh_data", "Refresh Data", class = "btn btn-primary me-2"),
                 downloadButton("download_intake", "Download Intake CSV", class = "btn btn-success me-2"),
             actionButton("admin_logout", "Logout", class = "btn btn-outline-secondary")
           ),
           uiOutput("data_stats"),
           div(class = "mt-3",
             tags$h5("Intake Data Preview"),
             DTOutput("admin_intake_table")
               )
             )
           ),
           
           # Help Requests Tab
           tabPanel("Help Requests",
             card(
               card_header("Help & Support Requests"),
               p("Manage user support requests and uploaded files."),
               div(class = "d-grid gap-2 d-md-block mb-3",
                 actionButton("refresh_help", "Refresh Help Requests", class = "btn btn-primary me-2"),
                 downloadButton("download_help_requests", "Download Help CSV", class = "btn btn-success me-2")
               ),
               div(class = "alert alert-info",
                 tags$strong("File Storage: "), "Uploaded files are stored in ", 
                 tags$code("data/uploads/"), " with timestamped filenames."
               ),
               div(class = "mt-3",
                 tags$h5("Help Requests Preview"),
                 DTOutput("admin_help_table")
               )
             )
           ),
           
           # System Debug Tab  
           tabPanel("System Debug",
             card(
               card_header("Debug Information"),
               p("System status and debugging information."),
               div(class = "row",
                 div(class = "col-md-6",
                   tags$h6("System Status"),
                   uiOutput("debug_status")
                 ),
                 div(class = "col-md-6",
                   tags$h6("Recent Logs"),
                   uiOutput("debug_logs")
                 )
               ),
               div(class = "mt-3",
                 actionButton("clear_logs", "Clear Logs", class = "btn btn-warning btn-sm"),
                 actionButton("export_logs", "Export Logs", class = "btn btn-info btn-sm ms-2")
               )
             )
           )
         )
       )
     )
   ),

   # --------------------------- HELP & CONTACT ---------------------------------
   nav_panel(
     title = span(
       icon("question-circle", class = "nav-icon", `aria-hidden` = "true"),
       " Help & Contact",
       `aria-label` = "Help & Contact - Support and assistance"
     ), 
     value = "help",
     
     # Page Header with styling matching Access Survey
     div(class = "page-header",
       h1(class = "page-title", "Help & Contact"),
       div(class = "title-underline")
     ),
     
     # Main Content Container
     div(class = "container-fluid",
       div(class = "row justify-content-center",
         div(class = "col-lg-8 col-xl-6",
           # Contact Form Card
      card(
             class = "access-form-card shadow-lg border-0",
             card_body(
               class = "p-4",
               
               # Form header
               div(class = "form-header mb-4",
                 tags$h4(class = "form-title mb-2", "Get Support"),
                 tags$p(class = "form-description text-muted", 
                        "Having trouble with your survey or analysis? We're here to help. Include files to help us diagnose the issue.")
               ),
               
               # Form fields
               div(class = "form-fields",
                 # Name field
                 div(class = "form-group mb-3",
                   tags$label(class = "form-label fw-semibold", `for` = "c_name",
                     "Your Name", tags$span(class = "text-danger", " *")
                   ),
                   textInput("c_name", "", placeholder = "Enter your full name"),
                   uiOutput("c_name_error")
                 ),
                 
                 # Email field
                 div(class = "form-group mb-3",
                   tags$label(class = "form-label fw-semibold", `for` = "c_email",
                     "Email Address", tags$span(class = "text-danger", " *")
                   ),
                   textInput("c_email", "", placeholder = "your.email@institution.edu"),
                   div(class = "form-text text-muted",
                     icon("info-circle", class = "me-1"),
                     "We'll respond to this email address within 24-48 hours."
                   ),
                   uiOutput("c_email_error")
                 ),
                 
                 # Subject field
                 div(class = "form-group mb-3",
                   tags$label(class = "form-label fw-semibold", `for` = "c_subject",
                     "Subject", tags$span(class = "text-danger", " *")
                   ),
                   textInput("c_subject", "", 
                            placeholder = "Brief description of your issue"),
                   uiOutput("c_subject_error")
                 ),
                 
                 # Message field
                 div(class = "form-group mb-4",
                   tags$label(class = "form-label fw-semibold", `for` = "c_body",
                     "Message", tags$span(class = "text-danger", " *")
                   ),
                   textAreaInput("c_body", "", rows = 4,
                                placeholder = "Describe your issue in detail. What were you trying to do? What went wrong? Include any error messages."),
                   div(class = "form-text text-muted",
                     icon("lightbulb", class = "me-1"),
                     "Be specific about steps you took and what you expected vs. what happened."
                   ),
                   uiOutput("c_body_error")
                 ),
                 
                # File Upload Section removed; provide email instruction for attachments
                div(class = "form-group mb-4",
                  div(class = "alert alert-info",
                    icon("envelope"), " For attachments, please email us directly at ",
                    tags$strong("woodwardlab@uchicago.edu"), 
                    ". Include QSF/CSV and screenshots if relevant."
                  )
                ),
                 
                 # Submit button
                 div(class = "form-group mb-4",
                   div(class = "d-grid",
                     actionButton("submit_issue", 
                                 tagList(icon("paper-plane"), " Submit Support Request"), 
                                 class = "btn btn-primary btn-lg py-3")
                   ),
                   uiOutput("issue_status")
                 ),
                 
                 # Alternative actions
                 div(class = "text-center mt-3",
                   actionButton("compose_email", 
                               tagList(icon("envelope"), " Or send direct email"), 
                               class = "btn btn-outline-primary btn-sm")
                 )
               )
             )
           )
         ),
         
         # Troubleshooting Info Sidebar
         div(class = "col-lg-4 mt-4 mt-lg-0",
           div(class = "main-content",
             h3(class = "section-title", "Quick Troubleshooting"),
             
             div(class = "instruction-step",
               h4(class = "step-title", style = "margin-bottom: 1rem;", "Common Issues"),
               div(class = "step-content", style = "margin-left: 0;",
                 tags$ul(class = "feature-list",
                   tags$li("Survey logic not working → Check QuestionIDs match canonical version"),
                   tags$li("Missing data in export → Verify all required fields are mapped"),
                   tags$li("Processing errors → Ensure CSV format matches Qualtrics export")
                 )
               )
             ),
             
             div(class = "instruction-step",
               h4(class = "step-title", style = "margin-bottom: 1rem;", "Before Contacting Us"),
               div(class = "step-content", style = "margin-left: 0;",
                 tags$ol(class = "feature-list",
                   tags$li("Try re-importing the canonical QSF"),
                   tags$li("Check our documentation for similar issues")
                 )
               )
             ),
             
             div(class = "instruction-step",
               h4(class = "step-title", style = "margin-bottom: 1rem;", "Response Time"),
               div(class = "step-content", style = "margin-left: 0;",
                 tags$p("We typically respond within 24-48 hours. Complex technical issues may take longer to resolve.")
               )
             )
           )
         )
       )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Enable bookmarking for state preservation
  enableBookmarking(store = "url")
  
  # Exclude admin password from bookmarking for security
  setBookmarkExclude(c("admin_password"))
  
  # Log session start
  log_message("New session started", "INFO")
  
  # Cleanup function for session end
  session$onSessionEnded(function() {
    # Clean up any temporary files or resources
    # This helps prevent memory leaks
    log_message("Session ended", "INFO")
    gc() # Force garbage collection
  })

  # ---------------- Tab state management ----------------
  # Track current tab for URL updates
  observe({
    if (!is.null(input$nav)) {
      session$sendCustomMessage("updateTabState", list(tab = input$nav))
    }
  })
  
  # Restore tab state from URL on session start
  observe({
    session$sendCustomMessage("restoreTabState", list())
  })
  
  # ---------------- Mobile menu collapse ----------------
  # Collapse mobile menu when tab changes
  observe({
    if (!is.null(input$nav)) {
      # Send message to collapse mobile menu
      session$sendCustomMessage("collapseMobileMenu", list())
    }
  })

  # ---------------- Gate state ----------------
  interested <- reactiveVal(FALSE)
  intake_ok  <- reactiveVal(FALSE)
  gate_override <- reactiveVal(FALSE)  # allow one-time navigation to Get Access

   # lock/unlock tabs util
   lock_tabs <- function(disabled) {
    session$sendCustomMessage("lockTabs",
      list(ids = c("download","qualtrics_help","processdata","plotting"), disabled = isTRUE(disabled)))
   }
  lock_tabs(TRUE)  # start locked

  # Home → Interested → navigate to access
  observeEvent(input$btn_interested, ignoreInit = TRUE, {
    interested(TRUE)
    bslib::nav_select("nav", "access")
    shinyjs::runjs("window.scrollTo({top: 0, left: 0, behavior: 'instant'});")
  })

  # Research Workflow quick links
  observeEvent(input$wf_access, ignoreInit = TRUE, {
    bslib::nav_select("nav", "download")
    shinyjs::runjs("window.scrollTo({top: 0, left: 0, behavior: 'instant'});")
  })
  observeEvent(input$wf_process, ignoreInit = TRUE, {
    bslib::nav_select("nav", "processdata")
    shinyjs::runjs("window.scrollTo({top: 0, left: 0, behavior: 'instant'});")
  })

  # Clear all error messages
  clear_errors <- function() {
    output$institution_error <- renderUI(NULL)
    output$lab_error <- renderUI(NULL)
    output$email_error <- renderUI(NULL)
    output$phone_error <- renderUI(NULL)
    output$purpose_error <- renderUI(NULL)
    output$consent_error <- renderUI(NULL)
    output$submit_status <- renderUI(NULL)
  }
  
  # Intake submit
  observeEvent(input$submit_intake, {
    log_message("Intake form submission started")
    clear_errors()
    
    # Show submitting state
    output$submit_status <- renderUI(
      tags$div(class="text-info", icon("spinner", class="fa-spin"), " Submitting… you will be redirected to Access Survey.")
    )
    
    tryCatch({
    # Validate required fields with specific messages
    has_errors <- FALSE
    
    if (!nzchar(trimws(input$institution))) {
      output$institution_error <- renderUI(
        tags$div(class="text-danger small", "Enter your institution or write \"Independent researcher.\"")
      )
      has_errors <- TRUE
    }
    
    if (!nzchar(trimws(input$lab))) {
      output$lab_error <- renderUI(
        tags$div(class="text-danger small", "Enter your lab or write \"Independent researcher.\"")
      )
      has_errors <- TRUE
    }
    
    if (!nzchar(trimws(input$contact_email))) {
      output$email_error <- renderUI(
        tags$div(class="text-danger small", "Enter a valid email address, for example name@university.edu")
      )
      has_errors <- TRUE
    } else {
      # Email validation
      email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
      if (!grepl(email_pattern, input$contact_email)) {
        output$email_error <- renderUI(
          tags$div(class="text-danger small", "Enter a valid email address, for example name@university.edu")
        )
        has_errors <- TRUE
      }
    }
    
    if (!isTRUE(input$consent_checkbox)) {
      output$consent_error <- renderUI(
        tags$div(class="text-danger small", "Please confirm the data-use notice.")
      )
      has_errors <- TRUE
    }
    
    if (has_errors) {
      output$submit_status <- renderUI(NULL)
      return()
    }
    
      # Prepare data with input sanitization
    df <- data.frame(
      timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE),
      type = "intake",
        institution = trimws(gsub("[<>\"'&]", "", input$institution)), # Basic XSS protection
        lab = trimws(gsub("[<>\"'&]", "", input$lab)),
        email = trimws(tolower(input$contact_email)), # Normalize email
        phone_number = ifelse(nzchar(trimws(input$phone_number)), 
                             gsub("[^0-9+() -]", "", trimws(input$phone_number)), ""), # Clean phone; keep digits, +, parentheses, space, hyphen
        purpose = ifelse(nzchar(trimws(input$purpose)), 
                        trimws(gsub("[<>\"'&]", "", input$purpose)), ""),
      stringsAsFactors = FALSE
    )
    
      # Save locally with error handling
    path <- file.path("data", "intake.csv")
      safe_file_operation(function() {
    if (!file.exists(path)) {
      write_csv(df, path, append = FALSE)
          log_message("Created new intake.csv file")
    } else {
      # Read existing data and append new row
      existing_data <- read_csv(path, show_col_types = FALSE)
      combined_data <- rbind(existing_data, df)
      write_csv(combined_data, path, append = FALSE)
          log_message("Appended to existing intake.csv file")
    }
      }, "Failed to save intake data")
    
    # Show success and redirect
    output$submit_status <- renderUI(
      tagList(
        tags$div(class="alert alert-success", 
          icon("check-circle"), 
          tags$strong("Thank you. Your access is approved. Redirecting to Access Survey.")
        ),
        tags$div(class="mt-2",
          actionLink("fallback_redirect", "If you are not redirected, select Go to Access Survey.", 
                    class = "btn btn-outline-primary btn-sm")
        )
      )
    )
    
    # Update state
    intake_ok(TRUE)
    
    # Redirect after a short delay
    shinyjs::delay(2000, {
      bslib::nav_select("nav", "download")
    })
      
    }, error = function(e) {
      # Handle any errors during submission
      output$submit_status <- renderUI(
        tags$div(class="alert alert-danger", 
          icon("exclamation-triangle"), 
          tags$strong("Error submitting form: "), 
          conditionMessage(e),
          tags$div(class="mt-2",
            actionButton("retry_submit", "Try Again", class = "btn btn-outline-danger btn-sm")
          )
        )
      )
    })
  })
  
  # Retry submit handler
  observeEvent(input$retry_submit, {
    # Clear error and retry
    output$submit_status <- renderUI(NULL)
    # Trigger the submit again
    shinyjs::click("submit_intake")
  })
  
  # Fallback redirect
  observeEvent(input$fallback_redirect, {
    bslib::nav_select("nav", "download")
  })

  # Gate logic: lock until intake is complete (no modal/redirect)
  observe({
    ok <- isTRUE(intake_ok())
    lock_tabs(!ok)
  })

  # Hard gate: prevent navigation to locked tabs even if a link is clicked
  observe({
    current <- input$nav
    if (is.null(current)) return()
    allowed <- isTRUE(intake_ok())
    locked_targets <- c("download","qualtrics_help","processdata","plotting")
    if (!allowed && current %in% locked_targets) {
      bslib::nav_select("nav", "home")
    }
  })


  # ---------------- Access Survey Module ----------------
  accessSurveyServer("access_survey")




  # ---------------- QSF management ----------------
  observeEvent(input$qsf_upload, {
    req(input$qsf_upload$datapath)
    file.copy(input$qsf_upload$datapath, canonical_qsf_path, overwrite = TRUE)
    output$qsf_status <- renderUI(tags$div(class="text-success", icon("check"),
                                           " QSF saved to ", code("www/snq.qsf")))
  })

  output$qsf_status <- renderUI({
    if (file.exists(canonical_qsf_path)) {
      tags$div(class="text-muted", icon("file"),
               " Serving ", code("www/snq.qsf"), " (", format(file.info(canonical_qsf_path)$size, big.mark=","), " bytes)")
    } else {
      tags$div(class="text-danger", icon("triangle-exclamation"),
               tagList(
                 " No QSF found. ",
                 HTML("Download the canonical CSNQ from <a href='https://github.com/bethanyou/CSNQ/blob/12696482a7b5f8d4b20444857a551529ddb57b19/Social_Network_Questionnaire_-_Bethany_working.qsf' target='_blank' rel='noopener'>GitHub</a> and upload it here, or place it at "),
                 code(user_canonical_qsf),
                 "."
               ))
    }
  })

  # Canonical download only
  output$dl_qsf_canonical <- downloadHandler(
    filename = function() "CSNQ_canonical.qsf",
    content  = function(file) file.copy(canonical_qsf_path, file, overwrite = TRUE)
  )

  # ---------------- Explore Data ----------------
  # Use reactive values to cache data and avoid re-reading files
  explore_data <- reactiveValues(
    nw = NULL,
    ld = NULL,
    al = NULL,
    loading = FALSE
  )
  
  # Attempt to auto-load from the Current data directory on startup
  observe({
    current_data_dir <- "/Users/beiniou/Desktop/STEEG/Current data"
    nw_path <- file.path(current_data_dir, "ego_level_network_summary.csv")
    ld_path <- file.path(current_data_dir, "node_level_long.csv")
    al_path <- file.path(current_data_dir, "activity_level_long.csv")
    try({
      if (is.null(explore_data$nw) && file.exists(nw_path)) {
        explore_data$nw <- suppressWarnings(readr::read_csv(nw_path, show_col_types = FALSE))
      }
    }, silent = TRUE)
    try({
      if (is.null(explore_data$ld) && file.exists(ld_path)) {
        explore_data$ld <- suppressWarnings(readr::read_csv(ld_path, show_col_types = FALSE))
      }
    }, silent = TRUE)
    try({
      if (is.null(explore_data$al) && file.exists(al_path)) {
        explore_data$al <- suppressWarnings(readr::read_csv(al_path, show_col_types = FALSE))
      }
    }, silent = TRUE)
  })
  
  nw <- reactive({
    if (!is.null(explore_data$nw)) return(explore_data$nw)
    req(input$nw_csv)
    if (is.null(explore_data$nw) || explore_data$loading) {
      explore_data$loading <- TRUE
      tryCatch({
        explore_data$nw <- suppressWarnings(readr::read_csv(input$nw_csv$datapath, show_col_types = FALSE))
        explore_data$loading <- FALSE
        log_message("Loaded ego-level network data")
      }, error = function(e) {
        explore_data$loading <- FALSE
        log_message(paste("Error loading ego-level data:", conditionMessage(e)), "ERROR")
        stop("Error loading ego-level data: ", conditionMessage(e))
      })
    }
    explore_data$nw
  })
  
  ld <- reactive({
    if (!is.null(explore_data$ld)) return(explore_data$ld)
    req(input$ld_csv)
    if (is.null(explore_data$ld) || explore_data$loading) {
      explore_data$loading <- TRUE
      tryCatch({
        explore_data$ld <- suppressWarnings(readr::read_csv(input$ld_csv$datapath, show_col_types = FALSE))
        explore_data$loading <- FALSE
        log_message("Loaded node-level data")
      }, error = function(e) {
        explore_data$loading <- FALSE
        log_message(paste("Error loading node-level data:", conditionMessage(e)), "ERROR")
        stop("Error loading node-level data: ", conditionMessage(e))
      })
    }
    explore_data$ld
  })
  
  al <- reactive({
    if (!is.null(explore_data$al)) return(explore_data$al)
    req(input$al_csv)
    if (is.null(explore_data$al) || explore_data$loading) {
      explore_data$loading <- TRUE
      tryCatch({
        explore_data$al <- suppressWarnings(readr::read_csv(input$al_csv$datapath, show_col_types = FALSE))
        explore_data$loading <- FALSE
        log_message("Loaded activity-level data")
      }, error = function(e) {
        explore_data$loading <- FALSE
        log_message(paste("Error loading activity-level data:", conditionMessage(e)), "ERROR")
        stop("Error loading activity-level data: ", conditionMessage(e))
      })
    }
    explore_data$al
  })

  output$explore_status <- renderUI({
    if (explore_data$loading) {
      tags$div(class="text-info", icon("spinner", class="fa-spin"), " Loading data...")
    } else {
      has_preloaded <- !is.null(explore_data$nw) && !is.null(explore_data$ld) && !is.null(explore_data$al)
      has_uploads <- !is.null(input$nw_csv) && !is.null(input$ld_csv) && !is.null(input$al_csv)
      if (has_preloaded || has_uploads) {
        tags$div(class="text-success", icon("check"),
                 " Files loaded. Use previews and charts below.")
      } else {
        tags$div(class="text-muted",
                 icon("circle-info"),
                 " Upload all three or place CSVs in Current data: ",
                 code("ego_level_network_summary.csv"), ", ", code("node_level_long.csv"), ", ", code("activity_level_long.csv"))
      }
    }
  })

  output$dt_nw <- renderDT({ req(nw()); datatable(nw(), options = list(pageLength = 10, scrollX = TRUE)) })
  output$dt_ld <- renderDT({ req(ld()); datatable(ld(), options = list(pageLength = 10, scrollX = TRUE)) })
  output$dt_al <- renderDT({ req(al()); datatable(al(), options = list(pageLength = 10, scrollX = TRUE)) })

  output$plt_sizes <- renderPlot({
    req(nw())
    df <- nw()
    size_col <- if ("network_size" %in% names(df)) "network_size" else NULL
    if (is.null(size_col)) return()
    ggplot(df, aes(x = .data[[size_col]])) +
      geom_histogram(bins = 20) +
      labs(title = "Network size distribution", x = "network_size", y = "count")
  })

  output$plt_entropy <- renderPlot({
    req(nw())
    df <- nw()
    xcol <- if ("network_racial_entropy" %in% names(df)) "network_racial_entropy" else NULL
    ycol <- if ("network_language_entropy" %in% names(df)) "network_language_entropy" else NULL
    if (is.null(xcol) || is.null(ycol)) return()
    ggplot(df, aes(x = .data[[xcol]], y = .data[[ycol]])) +
      geom_point(alpha = 0.6) +
      labs(title = "Racial vs. Language entropy", x = "Racial entropy", y = "Language entropy")
  })

  # Optional: reflect threshold in preview (view-only)
  observeEvent(input$validity_threshold, {
    if (is.null(input$nw_csv)) return()
    df <- nw()
    if ("network_75cutoff_validity" %in% names(df)) {
      df$valid_at_threshold <- ifelse(input$validity_threshold >= 0.75,
                                      df$network_75cutoff_validity, df$network_75cutoff_validity)
      output$dt_nw <- renderDT({ datatable(df, options = list(pageLength = 10, scrollX = TRUE)) })
    }
  })


  # ---------------- Admin Functions ----------------
  # Admin authentication state
  admin_authenticated <- reactiveVal(FALSE)
  
  # Data refresh trigger
  data_refresh_trigger <- reactiveVal(0)
  
  # Admin login
  observeEvent(input$admin_login, {
    if (input$admin_password == ADMIN_PASSWORD) {
      admin_authenticated(TRUE)
      output$admin_status <- renderUI(
        tags$div(class="text-success", icon("check"), " Admin authenticated successfully!")
      )
    } else {
      admin_authenticated(FALSE)
      output$admin_status <- renderUI(
        tags$div(class="text-danger", icon("exclamation-triangle"), " Invalid password!")
      )
    }
  })
  
  # Admin logout
  observeEvent(input$admin_logout, {
    admin_authenticated(FALSE)
    updateTextInput(session, "admin_password", value = "")
  })

  # ======================== PROCESS DATA PAGE ================================
  
  # Reactive values to store data
  process_values <- reactiveValues(
    raw_data = NULL,
    node_level_long = NULL,
    ego_level_network_summary = NULL,
    activity_level_long = NULL,
    processing_complete = FALSE
  )
  
  # Enable process button when file is uploaded
  observe({
    if (!is.null(input$process_file)) {
      shinyjs::enable("process_btn")
    } else {
      shinyjs::disable("process_btn")
    }
  })

  # Workflow card navigation handlers (landing page)
  observeEvent(input$wf_access, {
    updateNavbarPage(session, inputId = "nav", selected = "access_survey")
  }, ignoreInit = TRUE)
  observeEvent(input$wf_process, {
    updateNavbarPage(session, inputId = "nav", selected = "processdata")
  }, ignoreInit = TRUE)
  observeEvent(input$go_plotting, {
    updateNavbarPage(session, inputId = "nav", selected = "plotting")
  }, ignoreInit = TRUE)
  
  # File upload
  observeEvent(input$process_file, {
    req(input$process_file)
    
    ext <- tools::file_ext(input$process_file$datapath)
    
    process_values$raw_data <- tryCatch({
      if (ext == "csv") {
        df <- read_csv(input$process_file$datapath, show_col_types = FALSE)
        # Remove Qualtrics header rows if they exist
        if (nrow(df) > 2) {
          first_row_content <- paste(as.character(df[1,]), collapse = " ")
          if (grepl("Start Date|End Date|Response Type", first_row_content)) {
            df <- df %>% slice(-(1:2))
          } else if (any(grepl("ImportId|QID", names(df)))) {
            df <- df %>% slice(-(1:2))
          }
        }
        df
      } else if (ext %in% c("xlsx", "xls")) {
        read_excel(input$process_file$datapath)
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
    
    process_values$processing_complete <- FALSE
  })
  
  # Process data when button is clicked
  observeEvent(input$process_btn, {
    req(process_values$raw_data)
    
    showNotification("Processing data... This may take a few moments.", type = "default", duration = NULL, id = "processing")
     
     tryCatch({
      # Use the comprehensive pipeline (Option A): source and capture outputs
      # Parent must see base/utils and loaded packages; use globalenv() as parent
      env <- new.env(parent = globalenv())
      env$df <- process_values$raw_data
      # Use the canonical snq_pipeline.R.bak just like the Process Data tab expects
      sys.source("/Users/beiniou/Desktop/STEEG/snq_pipeline.R.bak", envir = env)
      # Debug: summarize what the pipeline saw and produced
      try({
        child_ids_in <- if ("ChildID" %in% names(env$df)) unique(as.character(env$df$ChildID)) else character(0)
        child_ids_out <- if (exists("ego_level_network_summary", envir = env) && "ChildID" %in% names(env$ego_level_network_summary)) {
          unique(as.character(env$ego_level_network_summary$ChildID))
        } else character(0)
        msg1 <- paste("[Process Debug] df rows:", nrow(env$df), " unique ChildIDs in: ", paste(child_ids_in, collapse=", "))
        msg2 <- paste("[Process Debug] ego rows:", if (exists("ego_level_network_summary", envir = env)) nrow(env$ego_level_network_summary) else NA,
            " unique ChildIDs out: ", paste(child_ids_out, collapse=", "), "\n")
        cat(msg1, "\n")
        cat(msg2)
        try({
          write(paste0(format(Sys.time(), "%F %T"), " ", msg1), file = "app.log", append = TRUE)
          write(paste0(format(Sys.time(), "%F %T"), " ", msg2), file = "app.log", append = TRUE)
        }, silent = TRUE)
      }, silent = TRUE)
      
      # Store the results produced by the comprehensive pipeline
      process_values$node_level_long <- env$node_level_long
      process_values$ego_level_network_summary <- env$ego_level_network_summary
      process_values$activity_level_long <- env$activity_level_long
      process_values$processing_complete <- TRUE
      
      removeNotification("processing")
      showNotification(paste("Data processing completed successfully!", 
                            "Generated:", nrow(process_values$ego_level_network_summary), "children,",
                            nrow(process_values$node_level_long), "network nodes,",
                            nrow(process_values$activity_level_long), "activities"), 
                      type = "default", duration = 10)
      
     }, error = function(e) {
      removeNotification("processing")
      showNotification(paste("Error processing data:", e$message), type = "error")
      cat("Detailed error:\n")
      print(e)
    })
  })
  
  # Download handlers
  output$download_node <- downloadHandler(
    filename = function() {
      paste0("node_level_long_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(process_values$node_level_long, file, row.names = FALSE)
    }
  )
  
  output$download_ego <- downloadHandler(
    filename = function() {
      paste0("ego_level_network_summary_", Sys.Date(), ".csv")
    },
     content = function(file) {
      write.csv(process_values$ego_level_network_summary, file, row.names = FALSE)
    }
  )
  
  output$download_activity <- downloadHandler(
    filename = function() {
      paste0("activity_level_long_", Sys.Date(), ".csv")
    },
     content = function(file) {
      write.csv(process_values$activity_level_long, file, row.names = FALSE)
    }
  )
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("SNQ_processed_data_", Sys.Date(), ".zip")
    },
     content = function(file) {
      temp_dir <- tempdir()
      
      node_file <- file.path(temp_dir, "node_level_long.csv")
      ego_file <- file.path(temp_dir, "ego_level_network_summary.csv")
      activity_file <- file.path(temp_dir, "activity_level_long.csv")
      
      write.csv(process_values$node_level_long, node_file, row.names = FALSE)
      write.csv(process_values$ego_level_network_summary, ego_file, row.names = FALSE)
      write.csv(process_values$activity_level_long, activity_file, row.names = FALSE)
      
      zip(file, c(node_file, ego_file, activity_file), flags = "-j")
    }
  )
  
  # Control visibility of download buttons
  output$show_download <- reactive({
    process_values$processing_complete
  })
  
  output$show_individual_downloads <- reactive({
    process_values$processing_complete
  })
  
  outputOptions(output, "show_download", suspendWhenHidden = FALSE)
  outputOptions(output, "show_individual_downloads", suspendWhenHidden = FALSE)
  
  # ---------- Full previews ----------
  output$preview_ego <- DT::renderDataTable({
    req(process_values$processing_complete)
    DT::datatable(process_values$ego_level_network_summary, options = list(scrollX = TRUE, pageLength = 25))
  })
  output$preview_node <- DT::renderDataTable({
    req(process_values$processing_complete)
    DT::datatable(process_values$node_level_long, options = list(scrollX = TRUE, pageLength = 25))
  })
  output$preview_activity <- DT::renderDataTable({
    req(process_values$processing_complete)
    DT::datatable(process_values$activity_level_long, options = list(scrollX = TRUE, pageLength = 25))
  })
  
  
  # Load intake data
  intake_data <- reactive({
    # Trigger refresh when data_refresh_trigger changes
    data_refresh_trigger()
    
    path <- file.path("data", "intake.csv")
    if (file.exists(path)) {
      read_csv(path, show_col_types = FALSE)
    } else {
      data.frame(
        timestamp = character(),
        type = character(),
        institution = character(),
        lab = character(),
        email = character(),
        phone_number = character(),
        purpose = character(),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Refresh data
  observeEvent(input$refresh_data, {
    # Increment trigger to force reactive update
    data_refresh_trigger(data_refresh_trigger() + 1)
  })
  
  # Data statistics
  output$data_stats <- renderUI({
    data <- intake_data()
    if (nrow(data) > 0) {
      tagList(
        tags$div(class="alert alert-info",
          tags$h6("Data Summary"),
          tags$p(tags$strong("Total submissions:"), nrow(data)),
          tags$p(tags$strong("Latest submission:"), max(data$timestamp)),
          tags$p(tags$strong("Unique institutions:"), length(unique(data$institution))),
          tags$p(tags$strong("Unique labs:"), length(unique(data$lab))),
          tags$p(tags$strong("With phone numbers:"), sum(nzchar(data$phone_number)))
        )
      )
    } else {
      tags$div(class="alert alert-warning", "No intake data available.")
    }
  })
  
  # Admin intake table
  output$admin_intake_table <- renderDT({
    data <- intake_data()
    if (nrow(data) > 0) {
      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(0, 'desc'))  # Sort by timestamp descending
        ),
        rownames = FALSE
      )
    } else {
      datatable(
        data.frame(Message = "No data available"),
        options = list(pageLength = 1, dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # Download intake data
  output$download_intake <- downloadHandler(
    filename = function() {
      paste0("intake_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- intake_data()
      write_csv(data, file)
    }
  )

  # Download handler for snq_pipeline.rmd (Process Data page)
  output$download_pipeline_rmd <- downloadHandler(
    filename = function() {
      "snq_pipeline.rmd"
    },
    content = function(file) {
      file.copy("/Users/beiniou/Desktop/STEEG/snq_pipeline.rmd", file, overwrite = TRUE)
    }
  )
  
  # Load help requests data
  help_requests_data <- reactive({
    data_refresh_trigger()
    
    path <- file.path("data", "help_requests.csv")
    if (file.exists(path)) {
      read_csv(path, show_col_types = FALSE)
    } else {
      data.frame(
        timestamp = character(),
        type = character(),
        name = character(),
        email = character(),
        subject = character(),
        message = character(),
        uploaded_files = character(),
        status = character(),
        stringsAsFactors = FALSE
      )
    }
  })

  # Refresh help requests
  observeEvent(input$refresh_help, {
    data_refresh_trigger(data_refresh_trigger() + 1)
  })

  # Help requests table
  output$admin_help_table <- renderDT({
    data <- help_requests_data()
    if (nrow(data) > 0) {
      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(0, 'desc'))
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        "status",
        backgroundColor = styleEqual("pending", "#fff3cd")
      )
    } else {
      datatable(
        data.frame(Message = "No help requests available"),
        options = list(pageLength = 1, dom = 't'),
        rownames = FALSE
      )
    }
  })

  # Download help requests
  output$download_help_requests <- downloadHandler(
    filename = function() {
      paste0("help_requests_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- help_requests_data()
      write_csv(data, file)
    }
  )
  
  # Admin authentication status for UI
  output$admin_authenticated <- reactive({
    admin_authenticated()
  })
  outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)

  # Debug information
  output$debug_status <- renderUI({
    tagList(
      tags$p(tags$strong("R Version:"), R.version.string),
      tags$p(tags$strong("Shiny Version:"), packageVersion("shiny")),
       tags$p(tags$strong("Memory Usage:"), {
         mem_txt <- tryCatch({
           paste(round(memory.size()/1024/1024, 2), "MB")
         }, error = function(...) "n/a")
         mem_txt
       }),
      tags$p(tags$strong("Data Directory:"), file.path(getwd(), "data")),
      tags$p(tags$strong("Log File:"), ifelse(file.exists(file.path("data", "app.log")), 
                                              "Available", "Not created")),
      tags$p(tags$strong("Intake Records:"), nrow(intake_data())),
      tags$p(tags$strong("Session ID:"), session$token)
    )
  })
  
  # Debug logs
  output$debug_logs <- renderUI({
    log_file <- file.path("data", "app.log")
    if (file.exists(log_file)) {
      logs <- readLines(log_file, warn = FALSE)
      recent_logs <- tail(logs, 10) # Show last 10 log entries
      if (length(recent_logs) > 0) {
        tags$pre(paste(recent_logs, collapse = "\n"), 
                style = "font-size: 0.8em; max-height: 200px; overflow-y: auto;")
      } else {
        tags$p("No logs available")
      }
    } else {
      tags$p("Log file not found")
    }
  })
  
  # Clear logs
  observeEvent(input$clear_logs, {
    log_file <- file.path("data", "app.log")
    if (file.exists(log_file)) {
      file.remove(log_file)
      log_message("Log file cleared by admin")
      output$debug_logs <- renderUI(tags$p("Logs cleared"))
    }
  })
  
  # Export logs
  output$export_logs <- downloadHandler(
    filename = function() {
      paste0("app_logs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      log_file <- file.path("data", "app.log")
      if (file.exists(log_file)) {
        file.copy(log_file, file)
      } else {
        write("No logs available", file)
      }
    }
  )

  # ---------------- Help & Contact with file uploads and enhanced validation --------------
  clear_help_errors <- function() {
    output$c_name_error <- renderUI(NULL)
    output$c_email_error <- renderUI(NULL)
    output$c_subject_error <- renderUI(NULL)
    output$c_body_error <- renderUI(NULL)
    output$issue_status <- renderUI(NULL)
  }

  # File upload helper - save uploaded files to data directory
  save_uploaded_file <- function(file_input, prefix = "help") {
    if (is.null(file_input) || nrow(file_input) == 0) return(NULL)
    
    saved_files <- character(0)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    for (i in seq_len(nrow(file_input))) {
      original_name <- file_input$name[i]
      file_ext <- tools::file_ext(original_name)
      new_name <- paste0(prefix, "_", timestamp, "_", i, ".", file_ext)
      dest_path <- file.path("data", "uploads", new_name)
      
      # Create uploads directory if it doesn't exist
      dir.create(file.path("data", "uploads"), showWarnings = FALSE, recursive = TRUE)
      
      # Copy file
      file.copy(file_input$datapath[i], dest_path, overwrite = TRUE)
      saved_files <- c(saved_files, paste0(original_name, " -> ", new_name))
    }
    
    return(paste(saved_files, collapse = "; "))
  }

  observeEvent(input$submit_issue, {
    log_message("Help form submission started")
    clear_help_errors()
    
    # Show submitting state
    output$issue_status <- renderUI(
      tags$div(class="text-info", icon("spinner", class="fa-spin"), " Submitting your support request...")
    )
    
    tryCatch({
      # Validate required fields
      has_errors <- FALSE
      
      if (!nzchar(trimws(input$c_name))) {
        output$c_name_error <- renderUI(tags$div(class="text-danger small", "Please enter your name"))
        has_errors <- TRUE
      }
      
      if (!nzchar(trimws(input$c_email))) {
        output$c_email_error <- renderUI(tags$div(class="text-danger small", "Please enter your email address"))
        has_errors <- TRUE
      } else if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", input$c_email)) {
        output$c_email_error <- renderUI(tags$div(class="text-danger small", "Please enter a valid email address"))
        has_errors <- TRUE
      }
      
      if (!nzchar(trimws(input$c_subject))) {
        output$c_subject_error <- renderUI(tags$div(class="text-danger small", "Please enter a subject"))
        has_errors <- TRUE
      }
      
      if (!nzchar(trimws(input$c_body))) {
        output$c_body_error <- renderUI(tags$div(class="text-danger small", "Please describe your issue"))
        has_errors <- TRUE
      }
      
      if (has_errors) {
        output$issue_status <- renderUI(NULL)
        return()
      }
      
      # Handle file uploads
      qsf_files <- save_uploaded_file(input$help_qsf, "qsf")
      csv_files <- save_uploaded_file(input$help_csv, "csv") 
      doc_files <- save_uploaded_file(input$help_docs, "docs")
      
      files_summary <- paste(
        Filter(function(x) !is.null(x) && x != "", 
               c(qsf_files, csv_files, doc_files)), 
        collapse = " | "
      )
      
      # Prepare data for storage
    df <- data.frame(
      timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE),
        type = "help_request",
        name = trimws(gsub("[<>\"'&]", "", input$c_name)),
        email = trimws(tolower(input$c_email)),
        subject = trimws(gsub("[<>\"'&]", "", input$c_subject)),
        message = trimws(gsub("[<>\"'&]", "", input$c_body)),
        uploaded_files = ifelse(files_summary != "", files_summary, "None"),
        status = "pending",
      stringsAsFactors = FALSE
    )
      
      # Save to help requests file
      path <- file.path("data", "help_requests.csv")
      safe_file_operation(function() {
    if (!file.exists(path)) {
      write_csv(df, path, append = FALSE)
          log_message("Created new help_requests.csv file")
    } else {
      existing_data <- read_csv(path, show_col_types = FALSE)
      combined_data <- rbind(existing_data, df)
      write_csv(combined_data, path, append = FALSE)
          log_message("Appended to existing help_requests.csv file")
        }
      }, "Failed to save help request")
      
      # Show success message
      output$issue_status <- renderUI(
        tagList(
          tags$div(class="alert alert-success", 
            icon("check-circle"), 
            tags$strong("Support request submitted successfully!"),
            tags$br(),
            "We'll review your request and respond within 24-48 hours.",
            if (files_summary != "") {
              tagList(tags$br(), tags$small("Uploaded files: ", files_summary))
            }
          )
        )
      )
      
      # Clear form after successful submission
      updateTextInput(session, "c_name", value = "")
      updateTextInput(session, "c_email", value = "")
      updateTextInput(session, "c_subject", value = "")
      updateTextAreaInput(session, "c_body", value = "")
      
      log_message(paste("Help request submitted by", df$email, "with subject:", df$subject))
      
    }, error = function(e) {
      output$issue_status <- renderUI(
        tagList(
          tags$div(class="alert alert-danger", 
            icon("exclamation-triangle"), 
            tags$strong("Error submitting request: "), 
            conditionMessage(e),
            tags$div(class="mt-2",
              actionButton("retry_help", "Try Again", class = "btn btn-outline-danger btn-sm")
            )
          )
        )
      )
      log_message(paste("Help form submission error:", conditionMessage(e)), "ERROR")
    })
  })

  # Retry help submission handler
  observeEvent(input$retry_help, {
    output$issue_status <- renderUI(NULL)
  })

  observeEvent(input$compose_email, {
    if (nzchar(trimws(input$c_subject)) && nzchar(trimws(input$c_body))) {
      subj <- utils::URLencode(paste("CSNQ Support:", input$c_subject), reserved = TRUE)
      body_text <- paste0("Name: ", input$c_name, "\nEmail: ", input$c_email, "\n\nIssue:\n", input$c_body)
      body <- utils::URLencode(body_text, reserved = TRUE)
    browseURL(paste0("mailto:woodwardlab@uchicago.edu?subject=", subj, "&body=", body))
    } else {
      subj <- utils::URLencode("CSNQ Support Request", reserved = TRUE)
      browseURL(paste0("mailto:woodwardlab@uchicago.edu?subject=", subj))
    }
  })
}

shinyApp(ui, server)
