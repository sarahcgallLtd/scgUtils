#' Retrieve and Process File from Various Sources
#'
#' @description
#' `get_file` retrieves and preprocesses a file from different sources, including
#' local storage, OneDrive, and the web. It supports multiple file formats such as
#' CSV, Excel (XLSX and XLS), SPSS (SAV), and ZIP archives containing these file types.
#' The function applies preprocessing based on the file type and, for ZIP files, processes
#' and combines data from supported files within the archive.
#'
#' @param file_path The path, ID, or URL of the file to be retrieved.
#' @param source The source of the file: 'local', 'onedrive', or 'web'
#'   (default is 'local').
#' @param row_no The number of rows to skip at the beginning of the file, applicable
#'   for CSV and Excel files (default is 0).
#' @param sheet_no The sheet number to read from Excel files (XLSX or XLS), where
#'   1 is the first sheet (default is 1, which will read the first sheet). Ignored
#'   for non-Excel files.
#' @param file_name Optional; for ZIP files, the name of a specific file within the archive
#'   to process. If `NULL`, all supported files in the ZIP are processed (default is `NULL`).
#' @param add_name Optional; for ZIP files. If `TRUE`, the file name of the unzipped file will be added in
#'   a column called "file_name" within the processed data frame (default is `FALSE`).
#' @param file_type Optional; an override option for extensions. This is particularly useful for url's with
#'   no extension found within the url (default is `NULL`).
#'
#' @return A data frame containing the contents of the file after preprocessing.
#'   For ZIP files, it returns a combined data frame from all processed files, with an
#'   additional column `'file_name'` indicating the source file within the archive.
#'
#' @details
#' The function determines the file type from its extension (e.g., 'csv', 'xls', 'xlsx', 'sav', 'zip'),
#' retrieves the file from the specified source using `authenticate_source`, and preprocesses
#' it with `preprocess_file_type`. Supported file types are handled as follows:
#'
#' - **CSV**: Reads the file with automatic delimiter detection, skips the specified number
#'   of rows, and converts all columns to character initially before auto-detecting types.
#'   Special characters are removed from text columns.
#' - **Excel (XLSX and XLS)**: Reads the specified sheet (via `sheet_no`), skips the specified
#'   number of rows, and processes columns similarly to CSV files.
#' - **SAV (SPSS)**: Reads the file, attempting default encoding first and falling back to
#'   'latin1' if needed. It removes labels and processes factors for cleaner output.
#' - **ZIP**: Extracts supported files (CSV, XLS, XLSX, SAV) from the archive into a unique
#'   temporary directory, processes them, and combines the data into a single data frame with
#'   an additional `'file_name'` column. If `file_name` is specified, only that file is processed.
#'
#' If the file type is not supported, an error is thrown.
#'
#' @examples
#' \dontrun{
#'   # Retrieve a local CSV file
#'   data <- get_file("path/to/local/file.csv")
#'
#'   # Retrieve a local Excel file, reading the second sheet
#'   data <- get_file("path/to/local/file.xlsx", sheet_no = 2)
#'
#'   # Retrieve a file from OneDrive
#'   data <- get_file("file-id", source = "onedrive")
#'
#'   # Retrieve and preprocess a Google Drive file, skipping the first row
#'   data <- get_file("file-id", source = "googledrive", row_no = 1)
#'
#'   # Retrieve a file from the web, skipping the first row
#'   data <- get_file("https://example.com/data.csv", source = "web")
#'
#'   # Retrieve and process all supported files from a local ZIP archive
#'   data <- get_file("path/to/local/archive.zip")
#'
#'   # Retrieve a file from the web with no extension in the url
#'   data <- get_file("https://example.com/data", source = "web", file_type = "zip")
#'
#'   # Retrieve and process a specific file from a ZIP archive
#'   data <- get_file("path/to/local/archive.zip", file_name = "specific_file.csv")
#' }
#'
#' @export
get_file <- function(file_path,
                     source = c("local",
                                "onedrive",
                                #"googledrive",
                                "web"),
                     row_no = 0,
                     sheet_no = 1,
                     file_name = NULL,
                     add_name = FALSE,
                     file_type = NULL
) {
  # Get source
  source <- match.arg(source)

  # Define supported file extensions
  supported_exts <- c("zip", "csv", "xls", "xlsx", "sav")

  if (is.null(file_type )) {
    # Try to get file extension using tools::file_ext
    file_type <- tolower(tools::file_ext(file_path))

    # Fallback for web sources if no extension is found
    if (source == "web" & (file_type == "" || !file_type %in% supported_exts)) {
      # Convert URL to lowercase for case-insensitive matching
      url_lower <- tolower(file_path)

      # Find the first supported extension in the URL
      for (ext in supported_exts) {
        if (grepl(paste0("\\.", ext, "(?![a-zA-Z0-9])"), url_lower, perl = TRUE)) {
          file_type <- ext
          break
        }
      }

      # If still no extension found, error out
      if (file_type == "" || !file_type %in% supported_exts) {
        stop("Could not determine file type from URL: ", file_path)
      }
    }

    # Validate file type for all sources
    if (!file_type %in% supported_exts) {
      stop("Unsupported file type or file does not exist. check `file_path` and try again.")
    }
  }

  # Authenticate based on source
  file_path <- authenticate_source(file_path, source)

  # Handle zip files
  if (file_type == "zip") {
    data <- unzip_files(file_path, row_no, sheet_no, file_name, add_name)

  } else {
    # Preprocess file based on type
    data <- preprocess_file_type(file_path, file_type, row_no, sheet_no)
  }

  return(data)
}


get_file_type_from_url <- function(url) {
  pattern <- "openagent&([^&]+)"
  match <- regexpr(pattern, url)
  if (match > 0) {
    file_name <- regmatches(url, match)[1]
    file_name <- sub("openagent&", "", file_name)
    return(tolower(tools::file_ext(file_name)))
  }
  return(tolower(tools::file_ext(url)))
}


#' Authenticate and Retrieve File from Various Sources
#'
#' This internal helper function for `get_file` authenticates and retrieves a file
#' from a specified source, supporting local storage, OneDrive, Google Drive, and web URLs.
#'
#' @param file_path The path, ID, or URL of the file to be retrieved.
#' @param source The source from which to retrieve the file: 'local', 'onedrive',
#'   'googledrive', or 'web'.
#'
#' @return The path to the downloaded or retrieved file.
#'
#' @details
#' Depending on the `source`, the function either checks for the file's existence in
#' the local file system, authenticates and downloads it from OneDrive or Google Drive,
#' or directly downloads it from a specified web URL.
#'
#' @noRd
authenticate_source <- function(file_path,
                                source
) {
  # ==============================================================#
  # LOCAL:
  if (source == "local") {
    if (!file.exists(file_path)) {
      stop("File does not exist: ", file_path)
    }
    # ==============================================================#
    # ONE DRIVE:
  } else if (source == "onedrive") {
    # Authenticate with OneDrive
    od <- Microsoft365R::get_business_onedrive()
    # Get file
    file <- od$get_item(file_path)
    # Create a tempfile for the download
    local_file_path <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
    # Download the file to the temporary path
    file$download(local_file_path, overwrite = TRUE)
    # Set file_path to the local temporary path
    file_path <- local_file_path
    # ==============================================================#
    # GOOGLE DRIVE:
    # TODO: authentication unsupported by Google for "less secure apps". Need to work out solution.
    # } else if (source == "googledrive") {
    # # Authenticate with Google
    # googledrive::drive_auth()
    # # Get file
    # file <- googledrive::drive_get(file_path)
    # # Download file
    # file_path <- googledrive::drive_download(file, overwrite = TRUE)
    # ==============================================================#
    # WEBSITE:
  } else if (source == "web") {
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
    # Download file
    suppressWarnings(suppressMessages(
      utils::download.file(file_path, destfile = temp_file, mode = "wb", quiet = TRUE)
    ))
    file_path <- temp_file
    # ==============================================================#
  }
  return(file_path)
}


#' Unzip and Process Files from a Zip Archive
#'
#' This function extracts files from a specified zip archive into a unique temporary directory,
#' processes supported file types (CSV, Excel, SPSS), and combines the data into a single data frame.
#' It ensures that the temporary directory is unique to avoid conflicts with other operations and
#' cleans it up automatically after processing.
#'
#' @param file_path The path to the zip file to be processed.
#' @param row_no The number of rows to skip when reading the files.
#' @param sheet_no The sheet number to read from Excel files.
#' @param file_name Optional; the name of a specific file within the zip to process.
#'        If `NULL`, all files are processed.
#' @param add_name Optional; adds the name of the file in a column within the data frame.
#'
#' @return A data frame containing the combined data from all processed files, with an
#'         additional column `'file_name'` indicating the source file.
#'
#' @details
#' The function supports the following file types:
#' - **CSV** (`.csv`)
#' - **Excel** (`.xls` and `.xlsx`)
#' - **SPSS** (`.sav`)
#'
#' Unsupported file types are skipped with a warning. If no supported files are found in
#' the zip archive, the function stops with an error. Each processed file's data is read
#' using `preprocess_file_type()`, and a `'file_name'` column is added to identify the
#' source file. The temporary directory is created using `tempdir()` with a unique prefix
#' and is automatically deleted via `on.exit(unlink(...))` after processing completes.
#'
#' If `file_name` is specified and the file does not exist in the zip archive, the function
#' stops with an error.
#'
#' This function relies on the `utils::unzip()`, `tools::file_ext()`, and `dplyr::bind_rows()`
#' functions, as well as a custom `preprocess_file_type()` function to handle file reading.
#'
#' @noRd
unzip_files <- function(
  file_path,
  row_no,
  sheet_no,
  file_name,
  add_name
) {
  # Create a unique temporary directory for extraction
  temp_dir <- file.path(tempdir(), basename(tempfile(pattern = "unzip_")))
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Unzip the files into the unique temporary directory
  utils::unzip(file_path, exdir = temp_dir)

  # Determine which files to process
  if (!is.null(file_name)) {
    files_to_process <- file.path(temp_dir, file_name)
    if (!file.exists(files_to_process)) {
      stop("Specified file not found in zip archive: ", file_name)
    }
  } else {
    files_to_process <- list.files(temp_dir, full.names = TRUE)
  }

  # Initialise an empty data frame to store combined results
  combined_data <- data.frame()

  # Process each file and combine immediately
  for (file in files_to_process) {
    file_type_inner <- tolower(tools::file_ext(file))

    if (file_type_inner %in% c("csv", "xls", "xlsx", "sav")) {
      # Read the file into a data frame
      data <- preprocess_file_type(file, file_type_inner, row_no, sheet_no)

      # Add file_name column if TRUE
      if (add_name) {
        data$file_name <- basename(file)
      }

      # Combine with existing data
      combined_data <- dplyr::bind_rows(combined_data, data)

    } else {
      warning("Skipping unsupported file type: ", file_type_inner)
    }
  }

  # Check if any data was processed
  if (nrow(combined_data) == 0) {
    stop("No supported files found in the zip archive")
  }

  return(combined_data)
}


#' Preprocess File Based on File Type
#'
#' An internal helper function for `get_file` that preprocesses a file based on
#' its type, handling CSV and SPSS (SAV) file formats.
#'
#' @param file_path The path to the file.
#' @param file_type The type of the file: 'csv' or 'sav'.
#' @param row_no The number of rows to skip for CSV files.
#' @param sheet_no The sheet number for Excel files.
#'
#' @return A data frame with the contents of the processed file, with appropriate preprocessing
#'   applied based on the file type.
#'
#' @details
#' For CSV files, the function reads the file while skipping a specified number of rows
#' and removes special characters. For SAV files, it first attempts to read the file and
#' falls back to a different encoding if necessary. Additionally, it unlables the data
#' and removes unused factor levels for SAV files, ensuring cleaner and more usable data output.
#'
#' @noRd
preprocess_file_type <- function(file_path,
                                 file_type,
                                 row_no,
                                 sheet_no
) {
  # ==============================================================#
  # .csv, .xls, and .xlsx files (Excel files)
  if (file_type %in% c("csv", "xls", "xlsx")) {
    if (file_type == "csv") {
      # Read csv file
      data <- vroom::vroom(
        file_path,
        delim = NULL,                                                 # Automatically detect the delimiter
        skip = row_no,                                                # Skip the specified number of rows
        col_types = vroom::cols(.default = vroom::col_character()),   # Read all columns as character
        show_col_types = FALSE                                        # Suppress column type messages
      )
    } else {
      # Read Excel file, skipping row_no rows, all columns as text
      data <- readxl::read_excel(file_path,
                                 sheet = sheet_no,
                                 skip = row_no,
                                 col_types = "text"
      )
    }

    # Clean columns prior to column conversion:
    for (col in names(data)) {
      # Trim all whitespace
      data[[col]] <- trimws(data[[col]])
      # Remove quotes
      data[[col]] <- gsub("^\"|\"$", "", data[[col]])
      # Remove commas
      data[[col]] <- gsub(",", "", data[[col]])
      # Replace empty or invalid entries with NA
      data[[col]] <- ifelse(data[[col]] %in% c("", "N/A", "-", "NaN", "null"), NA, data[[col]])

      # Attempt to convert to numeric
      numeric_col <- suppressWarnings(as.numeric(data[[col]]))
      # If no new NAs are introduced, assign the numeric vector
      if (!any(is.na(numeric_col) & !is.na(data[[col]]))) {
        data[[col]] <- numeric_col
      }
    }

    # Automatically detect and convert column types (silently)
    invisible(utils::capture.output({
      data <- suppressMessages(readr::type_convert(data))
    }, type = "output"))

    # Remove special characters
    data <- dplyr::mutate(data,
                          dplyr::across(
                            dplyr::where(is.character),
                            ~iconv(., from = "UTF-8", to = "ASCII//TRANSLIT")
                          ))
    # ==============================================================#
  } else if (file_type == "sav") {
    # .sav (SPSS files)
    tryCatch({
      # First attempt to read the file
      data <- haven::read_sav(file_path)
    }, error = function(e) {
      # If an error occurs, try reading with 'latin1' encoding
      message("Error encountered. Trying to read with 'latin1' encoding...")
      tryCatch({
        data <- haven::read_sav(file_path, encoding = "latin1")
      }, error = function(e) {
        # If an error occurs again, stop and return an error message
        stop("Error reading the file with both default and 'latin1' encoding: ", e)
      })
    })
    # Unlabel data
    data <- labelled::unlabelled(data)
    # Remove unused factor levels
    data <- process_factors(data)

    # ==============================================================#
  } else {
    stop("Unsupported file type")
  }
  # ==============================================================#
  return(data)
}