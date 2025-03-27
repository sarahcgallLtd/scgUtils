#' Retrieve and Process File from Various Sources
#'
#' @description
#' `get_file` handles the retrieval and initial processing of a file from different
#' sources, including local storage, OneDrive, and directly from the web.
#' It also preprocesses the file based on its type.
#'
#' @param file_path The path, ID, or URL of the file to be retrieved.
#' @param source The source of the file: 'local', 'onedrive', or 'web'
#'   (default is 'local').
#' @param row_no The number of rows to skip at the beginning of the file, applicable
#'   for CSV files (default is 0).
#'
#' @return A data frame containing the contents of the file.
#'
#' @details
#' The function first identifies the file type based on its extension, then retrieves
#' the file from the specified source using `authenticate_source`. After retrieval,
#' the file is preprocessed according to its type using `preprocess_file_type`,
#' which handles different formats like CSV and SAV.
#'
#' @examples
#' \dontrun{
#'   # Retrieve a local CSV file
#'   data <- get_file("path/to/local/file.csv")
#'
#'   # Retrieve a file from OneDrive
#'   data <- get_file("file-id", source = "onedrive")
#'
#'   # Retrieve and preprocess a Google Drive file, skipping the first row
#'   data <- get_file("file-id", source = "googledrive", row_no = 1)
#'
#'   # Retrieve a file directly from a web URL
#'   data <- get_file("https://example.com/data.csv", source = "web")
#' }
#'
#' @export
get_file <- function(file_path,
                     source = c("local",
                                "onedrive",
                                #"googledrive",
                                "web"),
                     row_no = 0
) {
  # Determine file type
  file_type <- tolower(tools::file_ext(file_path))

  # Get source
  source <- match.arg(source)

  # Authenticate based on source
  file_path <- authenticate_source(file_path, source)

  # Preprocess file based on type
  data <- preprocess_file_type(file_path, file_type, row_no)

  return(data)
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
    local_file_path <- tempfile(fileext = tools::file_ext(file_path))
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
    temp_file <- tempfile()
    # Download file
    suppressWarnings(suppressMessages(
      utils::download.file(file_path, destfile = temp_file, mode = "wb", quiet = TRUE)
    ))
    file_path <- temp_file
    # ==============================================================#
  }
  return(file_path)
}

#' Preprocess File Based on File Type
#'
#' An internal helper function for `get_file` that preprocesses a file based on
#' its type, handling CSV and SPSS (SAV) file formats.
#'
#' @param file_path The path to the file.
#' @param file_type The type of the file: 'csv' or 'sav'.
#' @param row_no The number of rows to skip for CSV files.
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
                                 row_no
) {
  # ==============================================================#
  # .csv
  if (file_type == "csv") {
    # Read csv
    data <- readr::read_csv(file_path,
                            skip = row_no,
                            show_col_types = FALSE)
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