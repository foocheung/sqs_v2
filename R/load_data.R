# ==============================================================================
# LOCAL DATA LOADING FUNCTIONS
# ==============================================================================
# Replaces foodata2 package dependency
# Users can customize these data files in inst/data/
# ==============================================================================

#' Convert Excel Serial Date to R Date
#'
#' @param serial_dates Vector of Excel serial dates, numeric dates, or already-formatted dates
#' @return Date vector
#'
#' @keywords internal
excel_serial_to_date <- function(serial_dates) {
  # If already a date, return as-is
  if (inherits(serial_dates, "Date")) {
    return(serial_dates)
  }

  # Initialize result vector
  result <- rep(as.Date(NA), length(serial_dates))

  # Handle numeric values (Excel serial numbers)
  if (is.numeric(serial_dates)) {
    valid_idx <- !is.na(serial_dates) & serial_dates > 0
    result[valid_idx] <- as.Date(serial_dates[valid_idx], origin = "1899-12-30")
    return(result)
  }

  # Handle character/mixed values
  serial_dates <- as.character(serial_dates)

  for (i in seq_along(serial_dates)) {
    if (is.na(serial_dates[i])) {
      result[i] <- as.Date(NA)
      next
    }

    # Try to convert as numeric (Excel serial)
    numeric_val <- suppressWarnings(as.numeric(serial_dates[i]))

    if (!is.na(numeric_val) && numeric_val > 0) {
      # It's a numeric Excel serial
      result[i] <- as.Date(numeric_val, origin = "1899-12-30")
    } else {
      # Try to parse as character date string
      tryCatch({
        result[i] <- as.Date(serial_dates[i])
      }, error = function(e) {
        # If all else fails, return NA
        result[i] <<- as.Date(NA)
      })
    }
  }

  return(result)
}

#' Load Historical Coefficient of Variation Reference Data
#'
#' Loads historical CV data used for Levey-Jennings plots and trend analysis.
#' This corresponds to foodata2::load_data4()
#'
#' Reads from: inst/data/synthetic_data.xlsx
#'
#' @return Data frame with historical CV reference data
#'   Expected columns:
#'   - SampleType: "Calibrator", "QC", etc.
#'   - PlateId: Identifier for assay plate
#'   - ExpDate: Date of experiment (converted from Excel serial format)
#'   - 10%, 50%, 90%: CV percentiles
#'
#' @details
#' Data file: inst/data/synthetic_data.xlsx
#'
#' The function:
#' 1. Reads the Excel file with ExpDate as text (to preserve format)
#' 2. Converts ExpDate from Excel serial format to R Date class
#' 3. Returns as a tibble for consistency
#'
#' To prepare your own data:
#' \preformatted{
#'   my_cv_data <- data.frame(
#'     SampleType = c("Calibrator", "QC", ...),
#'     PlateId = c("Plate_001", "Plate_002", ...),
#'     ExpDate = as.Date(c("2023-01-15", "2023-01-16", ...)),
#'     `10%` = c(8.5, 7.2, ...),
#'     `50%` = c(10.1, 9.8, ...),
#'     `90%` = c(12.3, 11.5, ...)
#'   )
#'   writexl::write_xlsx(my_cv_data, "inst/data/synthetic_data.xlsx")
#' }
#'
#' @examples
#' \dontrun{
#'   cv_data <- load_historical_cv_data()
#'   summary(cv_data)
#' }
#'
#' @export
load_historical_cv_data <- function() {
  # Construct path to data file
  data_path <- system.file(
    "data",
    "synthetic_data.xlsx",
    package = "sqs"
  )

  # Check if file exists
  if (!file.exists(data_path)) {
    warning(
      "Historical CV data file not found at:\n  ",
      data_path, "\n",
      "Place 'synthetic_data.xlsx' in inst/data/ directory.\n",
      "Levey-Jennings plots will show 'Historical data not available'."
    )
    return(NULL)
  }

  # Read and return
  tryCatch({
    # Peek at one row to count columns
    n_cols <- ncol(readxl::read_xlsx(data_path, n_max = 1))

    # Force ExpDate to text, guess the rest
    col_types <- c("text", rep("guess", n_cols - 1))

    data <- readxl::read_xlsx(data_path, col_types = col_types)
    data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)

    # Clean column names (remove backticks if present)
    colnames(data) <- gsub("`", "", colnames(data))

    # Fix ExpDate: convert serials into readable dates
    # Ensure ExpDate is character before conversion
    if ("ExpDate" %in% colnames(data)) {
      data$ExpDate <- as.character(data$ExpDate)
      data$ExpDate <- excel_serial_to_date(data$ExpDate)
      # Ensure it's Date class (not character or factor)
      data$ExpDate <- as.Date(data$ExpDate)
    }

    # Convert to tibble and ensure all factors are character
    result <- tibble::as_tibble(data)

    # Make sure ExpDate is explicitly Date class before returning
    if ("ExpDate" %in% colnames(result)) {
      result$ExpDate <- as.Date(result$ExpDate)
    }

    return(result)

  }, error = function(e) {
    warning("Error reading historical CV data: ", e$message)
    NULL
  })
}

#' Load Filtered SOMAmers/Protein Targets
#'
#' Loads the list of protein targets to filter/exclude from certain analyses.
#' This corresponds to foodata2::load_data2()
#'
#' @return Data frame with filtered protein targets
#'   Columns should include: SeqId, Target, TargetFullName, etc.
#'
#' @details
#' Data file: inst/data/v4.1_filtered_somamers.xlsx
#' Users can customize this list by editing the Excel file directly.
#'
#' @examples
#' \dontrun{
#'   avoid_proteins <- load_filtered_somamers()
#'   head(avoid_proteins)
#' }
#'
#' @export
load_filtered_somamers <- function() {
  # Construct path to data file
  data_path <- system.file(
    "data",
    "v4.1_filtered_somamers.xlsx",
    package = "sqs"
  )

  # Check if file exists
  if (!file.exists(data_path)) {
    warning(
      "Filtered somamers data file not found at:\n  ",
      data_path, "\n",
      "Place 'v4.1_filtered_somamers.xlsx' in inst/data/ directory.\n",
      "Using empty data frame as fallback."
    )
    return(data.frame())
  }

  # Read and return
  tryCatch({
    readxl::read_xlsx(data_path)
  }, error = function(e) {
    warning("Error reading filtered somamers file: ", e$message)
    data.frame()
  })
}

#' Load Example ADAT Data
#'
#' Loads example SomaDataIO ADAT format data for demonstration.
#' Used when user selects "Load Example Data" in the app.
#'
#' @return SomaDataIO adat object (SomaLogic data format)
#'
#' @details
#' Data file: inst/data/example_data.adat
#'
#' This is the original SomaLogic binary format, used for:
#' - App demonstration
#' - Testing functionality
#' - Reference implementation
#'
#' @examples
#' \dontrun{
#'   example_data <- load_example_adat()
#'   dim(example_data)
#' }
#'
#' @export
load_example_adat <- function() {
  # Construct path to data file
  data_path <- system.file(
    "data",
    "example_data.adat",
    package = "sqs"
  )

  # Check if file exists
  if (!file.exists(data_path)) {
    stop(
      "Example ADAT data file not found at:\n  ",
      data_path, "\n",
      "Place an example '.adat' file in inst/data/example_data.adat"
    )
  }

  # Read and return using SomaDataIO
  tryCatch({
    SomaDataIO::read_adat(data_path)
  }, error = function(e) {
    stop("Error reading example ADAT file: ", e$message)
  })
}

#' Load Example ADAT Header
#'
#' Loads and parses the header from example ADAT file.
#'
#' @return Parsed header from ADAT file
#'
#' @examples
#' \dontrun{
#'   header <- load_example_adat_header()
#' }
#'
#' @export
load_example_adat_header <- function() {
  # Construct path to data file
  data_path <- system.file(
    "data",
    "example_data.adat",
    package = "sqs"
  )

  if (!file.exists(data_path)) {
    stop("Example ADAT data file not found at: ", data_path)
  }

  tryCatch({
    SomaDataIO::parseHeader(data_path)
  }, error = function(e) {
    stop("Error parsing example ADAT header: ", e$message)
  })
}

# ==============================================================================
# HELPER: Data file information and validation
# ==============================================================================

#' Get Data File Status and Paths
#'
#' Returns information about data files and their locations.
#' Useful for debugging and user information.
#'
#' @return List with file paths and existence status
#'
#' @examples
#' \dontrun{
#'   status <- get_data_file_status()
#'   cat(status$summary)
#' }
#'
#' @export
get_data_file_status <- function() {
  paths <- list(
    filtered_somamers = system.file(
      "data",
      "v4.1_filtered_somamers.xlsx",
      package = "sqs"
    ),
    historical_cv = system.file(
      "data",
      "synthetic_data.xlsx",
      package = "sqs"
    ),
    example_adat = system.file(
      "data",
      "example_data.adat",
      package = "sqs"
    )
  )

  exists <- sapply(paths, file.exists)

  summary <- paste(
    "Data File Status:",
    paste(
      sprintf(
        "  %s: %s (%s)",
        names(exists),
        ifelse(exists, "✓ Found", "✗ Missing"),
        paths
      ),
      collapse = "\n"
    ),
    sep = "\n"
  )

  list(
    paths = paths,
    exists = exists,
    summary = summary
  )
}
