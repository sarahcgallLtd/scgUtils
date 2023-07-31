# HELPER FUNCTIONS FOR GENERIC FUNCTIONS ACROSS THE SCGUTILS PACKAGE

# Function to test quoted or unquoted argument
test_deparse <- function(x) {
  x.try <- try(x, silent = TRUE)
  if (!inherits(x.try, "try-error") && is.character(x.try))
    x.try
  else
    deparse(substitute(x))
}

# Function to round to x decimal places
round_vars <- function(data, decimals) {
  df <- data %>%
    dplyr::mutate(
        dplyr::across(
                    .cols=dplyr::where(is.numeric),
                    .fns= ~ round(.,decimals)
        )
      )
}
