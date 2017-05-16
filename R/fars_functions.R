## NOTE: using the tag @import in the roxygen2 tags adds a package import to the
## package NAMESPACE, making their functions available inside the built package.
## This works in most cases, but I could not make it work for the maps package,
## that does not export the data necessary for the maps::map() function.
##
## I also included a Warning in the function documentation, in case the function
## is not loaded as part of a package.

#' Reads a data file and returns a tibble
#'
#' Reads a csv file and import the data, returning a tibble.
#'
#' @details The data type in the columns is guessed automatically by the
#'   \code{\link[readr]{read_csv}} function in the \code{\link{readr}} package.
#'   Additional messages and progress are supressed to limit the console
#'   output.
#'
#'   If the file is not found at the specified path, the function throws
#'   an error.
#'
#' @param filename path to a csv data file, as a character string.
#'
#' @return A tibble, enhanced implementation of data.frame
#'   (see \code{\link{tibble}}).
#'
#' @examples
#' data2013 <- fars_read('fars_data/accident_2013.csv.bz2')
#' data2014 <- fars_read('fars_data/accident_2014.csv.bz2')
#'
#' @import readr
#' @import dplyr
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Builds a filename as character string
#'
#' @description Inserts a year number into a base string, in order to build a
#'   filename.
#'
#' @details The resulting string can be passed to other functions to process
#'   the target data file. It is a convenience function to process many files
#'   that differ only in the year number.
#'
#' @param year a year number, as integer.
#'
#' @return A filename as a character string.
#'
#' @seealso (\code{\link{fars_read}})
#'
#' @examples
#' print(make_filename(2013))
#' filename <- make_filename(2014)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads a vector of files and returns months and years
#'
#' The function reads a list of csv data files, specified by year, and returns
#'   a list of tibbles \code{\link{tibble}} with two columns: month number and
#'   year combinations.
#'
#' @details If any of the years leads to an inexistent file, the function
#'   throws a warning for each.
#'
#' @note
#' The data files must be in the current working directory (see also
#'   \code{\link{getwd}}) to be found.
#'
#' @section Warning:
#'   \code{\link{dplyr}} package should be attached (\code{library(dplyr)}),
#'   or the function will throw an error because it cannot find the pipe
#'   operator.
#'
#' @param years a vector of years, as integers.
#'
#' @return A list of tibbles (see \code{\link{tibble}})).
#'
#' @seealso (\code{\link{make_filename}})
#'
#' @examples
#' year_list <- fars_read_years(2013)
#' year_list <- fars_read_years(c(2013, 2014, 2015))
#'
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Reads a vector of files and returns total observations by month and year
#'
#' The function takes a vector of year numbers, reads the corresponding files
#'   (using \code{\link{fars_read_years}}) and counts the number of rows in
#'   each, aggregated by month number.
#'   Then it builds a single table with one row per month number, and a column
#'   for each file / year, where the values represent the total number of
#'   observations.
#'
#' @note
#' The data files must be in the current working directory (see also
#'   \code{\link{getwd}}) to be found.
#'
#' @param years a vector of years, as integers.
#'
#' @return A tibble, enhanced implementation of data.frame
#'   (see \code{\link{tibble}}), with 12 rows and 1 column for each year.
#'
#' @seealso (\code{\link{fars_read_years}})
#'
#' @examples
#' aggregated <- fars_summarize_years(2013)
#' aggregated <- fars_summarize_years(c(2013, 2014, 2015))
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draws observations as point on a map
#'
#' \code{fars_map_state} takes a State number and a year as arguments.
#'   Reads the datafile for the corresponding year, and draws a map of the
#'   State with points at the longitude and latitude of the observations /
#'   accidents in the dataset.
#'
#' @details
#' If the value for the \code{state.num} parameter does not correspond to a
#'   valid State in the dataset, the function will throw an error.
#'   If the State and year combination has zero observations, a message is
#'   printed.
#'   All the points outside of Longitude > 900 or Latitude > 90 are nullified and
#'   not drawn on the map.
#'
#' @note
#' The data files must be in the current working directory (see also
#'   \code{\link{getwd}}) to be found.
#'
#' @section Warning:
#'   \code{\link{maps}} package should be attached (\code{library(maps)}),
#'   or the function will throw an error because maps does not export the data
#'   (\code{object 'stateMapEnv'}) for the \code{\link{map}} function.
#'
#' @param state.num a State number, an integer between 1 and 56.
#' @param year a year, as integer.
#'
#' @return \code{NULL (empty)} returned invisibly \code{\link{invisible}}.
#'   As side effect, the function will plot a State map with the observations
#'   (accidents) plotted as points on the map.
#'
#' @examples
#' fars_map_state(1, 2013)
#' fars_map_state(48, 2015)
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
