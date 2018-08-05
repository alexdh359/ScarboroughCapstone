#  BUILDING R PACKAGES
#  Peer-graded Assignment: Documenting Code
#  File: fars_functions.R

#' Read file with FARS data
#'
#' This function reads data from .csv file, stored on disk, from the \strong{US
#' National Highway Traffic Safety Administration's} \emph{Fatality Analysis
#' Reporting System} (FARS)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @note fars_read depends on read_csv and tbl_df from the readr and dplyr packages respectively
#'
#' @param filename character string with the name of the file to read
#'
#' @return a data frame with data readed from the csv file
#'
#' @examples
#' \dontrun{
#'   fars_read("data/filename.csv")
#' }
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


#' Read in multiple files by year
#'
#' Make .csv data file name related to the given \code{fars_read_years}
#' The function does not check if the file is available.
#'
#' @param year a vector of years to read in
#' @importFrom dplyr mutate select %>%
#' @return This function returns a string with the data file name for a given
#'   year, and the file path within the package. NULL when an error is encountered
#'
#' @examples
#' \dontrun{
#'   fars_read_years(c(2013, 2014))
#' }
#'
#'
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS years
#'
#' Ancillary function used by \code{fars_summarize_years}
#' @param years A vector with a list of years
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom magrittr "%>%"
#
#' @return A data.frame including entries in data by month, or NULL if the
#'  \code{year} is not valid
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#' @seealso \link{fars_summarize_years}
#' @examples
#' fars_read_years(2013)
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

#' Summarize data by year
#'
#' \code{fars_summarize_years} summarizes yearly accidents data, by month
#' @param years A vector with a list of years to summarize by.
#'
#' @return A data.frame with number of accidents by years summarized by month
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#' @seealso \link{fars_read_years}
#'
#' @examples
#' \dontrun{ 
#' fars_summarize_years(c(2013, 2014))}
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots points for accidents by state and year
#'
#' \code{fars_map_state} displays a plot with a state map including the accidents location by year
#' The function will take a year and state and attempt to plot accidents for a given state number.
#' If there is no data for a state, the function will stop and throw an error. If there are no accidents
#' a message will be returning indicating this.
#'
#' @param state.num state number to plot accidents for
#' @param year year of data to plot accidents that occured
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#' 
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
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