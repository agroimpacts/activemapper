#' Connection to database
#' @param host URL of host
#' @param dbname Database name
#' @param user Database user name
#' @param password Database password
#' @export
labeller_connection <- function(host, dbname, user, password) {
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = host,
                        dbname = dinfo$db_production_name,
                        user = dinfo$db_username, password = dinfo$db_password)
  return(con)
}

#' Read in data from multiple labeller databases
#' @param connection Database connection provided by labeller_connection
#' @param instances A character vector specifying the complete URLs of servers
#' @param table Table names to get from each instance
#' @param variables Vector of variable names to get from the table
#' @param as_tibble Returns output as single tibble (TRUE) or list (FALSE)
labeller_data <- function(connection, instances, table, variables,
                          as_tibble = TRUE) {
  # tstamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  # params <- yaml::yaml.load_file(here::here("common/config.yaml"))
  # dinfo <- params$mapper
  # get instance data
  labeller_data <- purrr::map(1:length(instances), function(x) {  # x <- 1
    host <- instances[x]
    table_data <- tbl(connection, table) %>% select(!!variables) %>%
      collect() %>% mutate(instance = x)
    return(table_data)
  })
  if(as_tibble) {
    out <- purrr::reduce(labeller_data, rbind)
  } else if(as_tibble == FALSE) {
    out <- labeller_data
  } else {
    out <- "Need to specify as_tibble as TRUE or FALSE"
  }
  return(out)
}

