n_of_preregistrations <- function() {
  gsheet_url <- paste0(
    "https://docs.google.com/spreadsheets/d/",
    "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs",
    "/edit?usp=sharing"
  )

  x <- gsheet::gsheet2tbl(gsheet_url)

  colnames(x) <- tolower(colnames(x))

  nrow(x)
}
