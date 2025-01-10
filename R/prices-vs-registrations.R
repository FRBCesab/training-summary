prices_vs_registrations <- function() {
  
  gsheet_url <- paste0("https://docs.google.com/spreadsheets/d/", 
                       "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs", 
                       "/edit?usp=sharing")
  
  x <- gsheet::gsheet2tbl(gsheet_url) |> 
    as.data.frame()
  
  colnames(x) <- tolower(colnames(x))
  
  x <- x |> 
    dplyr::group_by(course, year) |> 
    dplyr::summarize(n_registration = dplyr::n()) |> 
    dplyr::ungroup()
  
  
  gsheet_url <- paste0("https://docs.google.com/spreadsheets/d/", 
                       "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs", 
                       "/edit?gid=44693846&usp=sharing")
  
  y <- gsheet::gsheet2tbl(gsheet_url) |> 
    as.data.frame()
  
  colnames(y) <- tolower(colnames(y))
  
  y <- y[ , c("course", "year", "registration")]
  
  
  x <- merge(x, y, by = c("course", "year"))
  colnames(x)[4] <- "price"
  
  
  x$"price" <- gsub("\\,", ".", x$"price")
  x$"price" <- gsub("\\s| ", "", x$"price")
  x$"price" <- gsub("€", "", x$"price")
  x$"price" <- as.numeric(x$"price")
  
  
  ggplot(x) +
    
    geom_point(aes(x = price, y = n_registration), size = 2) +
    geom_smooth(aes(x = price, y = n_registration), method = "lm") +
    facet_wrap(vars(course)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    xlab("Prix d'inscription") +
    ylab("Nombre de pré-inscriptions")
  
  ggsave(filename = "registrations_vs_prices.png", path = here::here("figures"),
         width = 15, height = 10, units = "in", dpi = 600, scale = .5)
}
