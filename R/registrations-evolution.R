registrations_evolution <- function() {
  
  gsheet_url <- paste0("https://docs.google.com/spreadsheets/d/", 
                       "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs", 
                       "/edit?usp=sharing")
  
  x <- gsheet::gsheet2tbl(gsheet_url)
  
  colnames(x) <- tolower(colnames(x))
  
  # x <- x[x$"year" < 2025, ]
  
  students <- tapply(x$"fullname", list(x$"year", x$"course"), length, default = 0) |> 
    as.data.frame()
  students <- data.frame("year" = as.numeric(rownames(students)), students)
  
  colnames(students) <- colnames(students) |> 
    tolower() |> 
    gsub("\\.", "-", x = _)
  
  
  
  png(filename  = here::here("figures", "number_of_registrations.png"), 
      width     = 16, 
      height    = 8, 
      units     = "in", 
      pointsize = 16,
      res       = 600)
  
  par(mar = c(1.8, 3.7, 0.5, 0.5), mgp = c(0, 0.3, 0), family = "serif", new = FALSE)
  
  plot(0, pch = 19, xlim = range(students$year),
       ylim = c(0, 100), xlab = "", cex = 1.3, lwd = 1.75,
       ylab = "", axes = FALSE, col = "#c23438", type = "n")
  
  grid(lwd = 0.5)
  box()
  axis(1, lwd = 0, lwd.ticks = 0, cex.axis = 1.2)
  axis(2, lwd = 0, lwd.ticks = 0, cex.axis = 1.2, las = 1)
  
  for (i in 2:ncol(students)) {
    
    x <- students[ , c(1, i)]
    x <- x[x[ , 2] > 0, ]
    
    points(x[ , 1], x[ , 2], type = "b", pch = 19,
           cex = 1.8, lwd = 2.5, col = "#ffffff")
    points(x[ , 1], x[ , 2], type = "b", pch = 19,
           cex = 1.5, lwd = 2, col = color_scheme()[colnames(x)[2]])
    
    favicon <- png::readPNG(here::here("images", "favicons", 
                                       paste0(colnames(students)[i], ".png")))
    
    if (i == 2) {
      add_image(image = favicon, x = 2024, y = 21, size = 4)
    }
    
    if (i == 3) {
      add_image(image = favicon, x = 2022, y = 23, size = 4)
    }
    
    if (i == 4) {
      add_image(image = favicon, x = 2025, y = 41, size = 4)
    }
    
    if (i == 5) {
      add_image(image = favicon, x = 2019, y = 14, size = 4)
    }
    
    if (i == 6) {
      add_image(image = favicon, x = 2022, y = 58, size = 4)
    }
  }
  
  mtext(text = "Number of pre-registrations", side = 2, line = 2.2, cex = 1.4, font = 2)
  
  dev.off()
  
}
