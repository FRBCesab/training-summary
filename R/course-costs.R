course_costs <- function() {
  
  gsheet_url <- paste0("https://docs.google.com/spreadsheets/d/", 
                       "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs", 
                       "/edit?gid=666902769&usp=sharing")
  
  x <- gsheet::gsheet2tbl(gsheet_url) |> 
    as.data.frame()
  
  x <- x[-nrow(x), ]
  
  x <- x[order(x$"Total"), ]
  
  for (i in 3:ncol(x)) {
    x[ , i] <- gsub("\\,", ".", x[ , i])
    x[ , i] <- gsub("\\s| ", "", x[ , i])
    x[ , i] <- gsub("€", "", x[ , i])
    x[ , i] <- as.numeric(x[ , i])
  }
  

  x[ , 1] <- x[ , 1] |>
    tolower() |>
    gsub("\\s", "-", x = _)
  
  col_s <- c("#525214", "#6c6c2e", "#815a33", "#00407f", "#004c99", "#0059b2", "#338181")
  
  png(filename  = here::here("figures", "course_costs.png"), 
      width     = 16, 
      height    = 8, 
      units     = "in", 
      pointsize = 16,
      res       = 600)
  
  par(mar = c(1.8, 4.0, 0.5, 0.5), mgp = c(0, 0.3, 0), family = "serif", new = FALSE)
  
  plot(0, pch = 19, xlim = c(0, 8000),
       ylim = c(0.7, nrow(x) + 0.45), xlab = "", cex = 1.3, lwd = 1.75,
       ylab = "", axes = FALSE, col = "#c23438", type = "n")
  
  grid(lwd = 0.5)
  box()
  axis(1, at = seq(0, 8000, 1000), labels = paste(seq(0, 8000, 1000), "€"), lwd = 0, lwd.ticks = 0, cex.axis = 1.2)
  
  # axis(2, lwd = 0, lwd.ticks = 0, cex.axis = 1.2, las = 1)
  
  for (i in 1:nrow(x)) {
    
    x_lft <- 0
    x_rgt <- 0
    
    for (j in 7:13) {
      
      x_rgt <- x_rgt + x[i, j]
      rect(x_lft, i - 0.33, x_rgt, i + 0.33, border = "white", col = col_s[j - 6])
      
      if (x_rgt > x_lft) {
        text(x_lft + (x_rgt - x_lft) / 2, i + 0.29, colnames(x)[j], cex = 0.9, pos = 3, font = 2)
        
        if (x[i, j] > 200) {
          text(x_lft + (x_rgt - x_lft) / 2, i + 0.33, paste(x[i, j], "€"), cex = 0.65, col = "white", font = 2, pos = 1)
          
        }
      }
      
      x_lft <- x_lft + x[i, j]
    }
    
    favicon <- png::readPNG(here::here("images", "favicons", 
                                       paste0(x[i, 1], ".png")))
    
    par(xpd = TRUE)
    add_image(image = favicon, x = -650, y = i, size = 6)
    par(xpd = FALSE)
  }
  
  dev.off()
}
