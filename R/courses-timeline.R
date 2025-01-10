courses_timeline <- function() {
  
  dat <- readxl::read_xlsx(here::here("data", 
                                      "formations-suivi-inscriptions.xlsx"))
  
  start_date <- tapply(dat$"year", dat$"slug", 
                       function(x) min(x, na.rm = TRUE)) |> 
    data.frame()
  
  start_date$"slug" <- rownames(start_date)
  
  rownames(start_date) <- NULL
  colnames(start_date)[1] <- "start"
  
  end_date   <- tapply(dat$"year", dat$"slug", 
                       function(x) max(x, na.rm = TRUE)) |> 
    data.frame()
  
  end_date$"slug" <- rownames(end_date)
  
  rownames(end_date) <- NULL
  colnames(end_date)[1] <- "end"
  
  extent <- merge(start_date, end_date, by = "slug")
  
  label <- dat[ , 1:2]
  label <- label[!duplicated(label$"slug"), ]
  
  extent <- merge(extent, label, by = "slug", all = FALSE)

  extent <- extent[order(extent$start, decreasing = TRUE), ]

  
  x_range <- range(dat$"year")
  x_range[1] <- x_range[1] - 0.5
  
  
  png(filename  = here::here("figures", "courses-timeline.png"), 
      width     = 16, 
      height    = 8, 
      units     = "in", 
      pointsize = 16,
      res       = 600)
  
  par(mar = c(1.8, 0.5, 0.5, 0.5), mgp = c(0, 0.3, 0), family = "serif")
  
  plot(0, xlim = x_range, ylim = c(0.7, nrow(extent) + 0.3), xlab = "", 
       ylab = "", axes = FALSE)
  
  grid(lwd = 0.5)
  box()
  axis(1, lwd.ticks = 0, cex.axis = 1.2)
  
  for (i in 1:nrow(extent)) {
    
    if (extent[i, "end"] == max(extent[ , "end"])) {
      
      rect(xleft   = extent[i, "start"] - 0.05, 
           ybottom = i - 0.25, 
           xright  = extent[i, "end"] + 0.05, 
           ytop    = i + 0.25,
           border  = "#ffffff",
           col     = color_scheme()[extent[i, "slug"]],
           lwd     = 3)
      
      rect(xleft   = extent[i, "start"] - 0.05, 
           ybottom = i - 0.25, 
           xright  = extent[i, "end"] + 0.05, 
           ytop    = i + 0.25,
           border  = color_scheme()[extent[i, "slug"]],
           col     = NA)
      
    } else {
      
      rect(xleft   = extent[i, "start"] - 0.05, 
           ybottom = i - 0.25, 
           xright  = extent[i, "end"] + 0.05, 
           ytop    = i + 0.25,
           border  = NA,
           col     = color_scheme()[extent[i, "slug"]],
           lwd     = 3)
      
      rect(xleft   = extent[i, "end"] + 0.05, 
           ybottom = i - 0.25, 
           xright  = max(extent[ , "end"]) + 0.05, 
           ytop    = i + 0.25,
           border  = "white",
           col     = color_scheme()[extent[i, "slug"]],
           density = 20,
           angle   = 45,
           lwd     = 2)
      
      lines(rep(extent[i, "end"] + 0.05, 2), c(i - 0.25, i + 0.25), col = "white", 
            lwd = 1)
      
      rect(xleft   = extent[i, "start"] - 0.05, 
           ybottom = i - 0.25, 
           xright  = max(extent[ , "end"]) + 0.05, 
           ytop    = i + 0.25,
           border  = "#ffffff",
           col     = NA,
           lwd     = 3)
      
      rect(xleft   = extent[i, "start"] - 0.05, 
           ybottom = i - 0.25, 
           xright  = max(extent[ , "end"]) + 0.05, 
           ytop    = i + 0.25,
           border  = color_scheme()[extent[i, "slug"]],
           col     = NA)
    }
    
    
    
    favicon <- png::readPNG(here::here("images", "favicons", 
                                       paste0(extent[i, "slug"], ".png")))
    
    add_image(image = favicon, x = 2018.6, y = i, size = 4)
    
    col_or <- ifelse(extent[i, "start"] == 2019, "white", "black")
    
    text(x = 2019, y = i, label = extent[i, "course"], pos = 4, font = 2, col = col_or)

  }

  dev.off()
}
