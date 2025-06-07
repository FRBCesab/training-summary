students_evolution <- function(inset = TRUE) {
  gsheet_url <- paste0(
    "https://docs.google.com/spreadsheets/d/",
    "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs",
    "/edit?usp=sharing"
  )

  x <- gsheet::gsheet2tbl(gsheet_url)

  colnames(x) <- tolower(colnames(x))

  # x <- x[x$"year" < 2025, ]

  y <- x[x$"selected" == 1, ]

  students <- tapply(
    y$"fullname",
    list(y$"year", y$"course"),
    length,
    default = 0
  ) |>
    as.data.frame()
  students <- data.frame("year" = as.numeric(rownames(students)), students)

  colnames(students) <- colnames(students) |>
    tolower() |>
    gsub("\\.", "-", x = _)

  ordre <- c(
    "year",
    "reproducibility",
    "theoretical-models",
    "literature-synthesis",
    "network-analysis",
    "biodiversity-data",
    "artificial-intelligence"
  )

  students <- students[, ordre]
  students <- rbind(students[1, ], students)

  students[1, 1] <- 2018
  students[1, 2] <- 0

  rownames(students) <- NULL

  years <- range(students$year)
  years[2] <- years[2] + 0.2

  filename <- ifelse(
    inset,
    "number_of_trainees-1.png",
    "number_of_trainees-0.png"
  )

  png(
    filename = here::here("figures", filename),
    width = 16,
    height = 8,
    units = "in",
    pointsize = 16,
    res = 600
  )

  par(
    mar = c(1.8, 3.7, 0.5, 0.5),
    mgp = c(0, 0.3, 0),
    family = "serif",
    new = FALSE
  )

  plot(
    0,
    pch = 19,
    xlim = years,
    ylim = c(0, sum(students[, -1]) + 10),
    xlab = "",
    cex = 1.3,
    lwd = 1.75,
    ylab = "",
    axes = FALSE,
    col = "#c23438",
    type = "n"
  )

  grid(lwd = 0.5)
  box()
  axis(1, lwd = 0, lwd.ticks = 0, cex.axis = 1.2)
  axis(2, lwd = 0, lwd.ticks = 0, cex.axis = 1.2, las = 1)

  y_bot <- rep(0, nrow(students))

  for (i in 2:ncol(students)) {
    if (i == 2) {
      rows <- 1:nrow(students)
    }
    if (i == 3) {
      rows <- 4:nrow(students)
    }
    if (i == 4) {
      rows <- 4:nrow(students)
    }
    if (i == 5) {
      rows <- 6:nrow(students)
    }
    if (i == 6) {
      rows <- 6:nrow(students)
    }
    if (i == 7) {
      rows <- 7:nrow(students)
    }

    x <- students[, c(1, 2:i)]

    x_val <- x[, "year"]

    y_up <- y_bot + cumsum(x[, i])

    polygon(
      x = c(x_val, rev(x_val)),
      y = c(y_up, rev(y_bot)),
      border = "#ffffff",
      col = color_scheme()[colnames(x)[i]],
      lwd = 2
    )

    y_at <- mean(c(max(y_bot), max(y_up)))

    y_bot <- y_up

    favicon <- png::readPNG(here::here(
      "images",
      "favicons",
      paste0(colnames(students)[i], ".png")
    ))

    add_image(
      image = favicon,
      x = max(students$year) + 0.23,
      y = y_at,
      size = 2
    )
  }

  # text(2022, 34, "Reproducibility", col = "white", font = 2)
  # text(2023, 108, "Theoretical models", col = "white", font = 2, srt = 17)
  # text(2023.4, 155, "Literature synthesis", col = "white", font = 2, srt = 25)

  students <- tapply(y$"fullname", y$"year", length, default = 0)

  years <- as.numeric(names(students))
  students <- cumsum(students)

  points(years, students, type = "p", pch = 19, cex = 1.65, col = "#ffffff")

  points(years, students, type = "p", pch = 19, cex = 1.3, col = "#000000")

  text(years, students, students, pos = 3, cex = 1, font = 2)

  mtext(
    text = "Cumulative number of trainees",
    side = 2,
    line = 2.2,
    cex = 1.4,
    font = 2
  )

  if (inset) {
    y <- y$"status"
    y <- gsub("Etudiant\\.e", "PhD", y)
    y <- gsub("Doctorant\\.e", "PhD", y)
    y <- gsub("Chercheur\\.se", "Researchers", y)
    y <- gsub("Post-doctorant\\.e", "Postdocs", y)
    y <- gsub("Ingénieur\\.e", "Engineers", y)
    y <- gsub("Autre", "Others", y)

    y <- sort(table(y))

    par(new = TRUE, fig = c(0.075, 0.72, 0.325, 0.97))

    par(mar = c(0, 0, 0.5, 0), mgp = c(0, -1, 0), family = "serif")

    plot(
      0,
      type = "n",
      xlim = c(0, max(y) + 10),
      ylim = c(0.5, 5.5),
      axes = FALSE,
      ann = FALSE
    )
    # box(which = "figure")

    axis(3, at = seq(0, max(y), by = 20), lwd = 0, lwd.ticks = 0)

    for (i in 1:length(y)) {
      rect(0, i - 0.33, y[i], i + 0.33, col = "#4a6741", border = "#374f2f")
      text(y[i], i, names(y)[i], pos = 4, font = 2)
    }

    abline(v = seq(0, max(y), by = 20), col = "#ffffff", lty = 3, lwd = 0.25)
  }

  dev.off()
}
