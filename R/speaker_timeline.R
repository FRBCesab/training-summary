speaker_timeline <- function() {
  gsheet_url <- paste0(
    "https://docs.google.com/spreadsheets/d/",
    "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs/edit?",
    "gid=335089110#gid=335089110"
  )

  x <- gsheet::gsheet2tbl(gsheet_url) |>
    as.data.frame()

  colnames(x) <- tolower(colnames(x))

  courses <- c(
    "Reproducibility",
    "Theoretical models",
    "Literature synthesis",
    "Network analysis",
    "Biodiversity Data",
    "Artificial intelligence"
  )

  x$course <- factor(x$course, levels = courses)
  x$n_years <- apply(x[, grep("[0-9]{4}", colnames(x))], 1, sum)
  x$older <- apply(x[, grep("[0-9]{4}", colnames(x))], 1, function(x) {
    min(which(x == 1))
  })

  x <- x[with(x, order(course, older, -n_years, fullname)), ]

  courses_fr <- c(
    "RECHERCHE REPRODUCTIBLE",
    "MODÉLISATION THÉORIQUE",
    "REVUE SYSTÉMATIQUE",
    "RÉSEAUX D'INTERACTION",
    "DONNÉE DE BIODIVERSITÉ",
    "INTELLIGENCE ARTIFICIELLE"
  )

  slugs <- tolower(gsub(" ", "-", courses))

  speakers <- x[!duplicated(x$fullname), "fullname"]
  speakers <- rev(speakers)

  years <- colnames(x)[grep("[0-9]{4}", colnames(x))]
  years <- sort(as.numeric(years))

  png(
    filename = here::here("figures", "list_of_speakers.png"),
    width = 16,
    height = 8,
    units = "in",
    pointsize = 16,
    res = 600
  )

  par(
    mar = c(0, 0, 0, 0),
    mgp = c(0, -1, 0),
    family = "serif",
    xaxs = "i",
    yaxs = "i"
  )

  plot(
    0,
    type = "n",
    xlim = c(-3, length(years) * length(courses) + length(courses)),
    ylim = c(0, length(speakers) + 5),
    axes = FALSE,
    ann = FALSE
  )

  for (i in 1:length(speakers)) {
    lines(
      c(0.25, length(years) * length(courses) + length(courses) - .25),
      rep(i, 2),
      col = "#aaaaaa",
      lty = 3,
      lwd = 0.25
    )
  }

  for (i in 1:length(courses)) {
    x_start <- length(years) * (i - 1) + (i - 1)

    for (j in 1:length(years)) {
      lines(
        rep(x_start + j, 2),
        c(0.25, length(speakers) + 1),
        col = "#aaaaaa",
        lty = 3,
        lwd = 0.25
      )
    }
  }

  for (i in 1:length(speakers)) {
    y <- x[x$fullname == speakers[i], ]

    text(0.5, i, speakers[i], pos = 2, cex = 0.5)

    for (j in 1:nrow(y)) {
      z <- y[j, ]

      course_id <- which(courses == z$course)

      annees <- z[, as.character(years)]
      annees <- unlist(annees, use.names = TRUE)
      annees <- which(annees == 1)

      for (k in 1:length(annees)) {
        x_start <- length(years) * (course_id - 1) + (course_id - 1)

        rect(
          (x_start + annees[k]) - .5,
          i - .5,
          (x_start + annees[k]) + .5,
          i + .5,
          border = color_scheme()[slugs[course_id]],
          col = color_scheme()[slugs[course_id]],
          lwd = .5
        )

        lines(
          c((x_start + annees[k]) - .5, (x_start + annees[k]) + .5),
          rep(i - .5, 2),
          col = "white",
          lwd = 1
        )

        lines(
          c((x_start + annees[k]) - .5, (x_start + annees[k]) + .5),
          rep(i + .5, 2),
          col = "white",
          lwd = 1
        )

        lines(
          rep((x_start + annees[k]) - .5, 2),
          c(i - .5, i + .5),
          col = "white",
          lwd = 1
        )

        lines(
          rep((x_start + annees[k]) + .5, 2),
          c(i - .5, i + .5),
          col = "white",
          lwd = 1
        )
      }
    }
  }

  abline(
    h = seq(0.5, length(speakers) + 0.5),
    v = seq(0.5, length(years) * length(courses) + length(courses) + 0.5),
    col = "#ffffff",
    lty = 3,
    lwd = 0.25
  )

  for (i in 1:length(courses)) {
    x_start <- length(years) * (i - 1) + (i - 1)

    for (j in 1:length(years)) {
      text(x_start + j, length(speakers) + 0.5, years[j], cex = 0.5, pos = 3)
    }

    lines(
      c(x_start + 0.5, x_start + j + 0.5),
      rep(length(speakers) + 3, 2),
      lwd = 1.5
    )

    text(
      mean(c(x_start + 0.5, x_start + j + 0.5)),
      length(speakers) + 4,
      courses_fr[i],
      font = 2,
      cex = 0.5
    )
  }

  text(
    0.5,
    length(speakers) + 1.83,
    paste("n =", length(speakers)),
    cex = 0.5,
    pos = 2,
    font = 4
  )

  dev.off()
}
