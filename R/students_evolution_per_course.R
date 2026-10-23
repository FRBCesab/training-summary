students_evolution_per_course <- function() {
  gsheet_url <- paste0(
    "https://docs.google.com/spreadsheets/d/",
    "1flVFi9SEY8kXXG125jTeh8mR0FWyf34_9ckzZ3nzKEs",
    "/edit?usp=sharing"
  )

  x <- gsheet::gsheet2tbl(gsheet_url)

  colnames(x) <- tolower(colnames(x))

  x <- x[x$"selected" == 1, ]

  courses <- c(
    "Reproducibility",
    "Theoretical models",
    "Literature synthesis",
    "Network analysis",
    "Biodiversity Data",
    "Artificial intelligence"
  )

  courses_fr <- c(
    "RECHERCHE REPRODUCTIBLE",
    "MODÉLISATION THÉORIQUE",
    "REVUE SYSTÉMATIQUE",
    "RÉSEAUX D'INTERACTION",
    "DONNÉE DE BIODIVERSITÉ",
    "INTELLIGENCE ARTIFICIELLE"
  )

  slugs <- tolower(gsub(" ", "-", courses))

  for (k in 1:length(courses)) {
    y <- x[x$"course" == courses[k], ]

    y <- y$"status"
    y <- gsub("Etudiant\\.e", "Doctorant·e·s", y)
    y <- gsub("Doctorant\\.e", "Doctorant·e·s", y)
    y <- gsub("Chercheur\\.se", "Chercheur·euse·s", y)
    y <- gsub("Post-doctorant\\.e", "Post-doctorant·e·s", y)
    y <- gsub("Ingénieur\\.e", "Ingénieur·e·s", y)
    y <- gsub("Autre", "Autres", y)

    y <- sort(table(y))
    y <- round(100 * y / sum(y))

    if (!("Autres" %in% names(y))) {
      y <- c(y, "Autres" = 0)
    }

    y <- y[c(
      "Autres",
      "Chercheur·euse·s",
      "Ingénieur·e·s",
      "Post-doctorant·e·s",
      "Doctorant·e·s"
    )]

    names(y) <- paste0(names(y), " (", y, " %)")

    png(
      filename = here::here(
        "figures",
        paste0("number_of_trainees_", slugs[k], ".png")
      ),
      width = 16,
      height = 8,
      units = "in",
      pointsize = 16,
      res = 600
    )

    par(mar = c(0, 0, 0.5, 0), mgp = c(0, -1, 0), family = "serif")

    plot(
      0,
      type = "n",
      xlim = c(0, 87),
      ylim = c(0.5, 6),
      axes = FALSE,
      ann = FALSE
    )

    # axis(3, at = seq(0, 100, by = 20), lwd = 0, lwd.ticks = 0)

    for (i in 1:length(y)) {
      rect(
        0,
        i - 0.45,
        y[i],
        i + 0.45,
        col = color_scheme()[slugs[k]],
        border = color_scheme()[slugs[k]]
      )

      text(y[i], i, names(y)[i], pos = 4, font = 2, cex = 2)
    }

    abline(v = seq(0, 80, by = 20), col = "#ffffff", lty = 3, lwd = 0.25)

    text(0, 6, courses_fr[k], cex = 2.5, pos = 4, font = 2)

    favicon <- png::readPNG(here::here(
      "images",
      "favicons",
      paste0(slugs[k], ".png")
    ))

    add_image(
      image = favicon,
      x = 82,
      y = 1.4,
      size = 15
    )

    dev.off()
  }
}
