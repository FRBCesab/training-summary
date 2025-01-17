<!-- Logo & Title -->

<h1 align="center">
  <br>
  <img src="images/readme/logo-readme.png" alt="Logo" width="200">
  <br>
  FRB-CESAB Courses
  <br>
  Summary of the training program of the FRB-CESAB
  <br>
</h1>


<!-- View presentation -->

<br>
<p align="center">
  <a href="https://frbcesab.github.io/training-summary" target="_blank"><b>View presentation</b></a>
</p>
<br>


<!-- Badges -->

<p align="center">

  <!-- Quarto -->
  <a href="https://quarto.org/">
    <img src="https://img.shields.io/badge/Made%20with-Quarto-blue.svg" alt="Quarto">
  </a>
  
  <!-- License -->
  <a href="https://choosealicense.com/licenses/cc-by-4.0/">
    <img src="https://img.shields.io/badge/License-CC%20BY%204.0-green.svg" alt="License CC BY 4.0">
  </a>
  
  <br/>
  
  <!-- Quarto render -->
  <a href="https://github.com/frbcesab/training-summary/actions/workflows/quarto-render.yml">
    <img src="https://github.com/frbcesab/training-summary/actions/workflows/quarto-render.yml/badge.svg" alt="GHA render">
  </a>
  
  <!-- GitHub deployment -->
  <a href="https://github.com/frbcesab/training-summary/actions/workflows/pages/pages-build-deployment">
    <img src="https://github.com/frbcesab/training-summary/actions/workflows/pages/pages-build-deployment/badge.svg" alt="GHA deploy">
  </a>
</p>


<!-- Table of content -->

<p align="center">
  <a href="#overview">Overview</a> •
  <a href="#usage">Usage</a> •
  <a href="#citation">Citation</a> •
  <a href="#contributing">Contributing</a> •
  <a href="#acknowledgments">Acknowledgments</a>
</p>

<br>


## Overview

This repository contains files used to generate the online presentation [FRB-CESAB Courses](https://frbcesab.github.io/training-summary) based on [Quarto](https://quarto.org/) and the [RevealJS](https://quarto.org/docs/presentations/revealjs/) framework.


## Usage

If you want to render the slides locally, you need to install the following tools:

- [`R`](https://cran.r-project.org/) and [`RStudio Desktop`](https://posit.co/download/rstudio-desktop/)
- [`Quarto CLI`](https://quarto.org/docs/get-started/)

[Fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/fork-a-repo) this repository and [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) your copy.

Then, open the `index.qmd` in RStudio and click on the button **Render**. This will create an `index.html` containing your presentation. Open this `index.html` in your favorite web browser to access slides.

To understand the file organisation of this repository, please read this [Wiki](https://github.com/rdatatoolbox/training-summary/wiki).


## Citation

Casajus N (2024) _FRB-CESAB Courses: Summary of the training program of the FRB-CESAB_. Online presentation available at <https://github.com/frbcesab/training-summary/>.


## Contributing

All types of contributions are encouraged and valued. For more information, check out our [Contributor Guidelines](https://github.com/frbcesab/training-summary/blob/main/CONTRIBUTING.md).

Please note that the `training-summary` project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.


## Acknowledgments

This project has been developed for the [FRB-CESAB](https://www.fondationbiodiversite.fr/en/about-the-foundation/le-cesab/) training courses [program](https://frbcesab.github.io/content/courses.html).
