# Force install any missing packages before loading app
pkgs <- c("memoise", "cachem", "hoopR", "stringdist", "shinyWidgets",
          "shinydashboard", "httr2", "plotly", "DT", "glue",
          "lubridate", "scales")

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

source("global.R")
shinyApp(ui = ui, server = server)