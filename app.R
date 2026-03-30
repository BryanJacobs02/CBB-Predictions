# Ensure required packages are installed
if (!requireNamespace("memoise", quietly = TRUE)) install.packages("memoise")
if (!requireNamespace("cachem",  quietly = TRUE)) install.packages("cachem")

source("global.R")
shinyApp(ui = ui, server = server)

source("global.R")
shinyApp(ui = ui, server = server)