knitr::purl("Presentaciones/Clase4-KW-DUNN.Rmd", 
            output = "Clase4-KW-DUNN.R", documentation = 0)

pagedown::chrome_print("Presentaciones/Clase4-KW-DUNN.html", output = "Clase4-KW-DUNN.pdf")

pagedown::chrome_print(
  input = "Presentaciones/Clase1-Introducción-a-R.html",
  output = "Presentaciones/Clase1-Introducción-a-R.pdf"
)
