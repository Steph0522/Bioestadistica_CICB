knitr::purl("Presentaciones/Clase1-Introducción-a-R.Rmd", 
            output = "Clase1-Introducción-a-R.R", documentation = 0)

pagedown::chrome_print("Presentaciones/Clase1-Introducción-a-R.html", output = "Clase1-Introducción-a-R.pdf")

pagedown::chrome_print(
  input = "Presentaciones/Clase1-Introducción-a-R.html",
  output = "Presentaciones/Clase1-Introducción-a-R.pdf"
)
