knitr::purl("Presentaciones/Clase3-ANOVA.Rmd", 
            output = "Clase3-ANOVA.R", documentation = 0)

pagedown::chrome_print("Presentaciones/Clase3-ANOVA.html", output = "Clase3-ANOVA.pdf")

pagedown::chrome_print(
  input = "Presentaciones/Clase1-Introducción-a-R.html",
  output = "Presentaciones/Clase1-Introducción-a-R.pdf"
)
