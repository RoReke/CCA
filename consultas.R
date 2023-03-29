# # consulta sebastian
# Hola, qué tal?
#
#   En el minuto 7:28 de la clase mencionada me salta un error al correr el código y no estoy entendiendo porqué dado que copié lo mismo (quiero creer) que lo expuesto en el video:
#
#   Sex_Nivel <- table(Ejemplo$Nivel.Educativo,Ejemplo$Sexo)
ejemplo <- read.csv2("data/Datos barras agrupadas.csv", header = TRUE)
ejemplo
sex_nivel <- table(ejemplo$Nivel.Educativo, ejemplo$Sexo)
sex_nivel
