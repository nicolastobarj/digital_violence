# Preparación ----

pacman::p_load(tidyverse, #Data depuration
               sjPlot, #Bivariate Analysis
               summarytools, #Descriptive tables 
               labelled, #Label data
               haven, #Label data
               knitr, #Rendering
               kableExtra #Rendering
)

#load("~/GitHub/milenio_nudos/digital_violence/data/raw_data/simce2m2023.RData")
segundo <- data
rm(data)
#rbd <- read.csv(file = "data/raw_data/simce2m2023_rbd_público_final.csv", sep = ";")

segundo <- merge(segundo,rbd, by="rbd")

# Recodificación ----

# Convertir 0 y 99 a NA y asegurar data_type numérico
segundo <- segundo %>%
  mutate(across(starts_with("cest_p11") | starts_with("cest_p12") | starts_with("cest_p27"), ~ 
                  ifelse(.x %in% c(0, 99), NA, as.numeric(.x))))

#Revisar si funciona
summary(segundo %>% 
          select(starts_with("cest_p11") | starts_with("cest_p12")))

# Dicotomizar género, 0 y 99 a NA y asegurar data_type numérico
segundo <- segundo %>%
  mutate(cest_p01 = case_when(
    cest_p01 %in% c(3,4,5) ~ 3,
    cest_p01 %in% c(0,99) ~ NA_integer_,
    cest_p01 == 1 ~ 1,
    cest_p01 == 2 ~ 2
    )
  )

# Agrupar Tipo de escuela
segundo <- segundo %>%
  mutate(rbd_financiamiento = case_when(
    cod_depe2 %in% c(1,4,5) ~ 1,
    cod_depe2 == 2 ~ 2,
    cod_depe2 == 3 ~ 3,
    TRUE ~ NA_real_
  ))
  

#Revisar si funciona
count(segundo,cest_p01)

# Creación de variables estudiantes ----

segundo <- segundo %>%
  mutate(
    #Víctima Violencia Offline
    vict_off = ifelse(
      as.numeric(cest_p11_01) %in% c(1,2) &
      as.numeric(cest_p11_02) %in% c(1,2) &
      as.numeric(cest_p11_05) %in% c(1,2) &
      as.numeric(cest_p11_07) %in% c(1,2) &
      as.numeric(cest_p11_08) %in% c(1,2) &
      as.numeric(cest_p11_10) %in% c(1,2),
      0,1),
    #Víctima Violencia Online
    vict_onl = ifelse(
      as.numeric(cest_p11_03) %in% c(1,2) &
      as.numeric(cest_p11_04) %in% c(1,2),
      0,1),
    #Testigo Violencia en RRSS
    testigo_rrss = ifelse(
      as.numeric(cest_p12_01) %in% c(1,2) &
      as.numeric(cest_p12_02) %in% c(1,2) &
      as.numeric(cest_p12_03) %in% c(1,2) &
      as.numeric(cest_p12_04) %in% c(1,2) &
      as.numeric(cest_p12_05) %in% c(1,2) &
      as.numeric(cest_p12_06) %in% c(1,2),
      0,1),
    #Indice NUDOS
    adopcion_digital = rowMeans(across(starts_with("cest_p27")), na.rm = TRUE)
  )

# Creación de variables escuela -----
segundo <- segundo %>%
  group_by(rbd) %>%
  mutate(rbd_vict_off = mean(vict_off, na.rm = TRUE),
         rbd_vict_onl = mean(vict_onl, na.rm = TRUE),
         rbd_testigo_rrss = mean(testigo_rrss, na.rm = TRUE),
         rbd_adopcion_digital = mean(adopcion_digital, na.rm = TRUE)) %>%
  ungroup()


# Etiquetamiento de valores -----

# Define the value labels
value_labels <- c(
  "Nunca" = 1,
  "Pocas veces" = 2,
  "Algunas veces" = 3,
  "La mayoría de las veces" = 4
)

value_labels2 <- c(
  "No lo describe" = 1,
  "Lo describe poco" = 2,
  "Lo describe bastante" = 3,
  "Lo describe completamente" = 4
)

# Apply labels to the specified columns
segundo <- segundo %>%
  mutate(
    across(
    .cols = starts_with(c("cest_p11", "cest_p12")),
    .fns = ~ labelled(.x, labels = value_labels)),
    
    across(
      .cols = starts_with("cest_p27"),
      .fns = ~ labelled(.x, labels = value_labels2))
    )

#Género
segundo <- segundo %>%
  mutate(genero = factor(cest_p01,
                         levels = c(1,2,3),
                         labels = c("Masculino", "Femenino", "Otro")))

#Tipo Escuela
segundo <- segundo %>%
  mutate(rbd_financiamiento = factor(rbd_financiamiento,
                                     levels = c(1,2,3),
                                     labels = c("Público", "P. Subvencionado", "P. Pagado")
  ))


# Etiquetamiento de columnas -----
var_label(segundo) <- list(
  genero = "Género",
  cest_p11_01 = "Se han burlado de mí o me molestan.",
  cest_p11_02 = "Me han ignorado, aislado o me han excluido.",
  cest_p11_03 = "Me han hecho sentir mal en redes sociales o internet (por ejemplo, me han insultado, amenazado, etc.).",
  cest_p11_04 = "Han compartido fotos, videos o información mía por redes sociales o internet sin mi consentimiento.",
  cest_p11_05 = "Me han golpeado.",
  cest_p11_06 = "Me han roto o destruido cosas personales a propósito.",
  cest_p11_07 = "Me han amenazado.",
  cest_p11_08 = "Me han obligado a realizar cosas que no quiero.",
  cest_p11_09 = "Me han robado cosas.",
  cest_p11_10 = "He dejado de asistir al colegio porque siento miedo de que alguien me haga daño.",
  cest_p12_01 = "Insultan, ofenden o ridiculizan a otro(a) estudiante con mensajes o comentarios.",
  cest_p12_02 = "Amenazan o chantajean a otro(a) estudiante con mensajes o comentarios.",
  cest_p12_03 = "Envían contenidos sexuales, tales como imágenes, comentarios o mensajes, para molestar a otro(a) estudiante.",
  cest_p12_04 = "Se comparten mentiras sobre otro(a) estudiante para perjudicarlo(a) o molestarlo(a).",
  cest_p12_05 = "Comparten secretos o cosas íntimas de alguien sin su consentimiento.",
  cest_p12_06 = "Se hacen pasar por otro(a) estudiante para decir o hacer cosas pesadas o perjudiciales.",
  cest_p27_01 = "En el colegio nos motivan a utilizar herramientas tecnológicas.",
  cest_p27_02 = "Ocupamos la tecnología frecuentemente en el colegio para aprender.",
  cest_p27_03 = "Para mis compañeros(as) es importante saber usar los computadores para aprender.",
  cest_p27_04 = "Los profesores y profesoras nos enseñan a utilizar herramientas tecnológicas.",
  cest_p27_05 = "En el colegio se preocupan de tener los computadores en buen estado.",
  cest_p27_06 = "El colegio cuenta con internet de buena calidad.",
  vict_off = "Víctima de Violencia Offline",
  vict_onl = "Víctima de Violencia en Redes Sociales",
  testigo_rrss = "Testigo de Violencia en Redes Sociales",
  adopcion_digital = "Percepción de Adopción Digital en la Escuela",
  rbd_vict_off = "Escuela: Proporción de Estudiantes Víctimas de Violencia Offline",
  rbd_vict_onl = "Escuela: Proporción de Estudiantes Víctimas de Violencia Online",
  rbd_testigo_rrss = "Escuela: Proporción de Estudiantes Testigos de Violencia en Redes Sociales",
  rbd_adopcion_digital = "Escuela: Nivel de Adopción Digital",
  rbd_financiamiento = "Escuela: Tipo de financiamiento"
)

