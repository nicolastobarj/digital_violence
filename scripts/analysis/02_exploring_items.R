# Preparacion ----

pacman::p_load(tidyverse, #Data depuration
               sjPlot, #Bivariate Analysis
               summarytools, #Descriptive tables 
               labelled, #Label data
               knitr, #Rendering
               kableExtra, #Rendering
               gtsummary, #Summarise data
               summarytools, #Summarise data
               stargazer, #Summarise data
               corrplot, #Correlation
               ggcorrplot #Correlation
)

source("scripts/proccesing/01_proc_data.R")

#Seleccionar variables de interés
data <- segundo %>%
  select(idAlumno, rbd, genero, starts_with("cest_p11"), starts_with("cest_p12"))

# Análisis Descriptivo ----
# MIRADA GENERAL 
# Niveles de Violencia por individuo son bajos. Promedian menos que 2 las variables.
# 35% sufre violencia offline recurrente. En cambio disminuye a un 13% online.
# Aunque solo un 13% es víctima de violencia online, 45% es testigo de ella.
# A nivel escuela se repute el patrón. 

#Op. 1
summary(data)|>
  kable()


#EXPLORACION PROMEDIO ITEMS
# Las agresiones verbales (Burlas y aislaciones) son las que más se sufren
# Luego las de redes sociales, que están sobre la violencia directa (Golpes, amenazas, abusos)
# En ser testigo los níveles son mayores, pero hay mayor dispersión. No hay patrones.
data %>%
  summarise(across(starts_with(c("cest_p11", "cest_p12")),
                   ~ mean(as.numeric(.), na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "mean_value") %>%
  mutate(category = case_when(
    grepl("cest_p12", variable) ~ "Testigo RR.SS.",
    variable %in% c("cest_p11_03", "cest_p11_04") ~ "Víctima RR.SS",
    TRUE ~ "Víctima Offline")) %>%
  ggplot(aes(x = mean_value, y = variable, color = category)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Víctima RR.SS" = "#E41A1C", "Testigo RR.SS." = "#377EB8",
                                "Víctima Offline" = "seagreen4")) +
  labs(title = "Promedio por ítem",
       x = "Promedio (escala 1-4)",
       color = "Tipo de violencia",
       caption = "Fuente: SIMCE 2023") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

##PROMEDIOS POR GÉNERO
data %>%
  select(genero, starts_with("cest_p11")) %>%
  mutate(genero = as.factor(genero)) %>%  # Asegurar que género es factor
  pivot_longer(
    cols = -genero,
    names_to = "item",
    values_to = "valor"
  ) %>%
  group_by(genero, item) %>%
  summarise(
    promedio = mean(as.numeric(valor), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(item = gsub("cest_p11_", "Ítem ", item))%>%
  drop_na()%>%
  
ggplot(aes(x = item, y = promedio, fill = genero)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7,
    alpha = 0.8
  ) +
  geom_text(
    aes(label = round(promedio, 2)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(
    values = c("Masculino" = "#E41A1C", "Femenino" = "#377EB8", "Otro" = "seagreen4"),  # Colores distintivos
    name = "Género"
  ) +
  labs(
    title = "Promedio de respuestas por ítem y género",
    subtitle = "Variables que miden violencia escolar (cest_p11)",
    x = "Ítems",
    y = "Promedio (escala Likert)",
    caption = "Nota: Valores más altos indican mayor frecuencia de violencia"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#CORRELACIONES ENTRE ÍTEMS
#Los ítems están asociados positivamente entre sí, todos >.30. 
# La correlación es mayor en Ser testigo que en violencia.
# Los índicadores de rr.ss son los que correlacionan más fuerte en batería víctima.

# Select and convert variables
cor_matrix <- data %>%
  select(starts_with(c("cest_p11", "cest_p12"))) %>%
  mutate(across(everything(), as.numeric)) |>
  cor(use = "pairwise.complete.obs")
                     
# Create annotated heatmap
corrplot(
  cor_matrix,
  method = "color",
  col = colorRampPalette(c("#e41a1c","#f7fcf0","#2b8cbe"))(100),
  tl.col = "black",
  tl.cex = 0.7,
  tl.pos = "lt",
  cl.ratio = 0.2,
  addCoef.col = "black",
  number.cex = 0.6,
  addrect = 2,              # Draw rectangles around clusters
  rect.col = "black",
  rect.lwd = 2,
  mar = c(0,0,2,0),
  title = "Correlación entre dimensiones de violencia escolar"
)

#FACTORIAL EXPLORATORIO
#Para entender bien las dimensiones del estudio se hace un modelo exploratorio con todo.
#Todas las variables de testigo juntas, con alta consistencia.
#Las varianles de violencia online correlacionan consistentemente con violencia directa.
#Víctima de daño a la propiedad y de insultos o burlas se mueven por separado
#¿Está la violencia online más cerca de la violencia directa que de la verbal?
#Una vez que se separa por tres factores la batería de víctima, rr.ss vuelve a relacionarse con amenaza

data|> 
  select(starts_with("cest_p11"), starts_with("cest_p12"))|>
  tab_fa()

data|> 
  select(starts_with("cest_p11"))|>
  tab_fa(nmbr.fctr = 3)


#REVISION DE CONSISTENCIA INTERNA
#Se nota una alta consistencia en violencia offline, tanto si se elimina robo y rr.ss como si no.
#Robo discrimina menos que el resto. Mejor eliminar para tener más variabilidad en offline.
#La escala de testigo rr.ss también tiene una alta consistencia, aunque roza la redundancia.

data|> 
  select(starts_with("cest_p11"))|>
  tab_itemscale()

data|>
  select(cest_p11_01,cest_p11_02,cest_p11_05,cest_p11_06,cest_p11_07,cest_p11_08,
         cest_p11_10)|>
  tab_itemscale()

data|>
  select(starts_with("cest_p12"))|>
  tab_itemscale()
