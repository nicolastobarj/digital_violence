# Preparacion ----

pacman::p_load(tidyverse, #Data depuration
               sjPlot, #Bivariate Analysis
               summarytools, #Descriptive tables 
               labelled, #Label data
               knitr, #Rendering
               kableExtra #Rendering
               )

load("~/GitHub/milenio_nudos/digital_violence/data/raw_data/simce2m2023.RData")
segundo <- data
rm(data)

rbd <- read.csv(file = "data/raw_data/simce2m2023_rbd_público_final.csv", sep = ";")


segundo <- merge(segundo, rbd, by = "rbd")
  
segundo <- segundo|>
  mutate(across(starts_with("cest_p11"),~ ifelse(.x %in% c(0,99), NA, .x)))|>
  mutate(across(starts_with("cest_p12"),~ ifelse(.x %in% c(0,99), NA, .x)))

var_label(segundo) <- list(
  cest_p11_01 = "Se han burlado de mí o me molestan.",
  cest_p11_02 = "Me han ignorado, aislado o me han excluido.",
  cest_p11_03 = "Me han hecho sentir mal en redes sociales o internet (por ejemplo, me han insultado, amenazado, etc.).",
  cest_p11_04 = "Han compartido fotos, videos o información mía por redes sociales o internet sin mi consentimiento.",
  cest_p11_05 = "Me han golpeado.",
  cest_p11_06 = "Me han roto o destruido cosas personales a propósito.",
  cest_p11_07 = "Me han amenazado.",
  cest_p11_08 = "Me han obligado a realizar cosas que no quiero.",
  cest_p11_09 = "Me han robado cosas.",
  cest_p11_10 = "He dejado de asistir al colegio porque siento miedo de que alguien me haga daño."
)

# 1. Frecuencias y Distribuciones Batería de violencia offline ----

segundo |> 
  select(starts_with("cest_p11")) |> 
  dfSummary() |> 
  print(method = "browser") |>  # Opens in Viewer
  kable(format = "html") |> 
  kable_styling("striped") |> 
  save_kable(file = "output/tables/summary_2m_violencia_offline.html")  # Save to file


# 2. Correlación con violencia en redes sociales (Solo segundo) ----

# Save as PNG
png(filename = "output/plots/corrplot_exploration.png", 
    width = 5, 
    height = 5, 
    units = "in", 
    res = 300)  # 300 dpi for high quality

# Generate and save the plot
segundo %>%
  select(cest_p11_03, cest_p11_04, starts_with("cest_p12")) %>%
  cor(use = "complete.obs") %>%
  corrplot::corrplot(
    method = "color",
    type = "upper",
    tl.col = "black",
    addCoef.col = "black",
    number.cex = 0.7  # Adjust coefficient font size if needed
  )

dev.off()  # Important - this saves the file

# 3. Factorial exploration ----

segundo|>
  select(starts_with("cest_p11"))|>
  tab_fa()

segundo|>
  select(cest_p11_03,cest_p11_04,starts_with("cest_p12"))|>
  tab_fa()


segundo|>
  select(starts_with("cest_p12"))|>
  tab_fa()

segundo|>
  select("cest_p11_01", "cest_p11_02", 
         "cest_p11_05", "cest_p11_07", 
         "cest_p11_08")|>
  tab_fa()


# 3. Multivariado ----

segundo <- segundo %>%
  mutate(
    violencia_offline = rowMeans(
      select(., cest_p11_01, cest_p11_02, cest_p11_05,
             cest_p11_06, cest_p11_07, cest_p11_08, cest_p11_09),
      na.rm = TRUE
    ),
    violencia_online = rowMeans(
      select(., cest_p11_03, cest_p11_04),
      na.rm = TRUE
    ),
    testigo_violencia_online = rowMeans(
      select(., starts_with("cest_p12")),
      na.rm = TRUE
    ),
    genero = case_when(cest_p01 %in% c(0,99) ~ NA_character_,
                       cest_p01 == "1" ~ "a. Masculino",
                       cest_p01 == "2" ~ "b. Femenino", 
                       cest_p01 %in% c("3", "4", "5") ~ "c. Otro")
  )

model1 <- lm(violencia_offline ~ violencia_online + testigo_violencia_online +
               genero, data = segundo)

# Save as self-contained HTML file
html_table <- tab_model(
  model1,
  show.ci = FALSE, 
  auto.label = TRUE,
  p.style = "stars",
  collapse.se = FALSE,
  show.re.var = TRUE,
  show.icc = FALSE,
  show.obs = TRUE,
  file = "output/tables/model_results.html"
)

# If you need to further customize the HTML:
html_table %>%
  save_kable(file = "output/tables/model_results.html", 
             self_contained = TRUE)  # Creates single HTML file

model2 <- lm(cest_p11_03 ~ violencia_offline + testigo_violencia_online +
               genero, data = segundo)

# Save as self-contained HTML file
html_table2 <- tab_model(
  model2,
  show.ci = FALSE, 
  auto.label = TRUE,
  p.style = "stars",
  collapse.se = FALSE,
  show.re.var = TRUE,
  show.icc = FALSE,
  show.obs = TRUE,
  file = "output/tables/model_results.html"
)

# If you need to further customize the HTML:
html_table2 %>%
  save_kable(file = "output/tables/model_results2.html", 
             self_contained = TRUE)  # Creates single HTML file


model3 <- lm(cest_p11_04 ~ violencia_offline + testigo_violencia_online +
               genero, data = segundo)

# Save as self-contained HTML file
html_table3 <- tab_model(
  model3,
  show.ci = FALSE, 
  auto.label = TRUE,
  p.style = "stars",
  collapse.se = FALSE,
  show.re.var = TRUE,
  show.icc = FALSE,
  show.obs = TRUE,
  file = "output/tables/model_results.html"
)

# If you need to further customize the HTML:
html_table3 %>%
  save_kable(file = "output/tables/model_results3.html", 
             self_contained = TRUE)  # Creates single HTML file
