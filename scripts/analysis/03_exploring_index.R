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
               ggcorrplot, #Correlation
               ggfittext, #Plotting
               treemap, #Plotting
               ggplot2, #Plot data
               treemapify, #Plot data
               scales,
               ggpubr,
               GGally,
               ggrepel, #Labels plot
               patchwork, #Unify plots
               gridExtra,
               ggpubr
)

source("scripts/proccesing/01_proc_data.R")

#Seleccionar variables de interés
data <- segundo %>%
  select(idAlumno, rbd, genero, vict_off, vict_onl, testigo_rrss, adopcion_digital,
         rbd_vict_off, rbd_vict_onl, rbd_testigo_rrss, rbd_adopcion_digital, rbd_financiamiento)

#Comparación de proporciones ----
#Lo que más reportan los estudiantes es ser testigos de violencia en RR.SS.
# Sin embargo, solo un 13% declara de ser víctimas en ellas, lo cual tampoco se condice a la violencia offline.
data %>%
  summarise(
    Victimización_Offline = mean(vict_off, na.rm = TRUE),
    Victimización_Online = mean(vict_onl, na.rm = TRUE),
    Testigo_RRSS = mean(testigo_rrss, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Proporción"
  )%>%
  ggplot(aes(x = Variable, y = Proporción, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(Proporción, accuracy = 1)), 
            vjust = -0.5, size = 4) +
  
  # Customize appearance
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Prevalencia de Violencia Escolar",
    subtitle = "Proporción de estudiantes que reportaron cada tipo",
    x = "",
    y = "Porcentaje (%)",
    caption = "Nota: Variables dicotómicas (0 = No, 1 = Sí)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )


# Comparación de proporciones por género ----
# Las mujeres sufren y notan más violencia que los hombres, pero en el género "Otro" esto se dispara
# Los niveles de victimización online tienden a ser más similares por género.

data %>%
  group_by(genero) %>%  # Replace 'sexo' with your gender column name
  summarise(
    Victimización_Offline = mean(vict_off, na.rm = TRUE),
    Victimización_Online = mean(vict_onl, na.rm = TRUE),
    Testigo_RRSS = mean(testigo_rrss, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -genero,
    names_to = "Variable",
    values_to = "Proporción"
  ) %>%
  drop_na()%>%
ggplot(aes(x = Variable, y = Proporción, fill = genero)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = scales::percent(Proporción, accuracy = 1)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5
  ) +
  
  # Customization
  scale_fill_manual(
    values = c("#E41A1C", "#377EB8", "#4DAF4A"),
    name = "Género"
  ) +
  scale_y_continuous(
    labels = scales::percent) +
  labs(
    title = "Prevalencia por Género",
    subtitle = "Proporción que reportó cada tipo de violencia",
    x = "",
    y = "Porcentaje (%)",
    caption = "Variables dicotómicas: 0 = No, 1 = Sí"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

#Cruce de variables de interés ----
# 1. Prepare co-occurrence data with percentages
cooccur_data <- combn(c("vict_off", "vict_onl", "testigo_rrss"), 2, simplify = FALSE) %>%
  map_dfr(function(pair) {
    # Calculate total cases per pair for percentage calculation
    total <- nrow(data)
    
    data %>%
      count(!!sym(pair[1]), !!sym(pair[2])) %>%
      mutate(
        percentage = n/total,
        combination = paste(
          ifelse(!!sym(pair[1]) == 1, "Sí", "No"), 
          case_when(
            pair[1] == "vict_off" ~ "Offline",
            pair[1] == "vict_onl" ~ "Online",
            pair[1] == "testigo_rrss" ~ "Testigo"
          ),
          "\n",
          ifelse(!!sym(pair[2]) == 1, "Sí", "No"), 
          case_when(
            pair[2] == "vict_off" ~ "Offline",
            pair[2] == "vict_onl" ~ "Online",
            pair[2] == "testigo_rrss" ~ "Testigo"
          )
        ),
        pair = paste(pair, collapse = "_")
      )
  }) %>%
  mutate(
    pair = factor(pair,
                  levels = c("vict_off_vict_onl", "vict_off_testigo_rrss", "vict_onl_testigo_rrss"),
                  labels = c("Vict. Offline vs Vict. Online", 
                             "Vict. Offline vs Testigo RRSS", 
                             "Vict. Online vs Testigo RRSS")
    )
  )

# 2. Create faceted treemap plot with your custom colors
#Las relaciones entre variables no se comportan de la misma forma
ggplot(cooccur_data, aes(area = n, fill = pair, subgroup = pair)) +
  geom_treemap() +
  geom_treemap_text(
    aes(label = paste0(combination, "\n", percent(percentage, accuracy = 0.1))),
    color = "white",
    place = "center",
    size = 12,
    grow = FALSE,
    reflow = TRUE
  ) +
  facet_wrap(~pair, ncol = 2) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) + # Your custom palette
  labs(
    title = "Co-ocurrencia de los tipos de violencia",
    subtitle = "El tamaño del área representa la cantidad de casos (n)\nEl porcentaje se calcula sobre el total de la muestra",
    caption = "Fuente: SIMCE 2023"
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    strip.text = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.margin = margin(20, 20, 20, 20)
  )

#Correlaciones ----
#Las victimizaciones se refuerzan entre sí
#Ser testigo favorece reconocer víctimización
#Tenuemente, los que perciben mayor adopción, ven menos violencia.
#¿Por qué los niveles adopción están tan poco relacionados?

# Select and convert variables
cor_matrix <- data %>%
  select(vict_off, vict_onl, testigo_rrss, adopcion_digital) %>%
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

#Distribución de violencias por escuela ----

# Ensure one row per school (if not already aggregated)
data %>%
  distinct(rbd, .keep_all = TRUE) %>%  # Keeps only one row per school
  select(rbd, rbd_vict_off, rbd_vict_onl, rbd_testigo_rrss) %>%
  pivot_longer(
    cols = -rbd,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = case_when(
      variable == "rbd_vict_off" ~ "Victimización Offline",
      variable == "rbd_vict_onl" ~ "Victimización Online",
      variable == "rbd_testigo_rrss" ~ "Testigo RRSS",
      TRUE ~ variable
    )
  )%>%

ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(
    alpha = 0.8,
    width = 0.6,
    outlier.shape = 21,
    outlier.fill = "red"
  )  +
  
  # Custom colors (using your preferred palette)
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  
  # Labels and theme
  labs(
    title = "Distribución de Violencia Escolar por Institución",
    subtitle = paste("N =", n_distinct(rbd$rbd), "escuelas (una observación por escuela)"),
    x = "",
    y = "Porcentaje de estudiantes afectados",
    caption = "El punto amarillo indica la media\nFuente: SIMCE 2023"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

#Tipo de escuela y violencia -----
# Los particulares presentan niveles notoriamente menores de violencia.
# Los particulares presentan mayores niveles de dispersión.
# Los subvencionados tienen niveles muy similares a los públicos.

# 1. Preparar datos (similar al anterior)
# Ensure one row per school and reshape
score_school <- data %>%
  distinct(rbd, .keep_all = TRUE) %>%  # Una observación por escuela
  select(rbd, rbd_financiamiento, 
         rbd_vict_off, rbd_vict_onl, rbd_testigo_rrss) %>%
  pivot_longer(
    cols = c(rbd_vict_off, rbd_vict_onl, rbd_testigo_rrss),
    names_to = "violence_type",
    values_to = "score"
  ) %>%
  mutate(
    violence_type = case_when(
      violence_type == "rbd_vict_off" ~ "Vict. Offline",
      violence_type == "rbd_vict_onl" ~ "Vict. Online",
      violence_type == "rbd_testigo_rrss" ~ "Testigo RRSS"
    ),
    violence_type = factor(violence_type, 
                           levels = c("Vict. Offline", "Vict. Online", "Testigo RRSS")),
    rbd_financiamiento = factor(rbd_financiamiento)
  )

# 2. Boxplot agrupados
ggplot(score_school, 
       aes(x = violence_type, y = score, fill = rbd_financiamiento)) +
  geom_boxplot(
    alpha = 0.8,
    width = 0.6,
    outlier.shape = 21,
    outlier.fill = "red",
    position = position_dodge(width = 0.8)
  ) +
  
  # Personalización (igual que antes)
  scale_fill_manual(
    values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"), 
    name = "Financiamiento"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    title = "Distribución de Violencia por Tipo de Escuela",
    subtitle = "Cada caja representa la distribución entre escuelas del mismo tipo",
    x = "",
    y = "Porcentaje de estudiantes afectados",
    caption = paste("Fuente: SIMCE 2023 | N escuelas =", n_distinct(score_school$rbd))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.major.x = element_blank()
  )

#3. Tabla con estadísticos
score_school|>
  group_by(violence_type, rbd_financiamiento)|>
  summarise(mean = mean(score, na.rm = TRUE),
            sd = sd (score, na.rm = TRUE))|>
  ungroup()|>
  kable()


# Correlaciones nivel escuela ----
#Las correlaciones nivel escuela son altas (Varianza compartida nivel 1)
#A más violencia física, más online y más se es testigo de violencia online

# Ensure one row per school
school_data <- data %>%
  distinct(rbd, .keep_all = TRUE)

# Create all variable pairs
var_pairs <- combn(c("rbd_vict_off", "rbd_vict_onl", "rbd_testigo_rrss"), 2, simplify = FALSE)

plot_list <- map(var_pairs, ~ {
  # Calculate correlation
  corr <- cor(school_data[[.x[1]]], school_data[[.x[2]]], use = "complete.obs")
  
  # Create plot
  ggplot(school_data, aes(x = !!sym(.x[1]), y = !!sym(.x[2]))) +
    geom_point(alpha = 0.6, color = "#377EB8", size = 2) +
    geom_smooth(method = "lm", color = "#E41A1C", se = TRUE) +
    annotate("text", 
             x = Inf, y = -Inf,
             label = paste0("r = ", round(corr, 2)),
             hjust = 1.1, vjust = -1, size = 5,
             color = "black") +
    labs(
      x = case_when(
        .x[1] == "rbd_vict_off" ~ "Vict. Offline (%)",
        .x[1] == "rbd_vict_onl" ~ "Vict. Online (%)",
        .x[1] == "rbd_testigo_rrss" ~ "Testigo RRSS (%)"
      ),
      y = case_when(
        .x[2] == "rbd_vict_off" ~ "Vict. Offline (%)",
        .x[2] == "rbd_vict_onl" ~ "Vict. Online (%)",
        .x[2] == "rbd_testigo_rrss" ~ "Testigo RRSS (%)"
      )
    ) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
})

# Combine plots
ggarrange(plotlist = plot_list, ncol = 3, nrow = 1)

#Correlaciones por tipo de escuela ----
#Las violencias se relacionan de forma muy parecida en todos los establecimientos.
#Pero los colegios particulares tienen una asociación mucho más fuerte que el resto en las variables online.
#Aunquen los colegios particulares tengan niveles menores, tienen más fuerte la asociación.
#Los colegios particulares tienen más colegios en los niveles inferiores, pero
#también una mayor proporción en colegios con niveles superires. 
#Por eso el tamaño de efecto es más fuerte. Parte más abajo y termina más arriba.

# Datos a nivel de escuela (una observación por RBD)
school_data <- data %>% 
  distinct(rbd, .keep_all = TRUE)

# Definir parejas de variables
var_pairs <- list(
  c("rbd_vict_off", "rbd_vict_onl"),
  c("rbd_vict_off", "rbd_testigo_rrss"), 
  c("rbd_vict_onl", "rbd_testigo_rrss")
)

# Paleta de colores para financiamiento
financ_colors <- c("#E41A1C", "#377EB8", "#4DAF4A") # Ejemplo: Público, Subvencionado, Privado

create_stratified_plot <- function(x_var, y_var) {
  
  # Calcular correlaciones por grupo
  corrs <- school_data %>%
    group_by(rbd_financiamiento) %>%
    summarise(
      r = round(cor(!!sym(x_var), !!sym(y_var), use = "complete.obs"), 2),
      x_pos = max(!!sym(x_var), na.rm = TRUE) * 0.8,
      y_pos = min(!!sym(y_var), na.rm = TRUE) * 1.1
    )
  
  # Crear gráfico
  ggplot(school_data, aes(x = !!sym(x_var), y = !!sym(y_var), 
                          color = rbd_financiamiento, fill = rbd_financiamiento)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    
    # Anotar correlaciones
    geom_text_repel(
      data = corrs,
      aes(x = x_pos, y = y_pos, 
          label = paste0("r = ", r),
          color = rbd_financiamiento),
      color = "black",
      size = 4,
      hjust = 0,
      show.legend = FALSE
    ) +
    
    # Formateo
    scale_color_manual(values = financ_colors) +
    scale_fill_manual(values = financ_colors) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste(
        case_when(
          x_var == "rbd_vict_off" ~ "Victimización Offline",
          x_var == "rbd_vict_onl" ~ "Victimización Online",
          TRUE ~ "Testigo RRSS"
        ),
        "vs",
        case_when(
          y_var == "rbd_vict_off" ~ "Victimización Offline",
          y_var == "rbd_vict_onl" ~ "Victimización Online",
          TRUE ~ "Testigo RRSS"
        )
      ),
      x = "Porcentaje en escuela (X)",
      y = "Porcentaje en escuela (Y)",
      color = "Financiamiento",
      fill = "Financiamiento"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Gráfico 1: Offline vs Online
plot1 <- create_stratified_plot("rbd_vict_off", "rbd_vict_onl")

# Gráfico 2: Offline vs Testigo
plot2 <- create_stratified_plot("rbd_vict_off", "rbd_testigo_rrss")


# Gráfico 3: Online vs Testigo
plot3 <- create_stratified_plot("rbd_vict_onl", "rbd_testigo_rrss")

plot1  # Muestra Offline vs Online
plot2  # Muestra Offline vs Testigo
plot3  # Muestra Online vs Testigo

#Correlación adopción con violencia ----

# Prepare data - one row per school
school_data <- data %>%
  distinct(rbd, .keep_all = TRUE) %>%
  select(rbd, 
         rbd_adopcion_digital,
         rbd_vict_off, 
         rbd_vict_onl, 
         rbd_testigo_rrss)

# Function to create correlation plots
create_corr_plot <- function(y_var) {
  
  # Calculate correlation
  corr_val <- cor(school_data$rbd_adopcion_digital, 
                  school_data[[y_var]], 
                  use = "complete.obs")
  
  # Determine axis limits
  x_range <- range(school_data$rbd_adopcion_digital, na.rm = TRUE)
  y_range <- range(school_data[[y_var]], na.rm = TRUE)
  
  ggplot(school_data, aes(x = rbd_adopcion_digital, y = !!sym(y_var))) +
    geom_point(color = "#377EB8", alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", color = "#E41A1C", se = TRUE, fill = "#FDB462") +
    annotate("text",
             x = x_range[1] + diff(x_range)*0.7, 
             y = y_range[2]*0.9,
             label = paste0("r = ", round(corr_val, 2)),
             size = 5, color = "black", fontface = "bold") +
    labs(
      x = "Adopción Digital (Escuela)",
      y = case_when(
        y_var == "rbd_vict_off" ~ "Vict. Offline (%)",
        y_var == "rbd_vict_onl" ~ "Vict. Online (%)",
        y_var == "rbd_testigo_rrss" ~ "Testigo RRSS (%)"
      )
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Create individual plots
plot1 <- create_corr_plot("rbd_vict_off") + 
  ggtitle("Victimización Offline")

plot2 <- create_corr_plot("rbd_vict_onl") + 
  ggtitle("Victimización Online")

plot3 <- create_corr_plot("rbd_testigo_rrss") + 
  ggtitle("Testigo RRSS")

plot1
plot2
plot3
