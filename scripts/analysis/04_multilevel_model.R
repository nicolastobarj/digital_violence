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
               ggpubr,
               lme4
)

source("scripts/proccesing/01_proc_data.R")

# Modelo Nulo ----
# Las dinámicas de violencia entre escuelas se parecen mucho. 

m0_von <- glmer(
  vict_onl ~ 1 + (1 | rbd), 
  family = binomial(link = "logit"),            # Familia logística
  data = segundo)
summary(m0_von)
performance::icc(m0_von) #3% se explica por nivel escuela


m0_voff <- glmer(
  vict_off ~ 1 + (1 | rbd), 
  family = binomial(link = "logit"),            # Familia logística
  data = segundo)
summary(m0_voff)
performance::icc(m0_voff) #1% Se explica por nivel escuela


#¿Cómo va a ser tan poca la variabilidad por escuela? Algo raro está pasando
#Se plotean los efectos aleatorios de los interceptos.

#Plot Random effects Violencia Offline: Comportamiento normal.
random_effects_m0_voff <- ranef(m0_voff)$rbd
colnames(random_effects_m0_voff) <- "intercept"
random_effects_m0_voff$probability <- plogis(fixef(m0_voff)[1] + 
                                               random_effects_m0_voff$intercept)
ggplot(random_effects_m0_voff, aes(x = reorder(rownames(random_effects_m0_voff), probability), 
                           y = probability)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = plogis(fixef(m0_voff)[1]), 
             color = "red", 
             linetype = "dashed") +
  labs(title = "School-Level Intercepts in Victimization Probability",
       subtitle = "Little variation between schools (ICC = 1%)",
       x = "School (RBD)",
       y = "Predicted Probability of Victimization",
       caption = "Red line shows overall average probability") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Hide school IDs
        panel.grid.major.x = element_blank())



# Plot random effects violencia digital: Un outlier está distorsionando!
random_effects_m0_von <- ranef(m0_von)$rbd
colnames(random_effects_m0_von) <- "intercept"
random_effects_m0_von$probability <- plogis(fixef(m0_von)[1] + 
                                              random_effects_m0_von$intercept)
ggplot(random_effects_m0_von, aes(x = reorder(rownames(random_effects_m0_von), probability), 
                                   y = probability)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = plogis(fixef(m0_von)[1]), 
             color = "red", 
             linetype = "dashed") +
  labs(title = "School-Level Intercepts in Victimization Probability",
       subtitle = "Little variation between schools (ICC = 3%)",
       x = "School (RBD)",
       y = "Predicted Probability of Victimization",
       caption = "Red line shows overall average probability") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Hide school IDs
        panel.grid.major.x = element_blank())
       
#Identificar al outlier
random_effects_m0_von|>arrange(desc(intercept))|>head(10) #RBD 8953

#Filtrar el caso
segundo_a <- segundo|>
  filter(rbd != "8953") #Se restaron 22 casos

#Modelo nulo filtrado
m0_von_a <- glmer(
  vict_onl ~ 1 + (1 | rbd), 
  family = binomial(link = "logit"),            # Familia logística
  data = segundo_a)
summary(m0_von_a)
performance::icc(m0_von_a) #Todavía 3% se explica por nivel escuela

# Plot random effects violencia digital: Colegios sobre la media varían más
random_effects_m0_von_a <- ranef(m0_von_a)$rbd
colnames(random_effects_m0_von_a) <- "intercept"
random_effects_m0_von_a$probability <- plogis(fixef(m0_von_a)[1] + 
                                              random_effects_m0_von_a$intercept)
ggplot(random_effects_m0_von_a, aes(x = reorder(rownames(random_effects_m0_von_a), probability), 
                                  y = probability)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = plogis(fixef(m0_von_a)[1]), 
             color = "red", 
             linetype = "dashed") +
  labs(title = "School-Level Intercepts in Victimization Probability",
       subtitle = "Little variation between schools (ICC = 3%)",
       x = "School (RBD)",
       y = "Predicted Probability of Victimization",
       caption = "Red line shows overall average probability") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Hide school IDs
        panel.grid.major.x = element_blank())

#Por ahora trabajar con 1 solo nivel, pues no se distorsiona significativamente la variabilidad

# M1. Genero ----
m1_voff <- glmer(vict_off ~ genero + (1 | rbd), 
    data = segundo, 
    family = binomial(link = "logit"))

m1_vonl <- glmer(vict_onl ~ genero + (1 | rbd),
    data = segundo, 
    family = binomial(link = "logit"))

#M2. Testigo y Violencia ----
m2_voff <- glmer(vict_off ~ vict_onl + testigo_rrss + (1 | rbd), 
                 data = segundo, 
                 family = binomial(link = "logit"))

m2_vonl <- glmer(vict_onl ~ vict_off + testigo_rrss + (1 | rbd), 
                 data = segundo, 
                 family = binomial(link = "logit"))

#M3. Todas nivel 1 -----
m3_voff <- glmer(vict_off ~ genero + vict_onl + testigo_rrss + (1 | rbd), 
                 data = segundo, 
                 family = binomial(link = "logit"))

m3_vonl <- glmer(vict_onl ~ genero + vict_off + testigo_rrss + (1 | rbd), 
                 data = segundo, 
                 family = binomial(link = "logit"))

#M4. Interacción nivel 1 ----
m4_voff <- glmer(vict_off ~ genero + vict_onl + testigo_rrss + 
                   vict_onl*testigo_rrss + (1 | rbd), 
                 data = segundo, 
                 family = binomial(link = "logit"))

m4_vonl <- glmer(vict_onl ~ genero + vict_off + testigo_rrss +
                   vict_off * testigo_rrss + (1 | rbd), 
                 data = segundo, 
                 family = binomial(link = "logit"))

#Tabla de resultados ----
sjPlot::tab_model(m1_voff,m2_voff,m3_voff,m4_voff,
                  show.ci = FALSE, auto.label = FALSE,
                  p.style = "stars",collapse.se = TRUE,
                  show.re.var = TRUE,show.icc = FALSE,
                  show.obs = TRUE,show.ngroups = TRUE,
                  show.aic = TRUE)

sjPlot::tab_model(m1_vonl,m2_vonl,m3_vonl,m4_vonl,
                  show.ci = FALSE, auto.label = FALSE,
                  p.style = "stars",collapse.se = TRUE,
                  show.re.var = TRUE,show.icc = FALSE,
                  show.obs = TRUE,show.ngroups = TRUE,
                  show.aic = TRUE)

