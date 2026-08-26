# Instalar paquetes si no están disponibles
if (!require('haven')) install.packages('haven')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('readxl')) install.packages('readxl')
if (!require('chilemapas')) install.packages('chilemapas')
if (!require('dplyr')) install.packages('dplyr')
if (!require('tidyr')) install.packages('tidyr')
if (!require('stringr')) install.packages('stringr')
if (!require('parallelly')) install.packages('parallelly')
if (!require('future')) install.packages('future')
if (!require('future.apply')) install.packages('future.apply')
if (!require('patchwork')) install.packages('patchwork')
if (!require('broom.mixed')) install.packages('broom.mixed')
if (!require('jtools')) install.packages('jtools')
if (!require('interactions')) install.packages('interactions')
if (!require('margins')) install.packages('margins')
if (!require('car')) install.packages('car')
if (!require('lmtest')) install.packages('lmtest')
if (!require('sandwich')) install.packages('sandwich')
if (!require('lme4')) install.packages('lme4')
if (!require('sjPlot')) install.packages('sjPlot')
if (!require('ggeffects')) install.packages('ggeffects')
if (!require('janitor')) install.packages('janitor')
if (!require('stringi')) install.packages('stringi')
if (!require('betareg')) install.packages('betareg')
if (!require('moments')) install.packages('moments')
if (!require('clubSandwich')) install.packages('clubSandwich')
if (!require('sf'))          install.packages('sf')
if (!require('spdep'))      install.packages('spdep')
if (!require('spatialreg')) install.packages('spatialreg')

# Cargar librerías
library(haven)
library(tidyverse)
library(readxl)
library(chilemapas)
library(dplyr)
library(tidyr)
library(stringr)
library(parallelly)
library(future)
library(future.apply)
library(patchwork)
library(broom.mixed)
library(jtools)
library(interactions)
library(margins)
library(car)
library(lmtest)
library(sandwich)
library(lme4)
library(sjPlot)
library(ggeffects)
library(janitor)
library(stringi)
library(stringr)
library(betareg)
library(moments)
library(clubSandwich)
library(sf)
library(spdep)
library(spatialreg)
library(pandoc)
library(flextable)
library(modelsummary)


#inicio del codigo

procesar_eleccion <- function(path_excel, sheet = "Votación por comuna", nombre_eleccion = "eleccion") {
  
  #datitos
  datos_raw <- read_excel(path_excel, skip = 6, sheet = sheet)
  participacion_raw <- read_excel(path_excel, skip = 6, sheet = "Participación")
  
  # Inscritos
  fase2 <- participacion_raw %>%
    group_by(Comuna) %>%
    summarise(total_inscritos = sum(Inscritos, na.rm = TRUE), .groups = "drop")
  
  # comunas y regiones y circuns
  geografia <- datos_raw %>%
    select(Región, Comuna, `Circunscripción Provincial`) %>%
    distinct(Comuna, .keep_all = TRUE)
  
  # votos x comuna
  votos_comuna <- datos_raw %>%
    group_by(Comuna) %>%
    summarise(total_votos = sum(Votos, na.rm = TRUE), .groups = "drop")
  
  # Nulos y Blancos
  nulos_blancos <- datos_raw %>%
    mutate(Nombres = toupper(trimws(Nombres))) %>%
    filter(Nombres %in% c("VOTOS NULOS", "VOTOS EN BLANCO")) %>%
    group_by(Comuna) %>%
    summarise(
      votos_nulos = sum(Votos[Nombres == "VOTOS NULOS"], na.rm = TRUE),
      votos_blancos = sum(Votos[Nombres == "VOTOS EN BLANCO"], na.rm = TRUE),
      votos_invalidos = votos_nulos + votos_blancos,
      .groups = "drop"
    )
  
  # Pactos y Candidatos 
  pactos_info <- datos_raw %>%
    mutate(Nombres = toupper(trimws(Nombres))) %>%
    filter(!Nombres %in% c("VOTOS NULOS", "VOTOS EN BLANCO")) %>%
    mutate(Pacto_limpia = na_if(stringr::str_squish(as.character(Pacto)), ""),
           nombre_completo = paste(Nombres,
                                   toupper(trimws(`Primer apellido`)),
                                   toupper(trimws(`Segundo apellido`)))) %>%
    group_by(Comuna) %>%
    summarise(
      pactos_unicos = n_distinct(Pacto_limpia, na.rm = TRUE),
      candidatos = n_distinct(nombre_completo),
      Pactos = paste(sort(unique(Pacto_limpia[!is.na(Pacto_limpia)])), collapse = ", "),
      indep_fuera_pacto = n_distinct(nombre_completo[Pacto_limpia == "INDEPENDIENTES"]),
      .groups = "drop"
    )
  
  # unir todo
  alcaldefinal <- geografia %>%
    left_join(votos_comuna, by = "Comuna") %>%
    left_join(nulos_blancos, by = "Comuna") %>%
    left_join(fase2, by = "Comuna") %>%
    left_join(pactos_info, by = "Comuna") %>%
    mutate(eleccion = nombre_eleccion)
  
  # escañoxcargo
  alcaldefinal <- alcaldefinal %>%
    mutate(
      escaños = case_when(
        eleccion == "Gobernadores" ~ 1,
        eleccion == "Alcaldes"     ~ 1,
        eleccion == "Concejales" & total_inscritos <= 70000 ~ 6,
        eleccion == "Concejales" & total_inscritos > 70000 & total_inscritos <= 150000 ~ 8,
        eleccion == "Concejales" & total_inscritos > 150000 ~ 10,
        eleccion == "CORE" & `Circunscripción Provincial` == "ARICA" ~ 11,
        eleccion == "CORE" & `Circunscripción Provincial` == "PARINACOTA" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "IQUIQUE" ~ 11,
        eleccion == "CORE" & `Circunscripción Provincial` == "TAMARUGAL" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "ANTOFAGASTA" ~ 8,
        eleccion == "CORE" & `Circunscripción Provincial` == "EL LOA" ~ 5,
        eleccion == "CORE" & `Circunscripción Provincial` == "TOCOPILLA" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "CHAÑARAL" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "COPIAPO" ~ 8,
        eleccion == "CORE" & `Circunscripción Provincial` == "HUASCO" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "CHOAPA" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "ELQUI" ~ 8,
        eleccion == "CORE" & `Circunscripción Provincial` == "LIMARI" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "ISLA DE PASCUA" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "LOS ANDES" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "MARGA MARGA" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "PETORCA" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "QUILLOTA" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "SAN ANTONIO" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "SAN FELIPE" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "VALPARAISO I" ~ 5,
        eleccion == "CORE" & `Circunscripción Provincial` == "VALPARAISO II" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "CACHAPOAL I" ~ 5,
        eleccion == "CORE" & `Circunscripción Provincial` == "CACHAPOAL II" ~ 8,
        eleccion == "CORE" & `Circunscripción Provincial` == "CARDENAL CARO" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "COLCHAGUA" ~ 5,
        eleccion == "CORE" & `Circunscripción Provincial` == "CAUQUENES" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "CURICO" ~ 6,
        eleccion == "CORE" & `Circunscripción Provincial` == "LINARES" ~ 5,
        eleccion == "CORE" & `Circunscripción Provincial` == "TALCA" ~ 7,
        eleccion == "CORE" & `Circunscripción Provincial` == "DIGUILLIN" ~ 8,
        eleccion == "CORE" & `Circunscripción Provincial` == "ITATA" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "PUNILLA" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "ARAUCO" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "BIOBIO" ~ 6,
        eleccion == "CORE" & `Circunscripción Provincial` == "CONCEPCION I" ~ 6,
        eleccion == "CORE" & `Circunscripción Provincial` == "CONCEPCION II" ~ 6,
        eleccion == "CORE" & `Circunscripción Provincial` == "CONCEPCION III" ~ 6,
        eleccion == "CORE" & `Circunscripción Provincial` == "CAUTIN I" ~ 7,
        eleccion == "CORE" & `Circunscripción Provincial` == "CAUTIN II" ~ 8,
        eleccion == "CORE" & `Circunscripción Provincial` == "MALLECO" ~ 5,
        eleccion == "CORE" & `Circunscripción Provincial` == "RANCO" ~ 5,
        eleccion == "CORE" & `Circunscripción Provincial` == "VALDIVIA" ~ 9,
        eleccion == "CORE" & `Circunscripción Provincial` == "CHILOE" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "LLANQUIHUE" ~ 8,
        eleccion == "CORE" & `Circunscripción Provincial` == "OSORNO" ~ 6,
        eleccion == "CORE" & `Circunscripción Provincial` == "PALENA" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "AISEN" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "CAPITAN PRAT" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "COYHAIQUE" ~ 6,
        eleccion == "CORE" & `Circunscripción Provincial` == "GENERAL CARRERA" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "ANTARTICA CHILENA" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "MAGALLANES" ~ 7,
        eleccion == "CORE" & `Circunscripción Provincial` == "TIERRA DEL FUEGO" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "ULTIMA ESPERANZA" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "CHACABUCO" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "CORDILLERA" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "MAIPO" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "MELIPILLA" ~ 2,
        eleccion == "CORE" & `Circunscripción Provincial` == "SANTIAGO I" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "SANTIAGO II" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "SANTIAGO III" ~ 3,
        eleccion == "CORE" & `Circunscripción Provincial` == "SANTIAGO IV" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "SANTIAGO V" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "SANTIAGO VI" ~ 4,
        eleccion == "CORE" & `Circunscripción Provincial` == "TALAGANTE" ~ 2,
        TRUE ~ NA_real_ 
      )
    )
  
  return(alcaldefinal)
}

#repeticion de tods


Concejales <- procesar_eleccion(
  path_excel = "elecciones/2024_10_Concejales_DatosEleccion.xlsx",
  sheet = "Votación por comuna",
  nombre_eleccion = "Concejales"
)

CORE <- procesar_eleccion(
  path_excel = "elecciones/2024_10_ConsejerosRegionales_DatosEleccion.xlsx",
  sheet = "Votación por comuna",
  nombre_eleccion = "CORE"
)

GobernadoresRegionales <- procesar_eleccion(
  path_excel = "elecciones/2024_10_GobernadoresRegionales_Datos_Eleccion.xlsx",
  sheet = "Votación por comuna",
  nombre_eleccion = "Gobernadores"
)

Alcaldes <- procesar_eleccion(
  path_excel = "elecciones/2024_10_Alcaldes_Datos_Eleccion.xlsx",
  sheet = "Votación por comuna",
  nombre_eleccion = "Alcaldes"
)

#unir todo

basefinal <- bind_rows(
  Concejales,
  CORE,
  GobernadoresRegionales,
  Alcaldes
)

basefinal <- basefinal %>%
  mutate(
    porcentaje_nulos = round(votos_nulos / total_votos, 4),
    porcentaje_blancos = round(votos_blancos / total_votos, 4),
    pct_invalidos = round(votos_invalidos / total_votos, 4)
  )


basefinal <- basefinal %>%
  mutate(across(c(total_votos, votos_nulos, votos_blancos, votos_invalidos,
                  total_inscritos, pactos_unicos, candidatos, indep_fuera_pacto),
                ~ round(., 2)))


basefinal <- basefinal %>%
  mutate(
    Comuna = ifelse(Comuna == "LLAILLAY", "LLAY-LLAY", Comuna)
  )


#automatizacion para saber la participación de 2021

procesar_eleccion2 <- function(path_excel, sheet = "Participación", nombre_eleccion = "eleccion") {
  
  # 1. Leer datos
  datos_raw <- read_excel(path_excel, skip = 6, sheet = sheet)
  
  alo <- datos_raw %>%
    rename(Votos = any_of(c("Votos", "Votación", "Votacion"))) %>%
    group_by(Comuna) %>%
    summarise(total_inscritos = sum(Inscritos, na.rm = TRUE),
              total_votos= sum(Votos, na.rm = TRUE),
              .groups = "drop")
  
  alo <- alo %>% 
    mutate(porcentaje_participación2021= round(total_votos/total_inscritos, 4),
           eleccion = nombre_eleccion
    )
  
}


Concejales2 <- procesar_eleccion2(
  path_excel = "elecciones/2021_05_Concejales_DatosEleccion.xlsx",
  sheet = "Participación",
  nombre_eleccion = "Concejales"
)

CORE2 <- procesar_eleccion2(
  path_excel = "elecciones/2021_11_ConsejerosRegionales_DatosEleccion.xlsx",
  sheet = "Participación",
  nombre_eleccion = "CORE"
)

GobernadoresRegionales2 <- procesar_eleccion2(
  path_excel = "elecciones/2021_05_GobernadoresRegionales_Datos_Eleccion.xlsx",
  sheet = "Participación",
  nombre_eleccion = "Gobernadores"
)

Alcaldes2 <- procesar_eleccion2(
  path_excel = "elecciones/2021_05_Alcaldes_Datos_Eleccion.xlsx",
  sheet = "Participación",
  nombre_eleccion = "Alcaldes"
)

basefinal2 <- bind_rows(
  Concejales2,
  CORE2,
  GobernadoresRegionales2,
  Alcaldes2
)

basefinal2 <- basefinal2 %>%
  mutate(Comuna = ifelse(Comuna == "LLAILLAY", "LLAY-LLAY", Comuna))

basefinal2 <- basefinal2 %>%
  mutate(Comuna = ifelse(Comuna == "OLLAGUE", "OLLAGÜE", Comuna))




Sinim <- read_excel("Sinim.xlsx", skip = 2)

Sinim <- Sinim %>%
  mutate(
    Comuna = chartr("ÁÉÍÓÚ", "AEIOU", Comuna),
    Comuna = case_match(Comuna,
                        "LLAILLAY"       ~ "LLAY-LLAY",
                        "PAIGUANO"       ~ "PAIHUANO",
                        "MARCHIHUE"      ~ "MARCHIGUE",
                        "O´HIGGINS"      ~ "O'HIGGINS", 
                        "CABO DE HORNOS" ~ "CABO DE HORNOS(EX-NAVARINO)",
                        .default = Comuna 
    )
  )


base_unida <- basefinal %>%
  left_join(
    basefinal2 %>% select(Comuna, eleccion, porcentaje_participación2021), 
    by = c("Comuna", "eleccion")
  )

base_unida <- base_unida %>%
  left_join(Sinim, by= "Comuna") %>%
  select(-any_of(c("personas_enviadas_a_un_empleo", "complejidad")))


#importante

base_unida <- base_unida %>%
  mutate(
    prop_indep_fuera_pacto = indep_fuera_pacto / candidatos) %>%
  mutate(ratio_pactos = pactos_unicos / candidatos)

#Variable del INE para cantidad de personas x comuna

INE_población_2024 <- read_excel("ine 2024.xlsx", skip = 3, sheet = 3)


#vamos a arreglar esta cosa


normalizar_comuna <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("Á", "A") %>%
    str_replace_all("É", "E") %>%
    str_replace_all("Í", "I") %>%
    str_replace_all("Ó", "O") %>%
    str_replace_all("Ú", "U") %>%
    str_replace_all("Ü", "U") %>%
    str_replace_all("Ñ", "N")
}

# Normalizar base_unida 
base_unida <- base_unida %>%
  mutate(Comuna = normalizar_comuna(Comuna))

# Normalizar INE y limpiar

INE_población_2024 <- INE_población_2024 %>%
  mutate(Comuna = normalizar_comuna(Comuna)) %>%
  filter(Comuna != "PAIS")


base_unida <- base_unida %>%
  left_join(
    INE_población_2024 %>% select(Comuna, `Población censada`),
    by = "Comuna"
  )

# corregir comunas q dan error
base_unida <- base_unida %>%
  mutate(Comuna = case_when(
    Comuna == "CABO DE HORNOS(EX-NAVARINO)" ~ "CABO DE HORNOS",
    Comuna == "LLAY-LLAY" ~ "LLAILLAY",
    Comuna == "MARCHIGUE" ~ "MARCHIHUE",
    TRUE ~ Comuna
  ))

# juntar lo corrgedo
base_unida <- base_unida %>%
  select(-`Población censada`) %>%
  left_join(
    INE_población_2024 %>% select(Comuna, `Población censada`, `Código comuna`),
    by = "Comuna"
  )

#reparar nombres para q no den error, no tan importante creo
base_unida <- base_unida %>% clean_names()

base_unida$comuna <- str_trim(str_to_upper(stri_trans_general(base_unida$comuna, "Latin-ASCII")))

# Formatear código comuna como string de 5 dígitos para que coincida con chilemapas
base_unida$codigo_comuna <- sprintf("%05d", as.integer(base_unida$codigo_comuna))


names(base_unida)



#variable de educación de casencita

educacion_ine <- read_excel("Educación_INE.xlsx", skip = 3, sheet = 5)

educacion_ine <- educacion_ine %>%
  select(Comuna, Sexo, `Años de escolaridad promedio`) %>%
  filter(Sexo == "Total Comuna") %>%
  select(comuna = Comuna,
         anios_escolaridad_promedio = `Años de escolaridad promedio`)

#cambiar nombre para q no den error
educacion_ine$comuna <- str_trim(str_to_upper(stri_trans_general(educacion_ine$comuna, "Latin-ASCII")))

#juntar todo para agregar  educacion promedio

base_unida <- base_unida %>%
  left_join(educacion_ine, by = "comuna")


# Parte descriptiva de la tesis con tablas y otras cosas

#Variables dependientes x cargo redondeando para orden

descriptivo_dependientes <- base_unida %>%
  group_by(eleccion) %>%
  summarise(
    media_nulos          = round(mean(porcentaje_nulos, na.rm = TRUE), 4),
    sd_nulos             = round(sd(porcentaje_nulos, na.rm = TRUE), 4),
    min_nulos            = round(min(porcentaje_nulos, na.rm = TRUE), 4),
    max_nulos            = round(max(porcentaje_nulos, na.rm = TRUE), 4),
    media_blancos        = round(mean(porcentaje_blancos, na.rm = TRUE), 4),
    sd_blancos           = round(sd(porcentaje_blancos, na.rm = TRUE), 4),
    min_blancos          = round(min(porcentaje_blancos, na.rm = TRUE), 4),
    max_blancos          = round(max(porcentaje_blancos, na.rm = TRUE), 4),
    media_invalidos      = round(mean(pct_invalidos, na.rm = TRUE), 4),
    sd_invalidos         = round(sd(pct_invalidos, na.rm = TRUE), 4),
    min_invalidos        = round(min(pct_invalidos, na.rm = TRUE), 4),
    max_invalidos        = round(max(pct_invalidos, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(descriptivo_dependientes)

# Variables de oferta electoral

descriptivo_oferta <- base_unida %>%
  group_by(eleccion) %>%
  summarise(
    media_pactos     = round(mean(pactos_unicos, na.rm = TRUE), 2),
    sd_pactos        = round(sd(pactos_unicos, na.rm = TRUE), 2),
    min_pactos       = round(min(pactos_unicos, na.rm = TRUE), 2),
    max_pactos       = round(max(pactos_unicos, na.rm = TRUE), 2),
    media_indep      = round(mean(prop_indep_fuera_pacto, na.rm = TRUE), 2),
    sd_indep         = round(sd(prop_indep_fuera_pacto, na.rm = TRUE), 2),
    min_indep        = round(min(prop_indep_fuera_pacto, na.rm = TRUE), 2),
    max_indep        = round(max(prop_indep_fuera_pacto, na.rm = TRUE), 2),
    media_candidatos = round(mean(candidatos, na.rm = TRUE), 2),
    sd_candidatos    = round(sd(candidatos, na.rm = TRUE), 2),
    min_candidatos   = round(min(candidatos, na.rm = TRUE), 2),
    max_candidatos   = round(max(candidatos, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(descriptivo_oferta)

# Distribución de candidatos por cargo (justificación del log)
distribucion_candidatos <- base_unida %>%
  group_by(eleccion) %>%
  summarise(
    min_cand    = min(candidatos, na.rm = TRUE),
    max_cand    = max(candidatos, na.rm = TRUE),
    media_cand  = round(mean(candidatos, na.rm = TRUE), 2),
    mediana_cand= round(median(candidatos, na.rm = TRUE), 2),
    sd_cand     = round(sd(candidatos, na.rm = TRUE), 2),
    sesgo_cand  = round(skewness(candidatos, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(distribucion_candidatos)

# Tabla word
ft_dist_cand <- flextable(distribucion_candidatos) %>%
  set_header_labels(
    eleccion     = "Cargo",
    min_cand     = "Mín.",
    max_cand     = "Máx.",
    media_cand   = "Media",
    mediana_cand = "Mediana",
    sd_cand      = "DE",
    sesgo_cand   = "Sesgo"
  ) %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  set_caption("Distribución del número de candidatos por tipo de cargo") %>%
  add_footer_lines("Sesgo calculado con el coeficiente de Fisher (moments::skewness). Valores positivos indican sesgo a la derecha. Fuente: Elaboración propia basada en datos del SERVEL.") %>%
  autofit()

save_as_docx(ft_dist_cand, path = "tabla_distribucion_candidatos.docx")

# Tabla combinada: variables como filas, cargos como columnas

dep_long <- descriptivo_dependientes %>%
  pivot_longer(-eleccion, names_to = "stat_var", values_to = "valor") %>%
  mutate(
    variable = case_when(
      str_detect(stat_var, "nulos")     ~ "Voto nulo",
      str_detect(stat_var, "blancos")   ~ "Voto blanco",
      str_detect(stat_var, "invalidos") ~ "Voto inválido"
    ),
    estadistico = case_when(
      str_starts(stat_var, "media") ~ "Media",
      str_starts(stat_var, "sd")    ~ "DE",
      str_starts(stat_var, "min")   ~ "Min",
      str_starts(stat_var, "max")   ~ "Max"
    )
  ) %>%
  select(variable, estadistico, eleccion, valor) %>%
  pivot_wider(names_from = eleccion, values_from = valor)

oferta_long <- descriptivo_oferta %>%
  pivot_longer(-eleccion, names_to = "stat_var", values_to = "valor") %>%
  mutate(
    variable = case_when(
      str_detect(stat_var, "pactos")     ~ "Pactos únicos",
      str_detect(stat_var, "indep")      ~ "Indep. fuera de pacto",
      str_detect(stat_var, "candidatos") ~ "Candidatos"
    ),
    estadistico = case_when(
      str_starts(stat_var, "media") ~ "Media",
      str_starts(stat_var, "sd")    ~ "DE",
      str_starts(stat_var, "min")   ~ "Min",
      str_starts(stat_var, "max")   ~ "Max"
    )
  ) %>%
  select(variable, estadistico, eleccion, valor) %>%
  pivot_wider(names_from = eleccion, values_from = valor)

tabla_larga <- bind_rows(dep_long, oferta_long) %>%
  mutate(
    variable    = factor(variable, levels = c("Voto nulo", "Voto blanco", "Voto inválido",
                                              "Pactos únicos", "Indep. fuera de pacto", "Candidatos")),
    estadistico = factor(estadistico, levels = c("Media", "DE", "Min", "Max"))
  ) %>%
  arrange(variable, estadistico) %>%
  select(variable, estadistico, Alcaldes, Concejales, CORE, Gobernadores)

ft_combinada <- flextable(tabla_larga) %>%
  set_header_labels(variable = "Variable", estadistico = "") %>%
  merge_v(j = "variable") %>%
  hline(i = 12) %>%
  theme_vanilla() %>%
  align(align = "center", part = "header") %>%
  align(j = 3:6, align = "center", part = "body") %>%
  align(j = 1:2, align = "left", part = "body") %>%
  bold(j = "variable", part = "body") %>%
  set_caption("Estadísticos descriptivos de variables dependientes e independientes por tipo de cargo") %>%
  add_footer_lines("Fuente: Elaboración propia basada en datos del SERVEL, SINIM e INE.") %>%
  autofit()

save_as_docx(ft_combinada, path = "tabla_descriptiva_combinada.docx")



# Variables de control redondeado

descriptivo_control <- base_unida %>%
  distinct(comuna, .keep_all = TRUE) %>%
  summarise(
    media_pobreza        = round(mean(indice_de_pobreza_casen, na.rm = TRUE), 2),
    sd_pobreza           = round(sd(indice_de_pobreza_casen, na.rm = TRUE), 2),
    media_escolaridad    = round(mean(anios_escolaridad_promedio, na.rm = TRUE), 2),
    sd_escolaridad       = round(sd(anios_escolaridad_promedio, na.rm = TRUE), 2),
    media_rural          = round(mean(poblacion_rural, na.rm = TRUE), 2),
    sd_rural             = round(sd(poblacion_rural, na.rm = TRUE), 2),
    media_participacion  = round(mean(porcentaje_participacion2021, na.rm = TRUE), 2),
    sd_participacion     = round(sd(porcentaje_participacion2021, na.rm = TRUE), 2)
  )

print(descriptivo_control)


datasummary_df(descriptivo_control,
               output = "aloooooooo3.docx",
               title = "Tabla 1..",
               notes = "Fuente: Elaboración propia basada en datos de Sinim, servel, casen.")



# Gráficos descriptivos por siacaso 

# Voto nulo por cargo
ggplot(base_unida, aes(x = eleccion, y = porcentaje_nulos, fill = eleccion)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  labs(
    title = "Distribución del voto nulo por tipo de cargo",
    x = "Cargo",
    y = "Porcentaje de votos nulos"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Voto blanco x cargo
ggplot(base_unida, aes(x = eleccion, y = porcentaje_blancos, fill = eleccion)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  labs(
    title = "Distribución del voto blanco por tipo de cargo",
    x = "Cargo",
    y = "Porcentaje de votos blancos"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Medias de nulos y blancos por cargo (gráfico de barras)
base_unida %>%
  group_by(eleccion) %>%
  summarise(
    nulos   = mean(porcentaje_nulos, na.rm = TRUE),
    blancos = mean(porcentaje_blancos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(nulos, blancos), names_to = "tipo", values_to = "porcentaje") %>%
  ggplot(aes(x = eleccion, y = porcentaje, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Promedio de voto inválido por tipo de cargo",
    x = "Cargo",
    y = "Porcentaje promedio",
    fill = "Tipo de voto"
  ) +
  theme_minimal()


# ANÁLISIS BIVARIADO — Correlaciones de Pearson

cargos_biv <- c("Alcaldes", "Concejales", "CORE", "Gobernadores")

vars_ind <- c(
  "prop_indep_fuera_pacto",
  "candidatos",
  "indice_de_pobreza_casen",
  "anios_escolaridad_promedio",
  "poblacion_rural",
  "porcentaje_participacion2021"
)

vars_dep <- c("porcentaje_nulos", "porcentaje_blancos", "pct_invalidos")

nombres_ind <- c(
  "prop_indep_fuera_pacto"       = "Prop. independientes fuera de pacto",
  "candidatos"                   = "Número de candidatos",
  "indice_de_pobreza_casen"      = "Índice de pobreza (CASEN)",
  "anios_escolaridad_promedio"   = "Años de escolaridad promedio",
  "poblacion_rural"              = "Población rural",
  "porcentaje_participacion2021" = "Participación electoral 2021"
)

tabla_biv_completa <- map_dfr(cargos_biv, function(cargo) {
  datos_cargo <- base_unida %>% filter(eleccion == cargo)
  map_dfr(vars_ind, function(vi) {
    fila <- tibble(Cargo = cargo, Variable = nombres_ind[[vi]])
    for (vd in vars_dep) {
      test <- cor.test(datos_cargo[[vi]], datos_cargo[[vd]],
                       method = "pearson", use = "complete.obs")
      sig <- case_when(
        test$p.value < 0.01 ~ "***",
        test$p.value < 0.05 ~ "**",
        test$p.value < 0.1  ~ "*",
        TRUE                ~ ""
      )
      fila[[vd]] <- paste0(sprintf("%.3f", test$estimate), sig)
    }
    fila
  })
})

colnames(tabla_biv_completa) <- c("Cargo", "Variable", "Voto nulo", "Voto blanco", "Voto inválido")

print(tabla_biv_completa)

ft_biv <- flextable(tabla_biv_completa) %>%
  merge_v(j = "Cargo") %>%
  set_caption("Correlaciones de Pearson entre variables independientes y voto inválido por cargo") %>%
  add_footer_lines("* p<0.1; ** p<0.05; *** p<0.01. Fuente: Elaboración propia.") %>%
  theme_vanilla() %>%
  autofit()

save_as_docx(ft_biv, path = "tabla_bivariado.docx")

# Scatterplot 1: candidatos (log) vs voto nulo por cargo
p_candidatos_nulos <- ggplot(base_unida, aes(x = log(candidatos), y = porcentaje_nulos)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~ eleccion, scales = "free") +
  labs(
    title = "Número de candidatos y voto nulo por cargo",
    x = "Número de candidatos (log)",
    y = "% Voto nulo"
  ) +
  theme_minimal()

print(p_candidatos_nulos)

# Scatterplot 2: prop. independientes vs voto inválido por cargo
p_indep_invalidos <- ggplot(base_unida, aes(x = prop_indep_fuera_pacto, y = pct_invalidos)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  facet_wrap(~ eleccion, scales = "free") +
  labs(
    title = "Prop. independientes fuera de pacto y voto inválido por cargo",
    x = "Prop. independientes fuera de pacto",
    y = "% Voto inválido"
  ) +
  theme_minimal()

print(p_indep_invalidos)

# Histogramas: candidatos sin transformar vs con log — justificación visual
p_hist_raw <- base_unida %>%
  ggplot(aes(x = candidatos)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ eleccion, scales = "free") +
  labs(
    title = "Distribución del número de candidatos (sin transformar)",
    x = "N° de candidatos",
    y = "Frecuencia"
  ) +
  theme_minimal()

p_hist_log <- base_unida %>%
  ggplot(aes(x = log(candidatos))) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  facet_wrap(~ eleccion, scales = "free") +
  labs(
    title = "Distribución del número de candidatos (logaritmo natural)",
    x = "log(N° de candidatos)",
    y = "Frecuencia"
  ) +
  theme_minimal()

p_hist_comparado <- p_hist_raw / p_hist_log
print(p_hist_comparado)

ggsave("histograma_candidatos_log.png", plot = p_hist_comparado,
       width = 10, height = 8, dpi = 300)


# Diagnóstico de multicolinealidad usando log(candidatos)
cor_data_prev <- base_unida %>%
  mutate(log_candidatos = log(candidatos)) %>%
  select(prop_indep_fuera_pacto, log_candidatos,
         indice_de_pobreza_casen, anios_escolaridad_promedio,
         poblacion_rural, porcentaje_participacion2021) %>%
  drop_na()

# Valores entre -1 0 1
matriz_cor <- cor(cor_data_prev) %>% round(2)

print(matriz_cor)


summary(base_unida$poblacion_rural)


# regresion lineal multiple ols con errores robustos x cargo

# Candidatos por escaño disponible (sin log: ajusta igual o mejor que
# log(candidatos) en los 3 modelos)
base_unida <- base_unida %>%
  mutate(candidatos_por_escano = candidatos / escanos)

formula_nulos <- porcentaje_nulos ~ prop_indep_fuera_pacto + candidatos_por_escano +
  indice_de_pobreza_casen + anios_escolaridad_promedio + poblacion_rural + porcentaje_participacion2021

formula_blancos <- porcentaje_blancos ~ prop_indep_fuera_pacto + candidatos_por_escano +
  indice_de_pobreza_casen + anios_escolaridad_promedio + poblacion_rural + porcentaje_participacion2021

formula_invalidos <- pct_invalidos ~ prop_indep_fuera_pacto + candidatos_por_escano +
  indice_de_pobreza_casen + anios_escolaridad_promedio + poblacion_rural + porcentaje_participacion2021

cargos <- c("Alcaldes", "Concejales", "CORE", "Gobernadores")

modelos_nulos     <- list()
modelos_blancos   <- list()
modelos_invalidos <- list()

for (cargo in cargos) {
  datos_cargo <- base_unida %>% filter(eleccion == cargo)
  modelos_nulos[[cargo]]     <- lm(formula_nulos,     data = datos_cargo)
  modelos_blancos[[cargo]]   <- lm(formula_blancos,   data = datos_cargo)
  modelos_invalidos[[cargo]] <- lm(formula_invalidos, data = datos_cargo)
}

# Wild cluster bootstrap vcov — para Gobernadores con N=16 clusters
# Usa sandwich::vcovBS() con pesos Rademacher (wild bootstrap clusterizado)
wcb_vcov <- function(model, datos, R = 9999) {
  omitted      <- na.action(model)
  cluster_used <- if (is.null(omitted)) datos$region else datos$region[-omitted]
  sandwich::vcovBS(model, cluster = cluster_used, type = "rademacher", R = R)
}

# Pre-cómputo bootstrap Gobernadores (se reutiliza en diagnóstico y tablas)
cat("Calculando wild cluster bootstrap para Gobernadores (puede tardar unos minutos)...\n")
datos_gob <- base_unida %>% filter(eleccion == "Gobernadores")
vcov_wcb_nulos_gob     <- wcb_vcov(modelos_nulos[["Gobernadores"]],     datos_gob)
vcov_wcb_blancos_gob   <- wcb_vcov(modelos_blancos[["Gobernadores"]],   datos_gob)
vcov_wcb_invalidos_gob <- wcb_vcov(modelos_invalidos[["Gobernadores"]], datos_gob)
cat("Bootstrap Gobernadores completado.\n")

# Resultados con errores robustos con HC3 para Alcaldes y Concejales, cluster para CORE y Gobernadores
for (cargo in cargos) {
  datos_cargo <- base_unida %>% filter(eleccion == cargo)
  
  if (cargo == "Gobernadores") {
    vcov_nulos     <- vcov_wcb_nulos_gob
    vcov_blancos   <- vcov_wcb_blancos_gob
    vcov_invalidos <- vcov_wcb_invalidos_gob
  } else if (cargo == "CORE") {
    vcov_nulos     <- vcovCR(modelos_nulos[[cargo]],     cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
    vcov_blancos   <- vcovCR(modelos_blancos[[cargo]],   cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
    vcov_invalidos <- vcovCR(modelos_invalidos[[cargo]], cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
  } else {
    vcov_nulos     <- vcovHC(modelos_nulos[[cargo]],     type = "HC3")
    vcov_blancos   <- vcovHC(modelos_blancos[[cargo]],   type = "HC3")
    vcov_invalidos <- vcovHC(modelos_invalidos[[cargo]], type = "HC3")
  }
  
  cat("\n====", cargo, "- VOTO NULO ====\n")
  print(coeftest(modelos_nulos[[cargo]],     vcov = vcov_nulos))
  cat("\n====", cargo, "- VOTO BLANCO ====\n")
  print(coeftest(modelos_blancos[[cargo]],   vcov = vcov_blancos))
  cat("\n====", cargo, "- VOTO INVÁLIDO (nulos + blancos) ====\n")
  print(coeftest(modelos_invalidos[[cargo]], vcov = vcov_invalidos))
}

# R cuadrado ajustado por modelo
cat("\n==== R2 ajustado por modelo prueba si esta bn ====\n")
for (cargo in cargos) {
  cat(cargo, "- Nulos:    ", round(summary(modelos_nulos[[cargo]])$adj.r.squared,     3), "\n")
  cat(cargo, "- Blancos:  ", round(summary(modelos_blancos[[cargo]])$adj.r.squared,   3), "\n")
  cat(cargo, "- Inválidos:", round(summary(modelos_invalidos[[cargo]])$adj.r.squared, 3), "\n")
}


# TABLAS DE REGRESIÓN PARA investigacionsita

vcov_nulos     <- list()
vcov_blancos   <- list()
vcov_invalidos <- list()

for (cargo in cargos) {
  datos_cargo <- base_unida %>% filter(eleccion == cargo)
  
  if (cargo == "Gobernadores") {
    vcov_nulos[[cargo]]     <- vcov_wcb_nulos_gob
    vcov_blancos[[cargo]]   <- vcov_wcb_blancos_gob
    vcov_invalidos[[cargo]] <- vcov_wcb_invalidos_gob
  } else if (cargo == "CORE") {
    vcov_nulos[[cargo]]     <- vcovCR(modelos_nulos[[cargo]],     cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
    vcov_blancos[[cargo]]   <- vcovCR(modelos_blancos[[cargo]],   cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
    vcov_invalidos[[cargo]] <- vcovCR(modelos_invalidos[[cargo]], cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
  } else {
    vcov_nulos[[cargo]]     <- vcovHC(modelos_nulos[[cargo]],     type = "HC3")
    vcov_blancos[[cargo]]   <- vcovHC(modelos_blancos[[cargo]],   type = "HC3")
    vcov_invalidos[[cargo]] <- vcovHC(modelos_invalidos[[cargo]], type = "HC3")
  }
}

nombres_vars <- c(
  "prop_indep_fuera_pacto"       = "Prop. independientes fuera de pacto",
  "candidatos_por_escano"        = "Candidatos por escaño",
  "indice_de_pobreza_casen"      = "Índice de pobreza (CASEN)",
  "anios_escolaridad_promedio"   = "Años de escolaridad promedio",
  "poblacion_rural"              = "Población rural",
  "porcentaje_participacion2021" = "Participación electoral 2021",
  "(Intercept)"                  = "Constante"
)

gof_tabla <- tribble(
  ~raw,            ~clean,        ~fmt,
  "nobs",          "N",            0,
  "adj.r.squared", "R² ajustado",  3
)

modelsummary(
  models      = modelos_nulos,
  vcov        = vcov_nulos,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = nombres_vars,
  gof_map     = gof_tabla,
  title       = "Tabla X. Determinantes del voto nulo — Regresión OLS con errores robustos",
  notes       = "Errores estándar robustos HC3 (Alcaldes y Concejales), CR2 clusterizado por circunscripción provincial (CORE) y wild cluster bootstrap con 9.999 réplicas, distribución de Rademacher (Gobernadores, N=16 regiones). * p<0.1; ** p<0.05; *** p<0.01.",
  output      = "tabla_voto_nulo.docx"
)

modelsummary(
  models      = modelos_blancos,
  vcov        = vcov_blancos,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = nombres_vars,
  gof_map     = gof_tabla,
  title       = "Tabla X. Determinantes del voto blanco — Regresión OLS con errores robustos",
  notes       = "Errores estándar robustos HC3 (Alcaldes y Concejales), CR2 clusterizado por circunscripción provincial (CORE) y wild cluster bootstrap con 9.999 réplicas, distribución de Rademacher (Gobernadores, N=16 regiones). * p<0.1; ** p<0.05; *** p<0.01.",
  output      = "tabla_voto_blanco.docx"
)

modelsummary(
  models      = modelos_invalidos,
  vcov        = vcov_invalidos,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = nombres_vars,
  gof_map     = gof_tabla,
  title       = "Tabla X. Determinantes del voto inválido (nulos + blancos) — Regresión OLS con errores robustos",
  notes       = "Errores estándar robustos HC3 (Alcaldes y Concejales), CR2 clusterizado por circunscripción provincial (CORE) y wild cluster bootstrap con 9.999 réplicas, distribución de Rademacher (Gobernadores, N=16 regiones). * p<0.1; ** p<0.05; *** p<0.01.",
  output      = "tabla_voto_invalido.docx"
)



# diagnpara saber si se puede confiar en el modelo


# correlaciones entre rpedictores

# Se usa candidatos_por_escano para coincidir con la especificación del modelo
cor_data <- base_unida %>%
  select(prop_indep_fuera_pacto, candidatos_por_escano,
         indice_de_pobreza_casen, anios_escolaridad_promedio,
         poblacion_rural, porcentaje_participacion2021) %>%
  drop_na()

matriz_cor <- cor(cor_data) %>% round(2)

print(matriz_cor)
cat("Nota: correlaciones > 0.7 en valor absoluto indican posible multicolinealidad.\n")


# VIF para saber si hay multicolinealidad
# (solo depende de los predictores, no de la VD — se calcula una vez por cargo)

tipos_modelos <- list(
  Nulos     = modelos_nulos,
  Blancos   = modelos_blancos,
  Invalidos = modelos_invalidos
)

for (cargo in cargos) {
  vif_vals <- tryCatch(car::vif(modelos_nulos[[cargo]]), error = function(e) NA)
  if (!any(is.na(vif_vals))) {
    vmax   <- round(max(vif_vals), 2)
    estado <- ifelse(vmax > 10, "PROBLEMA GRAVE",
                     ifelse(vmax > 5, "MODERADO", "OK"))
    cat(sprintf("  %-15s VIF máximo: %.2f -> %s\n", cargo, vmax, estado))
    print(round(vif_vals, 2))
  }
}
cat("\n")


# TEST DE HETEROCEDASTICIDAD BREUSCH-PAGAN )(variación de los errores por n)

for (tipo_nombre in names(tipos_modelos)) {
  modelos <- tipos_modelos[[tipo_nombre]]
  cat("-- VD:", toupper(tipo_nombre), "--\n")
  for (cargo in cargos) {
    bp <- tryCatch(lmtest::bptest(modelos[[cargo]]), error = function(e) NULL)
    if (!is.null(bp)) {
      estado <- ifelse(bp$p.value < 0.05, "HETEROCED. detectada -> usar errores robustos",
                       "Homocedasticidad OK")
      cat(sprintf("  %-15s BP p-valor: %.4f -> %s\n", cargo, bp$p.value, estado))
    }
  }
  cat("\n")
}


#test para ver NORMALIDAD en los RESIDUOS SHAPIRO-WILK

for (tipo_nombre in names(tipos_modelos)) {
  modelos <- tipos_modelos[[tipo_nombre]]
  cat("-- VD:", toupper(tipo_nombre), "--\n")
  for (cargo in cargos) {
    resid_m <- residuals(modelos[[cargo]])
    sw <- tryCatch(shapiro.test(resid_m), error = function(e) NULL)
    if (!is.null(sw)) {
      estado <- ifelse(sw$p.value < 0.05, "No normal",
                       "Normalidad OK")
      cat(sprintf("  %-15s SW p-valor: %.4f -> %s\n", cargo, sw$p.value, estado))
    }
  }
  cat("\n")
}


# OBSERVACIONES INFLUYENTES DISTANCIA DE COOK (no tan importante creo) 

for (tipo_nombre in names(tipos_modelos)) {
  modelos <- tipos_modelos[[tipo_nombre]]
  cat("-- VD:", toupper(tipo_nombre), "--\n")
  for (cargo in cargos) {
    m     <- modelos[[cargo]]
    cook  <- cooks.distance(m)
    n     <- length(cook)
    n_inf <- sum(cook > 4 / n)
    pct   <- round(100 * n_inf / n, 1)
    estado <- ifelse(pct > 10, "ALTO — revisar outliers",
                     ifelse(pct > 5, "Moderado — mencionar en limitaciones", "OK"))
    cat(sprintf("  %-15s n=%d | influyentes: %d (%.1f%%) -> %s\n",
                cargo, n, n_inf, pct, estado))
  }
  cat("\n")
}

# pequeño analisis de R2 AJUSTADO 


for (cargo in cargos) {
  r2_n <- round(summary(modelos_nulos[[cargo]])$adj.r.squared,     3)
  r2_b <- round(summary(modelos_blancos[[cargo]])$adj.r.squared,   3)
  r2_i <- round(summary(modelos_invalidos[[cargo]])$adj.r.squared, 3)
  cat(sprintf("%-15s  Nulos: %.3f | Blancos: %.3f | Inválidos: %.3f\n",
              cargo, r2_n, r2_b, r2_i))
}




# COMPROBACIÓN DE LÍMITES OLS (Predicciones vs Realidad)

for (tipo_nombre in names(tipos_modelos)) {
  modelos <- tipos_modelos[[tipo_nombre]]
  cat("-- VD:", toupper(tipo_nombre), "--\n")
  for (cargo in cargos) {
    predicciones <- fitted(modelos[[cargo]])
    min_pred     <- min(predicciones, na.rm = TRUE)
    max_pred     <- max(predicciones, na.rm = TRUE)
    cat(sprintf("  %-15s Min: %.4f | Max: %.4f", cargo, min_pred, max_pred))
    if (min_pred < 0 | max_pred > 1) {
      cat("  [!] Valores imposibles (<0 o >1)\n")
    } else {
      cat("  [OK]\n")
    }
  }
  cat("\n")
}


# TEST DE MORAN I — INDEPENDENCIA DE ERRORES (AUTOCORRELACIÓN ESPACIAL)

# Mapa comunal de chilemapas convertido a sf (join por código, sin riesgo de typos en nombres)
mapa_sf <- sf::st_as_sf(chilemapas::mapa_comunas)

morans_cargo <- function(modelo, cargo_nombre) {
  datos <- base_unida %>% filter(eleccion == cargo_nombre)
  
  # Reproducir exactamente las filas que usó lm() (descarta NAs igual que el modelo)
  omitidas   <- na.action(modelo)
  mask       <- if (is.null(omitidas)) rep(TRUE, nrow(datos)) else
    !(seq_len(nrow(datos)) %in% omitidas)
  datos_used <- datos[mask, ]
  res        <- residuals(modelo)
  
  idx    <- match(datos_used$codigo_comuna, mapa_sf$codigo_comuna)
  valido <- !is.na(idx)
  
  mapa_ord <- mapa_sf[idx[valido], ]
  res_ok   <- res[valido]
  
  nb <- poly2nb(mapa_ord, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  tryCatch(
    moran.test(res_ok, lw, zero.policy = TRUE),
    error = function(e) {
      cat(sprintf("    [error] %s\n", conditionMessage(e)))
      NULL
    }
  )
}


for (tipo_nombre in names(tipos_modelos)) {
  modelos <- tipos_modelos[[tipo_nombre]]
  cat("-- VD:", toupper(tipo_nombre), "--\n")
  for (cargo in cargos) {
    resultado <- morans_cargo(modelos[[cargo]], cargo)
    if (!is.null(resultado)) {
      I_val  <- resultado$estimate[["Moran I statistic"]]
      estado <- ifelse(resultado$p.value < 0.05,
                       "Autocorrelación espacial detectada",
                       "Sin autocorrelación espacial OK")
      cat(sprintf("  %-15s I=%.4f | p-valor: %.4f -> %s\n",
                  cargo, I_val, resultado$p.value, estado))
    } else {
      cat(sprintf("  %-15s No se pudo calcular\n", cargo))
    }
  }
  cat("\n")
}



#experimento — candidatos_por_escano = candidatos / escanos
# Nulo y blanco usan la versión sin log (ajusta igual o mejor); inválido
# se deja con log como comparación de robustez frente al modelo principal.

formula_nulos_exp <- porcentaje_nulos ~ prop_indep_fuera_pacto + candidatos_por_escano +
  indice_de_pobreza_casen + anios_escolaridad_promedio + poblacion_rural + porcentaje_participacion2021

formula_blancos_exp <- porcentaje_blancos ~ prop_indep_fuera_pacto + candidatos_por_escano +
  indice_de_pobreza_casen + anios_escolaridad_promedio + poblacion_rural + porcentaje_participacion2021

formula_invalidos_exp <- pct_invalidos ~ prop_indep_fuera_pacto + log(candidatos_por_escano) +
  indice_de_pobreza_casen + anios_escolaridad_promedio + poblacion_rural + porcentaje_participacion2021

modelos_nulos_exp     <- list()
modelos_blancos_exp   <- list()
modelos_invalidos_exp <- list()

for (cargo in cargos) {
  datos_cargo <- base_unida %>% filter(eleccion == cargo)
  modelos_nulos_exp[[cargo]]     <- lm(formula_nulos_exp,     data = datos_cargo)
  modelos_blancos_exp[[cargo]]   <- lm(formula_blancos_exp,   data = datos_cargo)
  modelos_invalidos_exp[[cargo]] <- lm(formula_invalidos_exp, data = datos_cargo)
}

# #experimento — VIF: candidatos_por_escano vs log(candidatos)
cat("\n#experimento — VIF maximo: log(candidatos) vs candidatos_por_escano\n")
for (cargo in cargos) {
  vif_orig <- tryCatch(round(max(car::vif(modelos_nulos[[cargo]])),     2), error = function(e) NA)
  vif_exp  <- tryCatch(round(max(car::vif(modelos_nulos_exp[[cargo]])), 2), error = function(e) NA)
  cat(sprintf("  %-15s Original: %.2f | Experimento: %.2f\n", cargo, vif_orig, vif_exp))
}

# Errores robustos experimento (misma lógica que el modelo original)
vcov_nulos_exp     <- list()
vcov_blancos_exp   <- list()
vcov_invalidos_exp <- list()

# Pre-cómputo bootstrap Gobernadores para modelos experimento
cat("Calculando wild cluster bootstrap para Gobernadores (experimento)...\n")
vcov_wcb_nulos_exp_gob     <- wcb_vcov(modelos_nulos_exp[["Gobernadores"]],     datos_gob)
vcov_wcb_blancos_exp_gob   <- wcb_vcov(modelos_blancos_exp[["Gobernadores"]],   datos_gob)
vcov_wcb_invalidos_exp_gob <- wcb_vcov(modelos_invalidos_exp[["Gobernadores"]], datos_gob)
cat("Bootstrap Gobernadores (experimento) completado.\n")

for (cargo in cargos) {
  datos_cargo <- base_unida %>% filter(eleccion == cargo)
  
  if (cargo == "Gobernadores") {
    vcov_nulos_exp[[cargo]]     <- vcov_wcb_nulos_exp_gob
    vcov_blancos_exp[[cargo]]   <- vcov_wcb_blancos_exp_gob
    vcov_invalidos_exp[[cargo]] <- vcov_wcb_invalidos_exp_gob
  } else if (cargo == "CORE") {
    vcov_nulos_exp[[cargo]]     <- vcovCR(modelos_nulos_exp[[cargo]],     cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
    vcov_blancos_exp[[cargo]]   <- vcovCR(modelos_blancos_exp[[cargo]],   cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
    vcov_invalidos_exp[[cargo]] <- vcovCR(modelos_invalidos_exp[[cargo]], cluster = datos_cargo$circunscripcion_provincial, type = "CR2")
  } else {
    vcov_nulos_exp[[cargo]]     <- vcovHC(modelos_nulos_exp[[cargo]],     type = "HC3")
    vcov_blancos_exp[[cargo]]   <- vcovHC(modelos_blancos_exp[[cargo]],   type = "HC3")
    vcov_invalidos_exp[[cargo]] <- vcovHC(modelos_invalidos_exp[[cargo]], type = "HC3")
  }
}

nombres_vars_exp <- c(
  "prop_indep_fuera_pacto"       = "Prop. independientes fuera de pacto",
  "candidatos_por_escano"        = "Candidatos por escaño",
  "log(candidatos_por_escano)"   = "Candidatos por escaño (log)",
  "indice_de_pobreza_casen"      = "Índice de pobreza (CASEN)",
  "anios_escolaridad_promedio"   = "Años de escolaridad promedio",
  "poblacion_rural"              = "Población rural",
  "porcentaje_participacion2021" = "Participación electoral 2021",
  "(Intercept)"                  = "Constante"
)

modelsummary(
  models      = modelos_nulos_exp,
  vcov        = vcov_nulos_exp,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = nombres_vars_exp,
  gof_map     = gof_tabla,
  title       = "[EXPERIMENTO] Determinantes del voto nulo — candidatos por escano",
  notes       = "Errores estándar robustos HC3 (Alcaldes y Concejales), CR2 clusterizado por circunscripción provincial (CORE) y wild cluster bootstrap con 9.999 réplicas, distribución de Rademacher (Gobernadores, N=16 regiones). * p<0.1; ** p<0.05; *** p<0.01.",
  output      = "exp_tabla_voto_nulo.docx"
)

modelsummary(
  models      = modelos_blancos_exp,
  vcov        = vcov_blancos_exp,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = nombres_vars_exp,
  gof_map     = gof_tabla,
  title       = "[EXPERIMENTO] Determinantes del voto blanco — candidatos por escano",
  notes       = "Errores estándar robustos HC3 (Alcaldes y Concejales), CR2 clusterizado por circunscripción provincial (CORE) y wild cluster bootstrap con 9.999 réplicas, distribución de Rademacher (Gobernadores, N=16 regiones). * p<0.1; ** p<0.05; *** p<0.01.",
  output      = "exp_tabla_voto_blanco.docx"
)

modelsummary(
  models      = modelos_invalidos_exp,
  vcov        = vcov_invalidos_exp,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = nombres_vars_exp,
  gof_map     = gof_tabla,
  title       = "[EXPERIMENTO] Determinantes del voto invalido — candidatos por escano (log)",
  notes       = "Errores estándar robustos HC3 (Alcaldes y Concejales), CR2 clusterizado por circunscripción provincial (CORE) y wild cluster bootstrap con 9.999 réplicas, distribución de Rademacher (Gobernadores, N=16 regiones). * p<0.1; ** p<0.05; *** p<0.01.",
  output      = "exp_tabla_voto_invalido.docx"
)

# #experimento — R² ajustado comparado
cat("\n#experimento — R² ajustado (voto nulo): candidatos vs candidatos_por_escano\n")
for (cargo in cargos) {
  r2_orig <- round(summary(modelos_nulos[[cargo]])$adj.r.squared,     3)
  r2_exp  <- round(summary(modelos_nulos_exp[[cargo]])$adj.r.squared, 3)
  cat(sprintf("  %-15s Original: %.3f | Experimento: %.3f\n", cargo, r2_orig, r2_exp))
}



# MODELO DE ERROR ESPACIAL (SEM) — ANÁLISIS DE ROBUSTEZ
# Mismos predictores que el OLS. Se presentan como tablas independientes.


mapa_sf <- sf::st_as_sf(chilemapas::mapa_comunas)

construir_lw_sem <- function(datos_cargo, modelo_ols) {
  omitidas   <- na.action(modelo_ols)
  mask       <- if (is.null(omitidas)) rep(TRUE, nrow(datos_cargo)) else
    !(seq_len(nrow(datos_cargo)) %in% omitidas)
  datos_used <- datos_cargo[mask, ]
  idx        <- match(datos_used$codigo_comuna, mapa_sf$codigo_comuna)
  valido     <- !is.na(idx)
  mapa_ord   <- mapa_sf[idx[valido], ]
  nb         <- poly2nb(mapa_ord, queen = TRUE)
  lw         <- nb2listw(nb, style = "W", zero.policy = TRUE)
  list(lw = lw, datos = datos_used[valido, ])
}

modelos_sem_nulos     <- list()
modelos_sem_blancos   <- list()
modelos_sem_invalidos <- list()

for (cargo in cargos) {
  datos_cargo <- base_unida %>% filter(eleccion == cargo)
  lw_info     <- construir_lw_sem(datos_cargo, modelos_nulos[[cargo]])
  
  modelos_sem_nulos[[cargo]] <- tryCatch(
    errorsarlm(formula_nulos,     data = lw_info$datos, listw = lw_info$lw, zero.policy = TRUE),
    error = function(e) NULL
  )
  modelos_sem_blancos[[cargo]] <- tryCatch(
    errorsarlm(formula_blancos,   data = lw_info$datos, listw = lw_info$lw, zero.policy = TRUE),
    error = function(e) NULL
  )
  modelos_sem_invalidos[[cargo]] <- tryCatch(
    errorsarlm(formula_invalidos, data = lw_info$datos, listw = lw_info$lw, zero.policy = TRUE),
    error = function(e) NULL
  )
}

# Función para generar tabla SEM limpia (sin comparación)
tabla_sem <- function(modelos_sem, titulo, path_output) {
  
  vars_orden <- c("(Intercept)", "prop_indep_fuera_pacto", "candidatos_por_escano",
                  "indice_de_pobreza_casen", "anios_escolaridad_promedio",
                  "poblacion_rural", "porcentaje_participacion2021")
  
  nombres_vars_sem <- c(
    "(Intercept)"                  = "Constante",
    "prop_indep_fuera_pacto"       = "Prop. independientes fuera de pacto",
    "candidatos_por_escano"        = "Candidatos por escaño",
    "indice_de_pobreza_casen"      = "Índice de pobreza (CASEN)",
    "anios_escolaridad_promedio"   = "Años de escolaridad promedio",
    "poblacion_rural"              = "Población rural",
    "porcentaje_participacion2021" = "Participación electoral 2021"
  )
  
  stars_sem <- function(p) {
    ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                                   ifelse(p < 0.1, "*", "")))
  }
  
  filas <- list()
  
  # Coeficientes
  for (v in vars_orden) {
    fila_c <- tibble(Variable = nombres_vars_sem[v])
    fila_s <- tibble(Variable = "")
    for (cargo in cargos) {
      m <- modelos_sem[[cargo]]
      if (!is.null(m)) {
        sc <- summary(m)$Coef
        if (v %in% rownames(sc)) {
          fila_c[[cargo]] <- paste0(sprintf("%.4f", sc[v, "Estimate"]),
                                    stars_sem(sc[v, "Pr(>|z|)"]))
          fila_s[[cargo]] <- paste0("(", sprintf("%.4f", sc[v, "Std. Error"]), ")")
        } else {
          fila_c[[cargo]] <- "—"; fila_s[[cargo]] <- ""
        }
      } else {
        fila_c[[cargo]] <- "N/D"; fila_s[[cargo]] <- ""
      }
    }
    filas[[length(filas) + 1]] <- fila_c
    filas[[length(filas) + 1]] <- fila_s
  }
  
  # Lambda
  fila_lam <- tibble(Variable = "Lambda espacial (λ)")
  fila_lse <- tibble(Variable = "")
  for (cargo in cargos) {
    m <- modelos_sem[[cargo]]
    if (!is.null(m)) {
      s  <- summary(m)
      lz <- m$lambda / s$lambda.se
      lp <- 2 * pnorm(-abs(lz))
      fila_lam[[cargo]] <- paste0(sprintf("%.4f", m$lambda), stars_sem(lp))
      fila_lse[[cargo]] <- paste0("(", sprintf("%.4f", s$lambda.se), ")")
    } else {
      fila_lam[[cargo]] <- "N/D"; fila_lse[[cargo]] <- ""
    }
  }
  filas[[length(filas) + 1]] <- fila_lam
  filas[[length(filas) + 1]] <- fila_lse
  
  # Estadísticos
  fila_n   <- tibble(Variable = "N")
  fila_aic <- tibble(Variable = "AIC")
  for (cargo in cargos) {
    m <- modelos_sem[[cargo]]
    fila_n[[cargo]]   <- if (!is.null(m)) as.character(length(residuals(m))) else "N/D"
    fila_aic[[cargo]] <- if (!is.null(m)) sprintf("%.1f", AIC(m))           else "N/D"
  }
  filas[[length(filas) + 1]] <- fila_n
  filas[[length(filas) + 1]] <- fila_aic
  
  df <- bind_rows(filas) %>% select(Variable, all_of(cargos))
  df[is.na(df)] <- ""
  
  ft <- flextable(df) %>%
    set_header_labels(Variable = "Variable") %>%
    theme_vanilla() %>%
    align(align = "center", part = "header") %>%
    align(j = 2:5, align = "center", part = "body") %>%
    align(j = 1,   align = "left",   part = "body") %>%
    hline(i = which(df$Variable == "Lambda espacial (λ)"), part = "body") %>%
    hline(i = which(df$Variable == "N"),                   part = "body") %>%
    bold(i = which(df$Variable == "Lambda espacial (λ)"), j = 1, part = "body") %>%
    fontsize(size = 9, part = "all") %>%
    set_caption(titulo) %>%
    add_footer_lines(
      "Modelo de error espacial (SEM) estimado por Máxima Verosimilitud con matriz de pesos de reina (queen). * p<0.1; ** p<0.05; *** p<0.01. Errores estándar entre paréntesis. Fuente: Elaboración propia basada en datos del SERVEL, SINIM e INE."
    ) %>%
    autofit()
  
  save_as_docx(ft, path = path_output)
}

tabla_sem(modelos_sem_nulos,
          titulo      = "Tabla X. Determinantes del voto nulo — Modelo de error espacial (SEM)",
          path_output = "sem_voto_nulo.docx")

tabla_sem(modelos_sem_blancos,
          titulo      = "Tabla X. Determinantes del voto blanco — Modelo de error espacial (SEM)",
          path_output = "sem_voto_blanco.docx")

tabla_sem(modelos_sem_invalidos,
          titulo      = "Tabla X. Determinantes del voto inválido (nulos + blancos) — Modelo de error espacial (SEM)",
          path_output = "sem_voto_invalido.docx")






write.csv(base_unida,
          "base_unida.csv",
          row.names = FALSE)




names(base_unida)













