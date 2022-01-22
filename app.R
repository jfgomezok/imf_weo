
##############################################################################
################ WEO IMF DATA ANALYSIS TOOLKIT ###############################
##############################################################################

# 1) Initial Config  -----------------------------------------------------------


# Load packages
if ( !require("pacman") ) install.packages("pacman")
pacman::p_load(tidyverse, glue, ggrepel, tidytext,
               ggtext, lubridate, extrafont)


# Load helpers
source("src/weo_parser.R")
source("src/wb_countries_income_levels.R")


# Load fonts
loadfonts(device = "win", quiet = TRUE)



# 2) & 3) Import and Cleaning -------------------------------------------------------------

# First must download WEO databases (db) and save it in folder "data" !!!!!!!!!
# You can use individual db or  aggregates db

if (!file.exists(file.path("data"))){dir.create(file.path("data"))}

files.toimport <- list.files(
  path       = "data/",
  all.files  = FALSE,
  pattern    = "\\.xls$",
  full.names = FALSE,
  recursive  = FALSE
)

if ( length(files.toimport) == 0 ){stop("Please download WEO databases and save them on 'data' folder'")}

weo_data <- purrr::map_df(files.toimport, weo_parser)




# 4) Transformations  --------------------------------------------------------------

variables_interest <- c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIPCH", "GGXWDG_NGDP")
geo_iso_interest <- "ARG"
geo_name <- unique(weo_data[weo_data$geo_iso==geo_iso_interest,1]) %>% na.omit() %>% pull()

prep_data <- weo_data %>% 
  filter(
    geo_iso == geo_iso_interest
    & variable_code %in% variables_interest
    & year >= 2016
  ) %>% 
  select(-variable) %>% 
  mutate(
    year = ymd(glue("{year}0101")),
    weo_date = factor(weo_date, levels = c("Oct-2019", "Apr-2020","Oct-2020", "Apr-2021", "Oct-2021")),
    variable_to_show = case_when(
      variable_code == "LUR" ~ "<b>Desempleo</b><br>Porcentaje de la PEA",
      variable_code == "NGDP_RPCH" ~ "<b>Crecimiento del PBI</b><br>Precios constantes",
      variable_code == "GGXONLB_NGDP" ~ "<b>Resultado Primario del Sector Público</b><br>Porcentaje del PBI",
      variable_code == "BCA_NGDPD" ~ "<b>Resultado Cuenta Corriente</b><br>Porcentaje del PBI",
      variable_code == "PCPIPCH" ~ "<b>Inflación</b><br>Variación Interanual, fin de período",
      variable_code == "GGXWDG_NGDP" ~ "<b>Deuda Bruta del Gobierno General</b><br>Porcentaje del PBI"
    ),
    estimate = case_when(
      year(year) >= first_estimate ~ TRUE,
      TRUE ~ FALSE
    )
  )


data_aux <- prep_data %>% 
  filter(first_estimate == year(year)) %>% 
  mutate(estimate = FALSE)


prep_data2 <- rbind(prep_data, data_aux)

# 5) Vizualizations  --------------------------------------------------------------

plot <- ggplot(prep_data2) +
  geom_line(aes(x = year, y = value, size = weo_date, color = weo_date, linetype = estimate)) +
  scale_size_manual(values = c(0.5,0.5, 0.5, 0.5 , 0.8)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  guides(linetype = "none") +
  facet_wrap(~ variable_to_show, scales = "free") +
  labs(
    title = glue("<b>Estimaciones para {geo_name} del FMI</b>"),
    subtitle = "En base a las publicaciones del WEO",
    caption = "Fuente: Elaboración propia en base al FMI
                 <br> Nota: Líneas sólidas indican datos conocidos en cada publicación del WEO. Líneas punteadas indican estimaciones"
  ) +
  theme_light(base_family = "Segoe UI") +
  theme_light() +
  theme(
    plot.title = element_textbox(hjust = 0),
    plot.caption = element_textbox(hjust = 0),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_textbox(halign = 0.5, color = "black"),
    strip.text = element_textbox(hjust = 0.5),
    axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
    axis.text.y.left = element_text(margin = margin(r = .3, unit = "cm")),
    axis.text.y.right = element_text(margin = margin(l = .3, unit = "cm")),
    axis.title = element_blank(),
  )

plot

ggsave(
  filename = glue("plot_{geo_iso_interest}_.PNG"),
  plot = plot, 
  width = 9,
  height = 6,
  type = "cairo"
)






