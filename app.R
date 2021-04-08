
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

# First must download WEO databases and save it in folder "data" !!!!!!!!!
# Podes bajar las bases individuales y las agregadas!!! 
# El parser junta todo!!

if (!file.exists(file.path("data"))){dir.create(file.path("data"))}

files.toimport <- list.files(
  path       = "data/",
  all.files  = FALSE,
  pattern    = "\\.xls$",
  full.names = FALSE,
  recursive  = FALSE
)

if ( length(files.toimport) == 0 ){stop("Baja las bases WEO antes de seguir y guardalas en la carpeta 'data'")}

weo_data <- purrr::map_df(files.toimport, weo_parser)


# 4) Transformations  --------------------------------------------------------------

# Variables of interest
variables_interest <- c("LUR", "NGDP_RPCH", "GGXONLB_NGDP", "BCA_NGDPD", "PCPIPCH", "GGXWDG_NGDP")


prep_data <- weo_data %>% 
  filter(
    geo_name == "Argentina"
    & variable_code %in% variables_interest
    & year >= 2019
  ) %>% 
  mutate(
    year = ymd(glue("{year}0101")),
    weo_date = factor(weo_date, levels = c("Oct-2019", "Oct-2020", "Apr-2021")),
    variable_to_show = case_when(
      variable_code == "LUR" ~ "<b>Desempleo</b><br>Porcentaje de la PEA",
      variable_code == "NGDP_RPCH" ~ "<b>Crecimiento del PBI</b><br>Precios constantes",
      variable_code == "GGXONLB_NGDP" ~ "<b>Resultado Primario del Sector Público</b><br>Porcentaje del PBI",
      variable_code == "BCA_NGDPD" ~ "<b>Resultado Cuenta Corriente</b><br>Porcentaje del PBI",
      variable_code == "PCPIPCH" ~ "<b>Inflación</b><br>Variación Interanual, fin de período",
      variable_code == "GGXWDG_NGDP" ~ "<b>Deuda Bruta del Gobierno General</b><br>Porcentaje del PBI"
    )
  )


# 5) Vizualizations  --------------------------------------------------------------

plot <- ggplot(prep_data) +
  geom_line(aes(x = year, y = value, size = weo_date, linetype = weo_date), color = "#CB1E40") +
  geom_point(aes(x = year, y = value, color = weo_date, shape = weo_date), size = 3, color = "#CB1E40", fill = "#CB1E40") +
  geom_text_repel(
    data = prep_data %>% filter(weo_date == "Apr-2021"),
    mapping = aes(x = year, y = value, label = round(value, 1)),
    color = "#CB1E40",
    family = "Segoe UI"
  ) + 
  scale_size_manual(values = c(0.5,0.5,1.1)) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) +
  scale_shape_manual(values = c(NA, NA, 16)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~ variable_to_show, scales = "free_y") +
  labs(
    title = "<b>Estimaciones para Argentina del FMI</b>",
    subtitle = "Pre COVID (WEO Oct19), COVID1 (Oct20) y COVID2 (Abr21)",
    caption = "Fuente: Elaboración propia en base al FMI" 
  ) +
  theme_light(base_family = "Segoe UI") +
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
  filename = "plot.PNG",
  plot = plot, 
  width = 9,
  height = 6
)








# Debt Plot ---------------------------------------------------------------

my_colors = c(
  "High-income countries" = "#d0d0d0",
  "Middle-income countries" = "#008abc",
  "Low-income countries" = "#e47e01",
  "45 degree line" = "black"
)

prep_data2 <- weo_data %>% 
  filter(
    variable_code == "GGXWDG_NGDP"
    & year %in% c(2008, 2021)
    & weo_date == "Apr-2021"
  ) %>% 
  select(geo_name, geo_iso, year, value) %>% 
  filter(value  < 200) %>% 
  left_join(countries) %>% 
  pivot_wider(names_from = year, values_from = value)  %>%
  na.omit() %>% 
  mutate(
    income_level_group = case_when(
      income_level %in% c("LMC", "UMC") ~ "Middle-income countries",
      income_level == "HIC" ~ "High-income countries",
      income_level == "LIC" ~ "Low-income countries"
    ),
    income_level_group = factor(
      x = income_level_group,
      levels = c("High-income countries","Middle-income countries","Low-income countries", "45 degree line")
    )
  ) %>% 
  bind_rows(
    list(
      `2008` = 0,
      `2021` = 0,
      geo_iso = "test1",
      income_level_group = "test"
    ),
    list(
      `2008` = 200,
      `2021` = 200,
      geo_iso = "test2",
      income_level_group = "test"
    )
  )


plot_debt <- ggplot() +
  geom_point(
    data = prep_data2 %>% filter(!income_level_group == "test"),
    mapping = aes(
      x = `2008`,
      y = `2021`,
      fill = income_level_group
    ),
    size = 3,
    shape = 21,
    stroke = 1.1,
    color = "black"
  ) + 
  geom_line(
    data = prep_data2 %>% filter(str_detect(geo_iso, "^test")),
    mapping = aes(
      x = `2008`,
      y = `2021`,
      linetype = "45 dergree line"
    ),
    size = 1,
    color = "black"
    # show_legend = TRUE
  ) +
  scale_fill_manual(values = my_colors)+
  scale_y_continuous(
    limits = c(0,200),
    expand = expansion(c(0.03,0.03)),
    breaks = seq(0, 200, 20)
  ) + 
  scale_x_continuous(
    limits = c(0,200),
    expand = expansion(c(0.03,0.03)),
    breaks = seq(0, 200, 20)
  ) +
  labs(
    title = "Figure 1<br><span style = 'color:red;'>Gross general government debt-to-GDP ratio, 2008 versus 2021</span>",
    x = "General government debt-to-GDP ratio in 2008",
    y = "General government debt-to-GDP ratio in 2021",
    caption = "<span style = 'color:red;'>Note:</span>Countries with gross general government debt-to-GDP ratio over 200 in either year are excluded from this<br>chart.<br><span style = 'color:red;'>Sources:</span>Authors’ elaboration, based on data from IMF World Economic Outlook April 2021 and World Bank<br>World Development Indicators."
  ) +
  theme_bw() +
  theme(
    legend.position =c(.80, .20),
    legend.title = element_blank(),
    legend.box.background = element_rect(color = "black"),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    plot.caption = element_textbox(hjust = 0),
    plot.title = element_textbox(size = 16),
    plot.title.position = "plot"
  )



plot_debt

ggsave(
  filename = "plot_debt.PNG",
  plot = plot_debt, 
  width = 7,
  height = 7
)
