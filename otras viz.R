



# 4) Transformations  --------------------------------------------------------------

# Variables of interest
geo_iso_interest <- c("ARG", "BRA", "CHL", "URU", "USA")
geo_name_interest <- "Euro area"

prep_data <- weo_data %>% 
  filter(
    variable_code == "PCPIPCH"
    & year >= 2016
  ) %>% 
  filter(
    geo_iso %in% geo_iso_interest 
    | geo_name == geo_name_interest  
  ) %>% 
  select(-variable) %>% 
  mutate(
    year = ymd(glue("{year}0101")),
    weo_date = factor(weo_date, levels = c("Oct-2019", "Apr-2020","Oct-2020", "Apr-2021", "Oct-2021", "Apr-2022")),
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
  scale_size_manual(values = c(0.5,0.5, 0.5, 0.5 , 0.5, 0.8)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  guides(linetype = "none") +
  facet_wrap(~geo_name, scales = "free") +
  labs(
    title = glue("<b>Estimaciones de Inflación del FMI</b>"),
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
  filename = "plot.PNG",
  plot = plot, 
  width = 9,
  height = 6,
  type = "cairo"
)





# Debt Plot ---------------------------------------------------------------

my_colors = c(
  "High-income countries" = "#d0d0d0",
  "Middle-income countries" = "#008abc",
  "Low-income countries" = "#e47e01"
  # "45 degree line" = "black"
)

prep_data2 <- weo_data %>% 
  filter(
    variable_code == "GGXWDG_NGDP"
    & year %in% c(2019, 2022)
    & weo_date == "Apr-2022"
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
      `2019` = 0,
      `2022` = 0,
      geo_iso = "test1",
      income_level_group = "test"
    ),
    list(
      `2019` = 200,
      `2022` = 200,
      geo_iso = "test2",
      income_level_group = "test"
    )
  )


plot_debt <- ggplot() +
  geom_point(
    data = prep_data2 %>% filter(!income_level_group == "test"),
    mapping = aes(
      x = `2019`,
      y = `2022`,
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
      x = `2019`,
      y = `2022`,
      linetype = "45 dergree line"
    ),
    size = 1,
    color = "black"
    # show_legend = TRUE
  ) +
  scale_fill_manual(values = my_colors) +
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
    title = "Figure 1<br><span style = 'color:red;'>Gross general government debt-to-GDP ratio, 2019 versus 2022</span>",
    x = "General government debt-to-GDP ratio in 2019",
    y = "General government debt-to-GDP ratio in 2022",
    caption = "<span style = 'color:red;'>Note:</span>Countries with gross general government debt-to-GDP ratio over 200 in either year are excluded from this<br>chart.<br><span style = 'color:red;'>Sources:</span>Authors’ elaboration, based on data from IMF World Economic Outlook April 2022 and World Bank<br>World Development Indicators."
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
  # type = "cairo"
)

