# Cargar las librerias y limpiar el ambiente

rm(list=ls()) # Borra todo lo que había en el ambiente

packageList<-c("tidyverse", "stringr", "RCurl", "glue","rgeos","readr","ggrepel","haven","raster","rgdal","survey", "readxl", "gridExtra","xlsx", "XLConnect",
               "samplingbook", "sf", "openxlsx", "RSQLite", "DBI", "networkD3",
               "ggalluvial", "ggplot2", "ggplot") # Lista de librerias a cargar (mejor subir todas las que se suelen usar de una vez)

lapply(packageList,require,character.only=TRUE) # Comando que carga las librerias (packegelist)

# Crear atajo para acortar las rutas
path <- "C:/Users/Usuario/Documents/NICOLAS/TRABAJO/ADRES/"
datos <- paste0(path,"datos/")
graficas <- paste0(path,"graficas/") # ruta para guardar graficas
tablas <- paste0(path,"tablas/") # ruta para guardar tablas

#############################

facturas_comfenalco_reciever <- facturas_comfenalco_reciever %>% 
  rename(nit_rec = nit,
         name_rec = name) %>% 
  dplyr::select(id, nit_rec, name_rec)

facturas_comfenalco_issuer <- facturas_comfenalco_issuer %>% 
  rename(nit_iss = nit,
         name_iss = name) %>% 
  dplyr::select(id, nit_iss, name_iss)

facturas_comfenalco_invoice <- facturas_comfenalco_invoice %>% 
  left_join(facturas_comfenalco_issuer, by = "id") %>% 
  left_join(facturas_comfenalco_reciever, by = "id") %>% 
  mutate(valor_a_pagar = as.numeric(valor_a_pagar)) %>% 
  distinct(cufe, nit_iss, name_iss, nit_rec, name_rec) %>% 
  sort(-valor_a_pagar) %>% 
  mutate(posicion = row_number()) %>% 
  dplyr::select(posicion, everything())

###################################### Percentil 90 ###########################

facturas_comfenalco_item <- facturas_comfenalco_item %>% 
  dplyr::select(description, documented_quantity,
                line_extension_amount, price)

facturas_comfenalco_issuer <- facturas_comfenalco_issuer %>% 
  rename(nit_iss = nit,
         name_iss = name) %>% 
  dplyr::select(id, nit_iss, name_iss)

facturas_comfenalco_invoice <- facturas_comfenalco_invoice %>% 
  mutate(valor_a_pagar = as.numeric(valor_a_pagar))

percent_90 <- quantile(facturas_comfenalco_invoice$valor_a_pagar,0.9, na.rm = T)

facturas_comfenalco_invoice <- facturas_comfenalco_invoice %>% 
  left_join(facturas_comfenalco_item, by = "id") %>% 
  left_join(facturas_comfenalco_issuer, by = "id") %>% 
  filter(valor_a_pagar <= percent_90) %>% 
  distinct(id, description) %>% 
  dplyr::select(id, nit_iss, name_iss,
                description, line_extension_amount, valor_a_pagar) %>% 
  sort(-valor_a_pagar) %>% 
  mutate(posicion = row_number()) %>% 
  dplyr::select(posicion, everything())

################# Analisis grafico #####################

facturas_grandes <- read.xlsx(paste0(datos, "facturacion/pre_analisis_Ranking.xlsx"),
                             sheet = "01.RankingFacturas")

class(facturas_grandes$valor_a_pagar)

### Analizar emisores y receptores

# Issuer 

facturas_grandes_fil <- facturas_grandes %>% 
  group_by(Nombre_emisor) %>% 
  summarize(suma_total_mill = (sum(Valor_Bruto_Mas_Tributos, na.rm = TRUE) / 1000000)) %>% 
  mutate(Nombre_emisor = if_else(Nombre_emisor == "FUNDACION PARA EL SERVICIO INTEGRAL DE ATENCION MEDICA - FUNDACION SIAM",
                                  "FUNDACION SIAM", Nombre_emisor)) 

max_value <- max(facturas_grandes_fil$suma_total_mill)

grafica_fact_grandes <- ggplot(facturas_grandes_fil, aes(x = reorder(Nombre_emisor, suma_total_mill), 
                                                         y = suma_total_mill)) +
  geom_bar(stat = "identity", fill = "#4A66AC") +
  geom_text(aes(label = paste0("$", format(suma_total_mill, format ="f", digits = 2, big.mark = ".", decimal.mark = ","))),
            hjust = ifelse(facturas_grandes_fil$suma_total_mill == max_value, 1.2, -0.2),  # Ajuste de posición horizontal
            vjust = ifelse(facturas_grandes_fil$suma_total_mill == max_value, 0.5, 0.5),    # Ajuste de posición vertical
            color = "black",
            size = 6) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(labels = NULL) + 
  labs(x = "", y = "Valor total facturado (millones de pesos)") +
  theme(axis.text.y = element_text(size = 14))

grafica_fact_grandes <- rADRES::formatoADRES(grafica_fact_grandes)

ggsave(paste0(graficas,"Factura_electronica/factura_emisor.png"), grafica_fact_grandes,
       width = 20, height = 12, units = "in")

# Reciever

facturas_grandes_fil_re <- facturas_grandes %>% 
  mutate(name = if_else(str_detect(name_rec_equiv,"COMFENALCO"),"COMFENALCO","OTROS")) %>% 
  group_by(name) %>% 
  summarize(suma_total_mill = (sum(valor_a_pagar, na.rm = TRUE) / 1000000))
  
grafica_fact_grandes_re <- ggplot(facturas_grandes_fil_re, aes(x = reorder(name, suma_total_mill), 
                                                         y = suma_total_mill)) +
  geom_bar(stat = "identity", fill = "#4A66AC") +
  geom_text(aes(label = paste0("$", format(suma_total_mill, big.mark = ".", decimal.mark = ","))),
            vjust = 0.5,
            color = "black",
            size = 5) +
  theme_classic() +
  scale_y_continuous(labels = NULL) + 
  labs(x = "Receptor", y = "Valor total facturado (millones de pesos)")

# , format ="f", digits = 2

grafica_fact_grandes_re <- rADRES::formatoADRES(grafica_fact_grandes_re)

ggsave(paste0(graficas,"Factura_electronica/factura_receptor.png"), grafica_fact_grandes_re,
       width = 20, height = 12, units = "in")


#### Grafico de pie

min_value <- min(facturas_grandes_fil_re$suma_total_mill)
facturas_grandes_fil_re <- facturas_grandes_fil_re %>%
  mutate(is_min = suma_total_mill == min_value)

facturas_grandes_fil_re <- facturas_grandes_fil_re %>% 
  mutate(per_suma = (suma_total_mill/sum(suma_total_mill))*100)

# Crear gráfico de pie

grafica_fact_grandes_re_pie <- ggplot(facturas_grandes_fil_re, aes(x = "", y = suma_total_mill, fill = name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0("$", format(round(suma_total_mill, 2), big.mark = ".", decimal.mark = ","),
                               " (", round(per_suma, 1), "%)"),
                y = ifelse(is_min, suma_total_mill + max(suma_total_mill) * 0.05, suma_total_mill / 2)),
            color = "black",
            size = 5) +
  scale_fill_manual(values = c("#4A66AC", "#009999")) + 
  scale_y_continuous(breaks = NULL) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  labs(fill = "Receptor", y = NULL, x = NULL, title = NULL) +
  guides(fill = guide_legend(title.position = "top", nrow = 1)) +
  theme(plot.background = element_rect(fill = "white"))

grafica_fact_grandes_re_pie <- rADRES::formatoADRES(grafica_fact_grandes_re_pie)

ggsave(paste0(graficas,"Factura_electronica/factura_receptor_pie.png"), grafica_fact_grandes_re_pie,
       width = 20, height = 12, units = "in")

#####################################
# receptor izquierda, emisor derecha

sankey <- read.csv(paste0(datos,"sankey.csv"), sep = ",")

##############################

# Crear los datos de la gráfica Sankey
sankey_data <- sankey %>%
  mutate(Nombre_receptor = str_to_upper(Nombre_receptor)) %>% 
  mutate(Nombre_receptor = if_else(str_detect(Nombre_receptor,"COMFENALCO"),
                                   "COMFENALCO", Nombre_receptor)) %>% 
  group_by(Nombre_receptor, Nombre_emisor) %>%
  summarise(Valor_Bruto_mas_Tributos = sum(Valor_Bruto_Mas_Tributos, na.rm = TRUE)) %>%
  ungroup()

# Calcular el percentil 90 de 'Valor_Bruto_mas_Tributos'
threshold <- quantile(sankey_data$Valor_Bruto_mas_Tributos, 0.95, na.rm = TRUE)

# Filtrar las transacciones que estén en el 10% más alto
sankey_data_filtered <- sankey_data %>%
  filter(Valor_Bruto_mas_Tributos > threshold)

# Crear la gráfica Sankey estática

sank <- ggplot(data = sankey_data_filtered,
               aes(axis1 = reorder(Nombre_receptor, Valor_Bruto_mas_Tributos),
                   axis2 = Nombre_emisor,
                   y = Valor_Bruto_mas_Tributos/1000000)) +
  geom_alluvium(aes(fill = Nombre_receptor), width = 0.1, knot.pos = 0.5) +
  geom_stratum(width = 0.1, fill = "gray", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Nombre_receptor", "Nombre_emisor"), expand = c(0.15, 0.05)) +
  labs(y = "Valor factura (millones de pesos)") +
  theme_minimal()

# Ajustes adicionales al gráfico Sankey
sank <- sank +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "none")

ggsave(paste0(graficas,"Factura_electronica/sankey_zoomed.png"), sank, width = 20, height = 12, units = "in")

############ Grafico pie emisores a comfenalco

# Cargar base de datos

base_remitente <- read_xlsx(paste0(datos,"datos/facturacion/pre_analisis_Ranking.xlsx"),
                            sheet = "05. Ranking remitente destinat")

# 05. Ranking remitente destinat
quantile <- quantile(base_remitente$Valor_Bruto_Mas_Tributos,0.975, na.rm = T)
quantile

sankey_pie <- base_remitente %>% 
  filter(Nombre_receptor == "CAJA DE COMPENSACION FAMILIAR DEL VALLE DEL CAUCA - COMFENALCO VALLE DELAGENTE") %>% 
  mutate(peso = if_else(Valor_Bruto_Mas_Tributos <= quantile, "Otros", Nombre_emisor)) %>% 
  group_by(peso) %>% 
  summarize(suma_total = sum(Valor_Bruto_Mas_Tributos, na.rm = T))

sankey_pie <- sankey_pie %>% 
  mutate(per_suma = suma_total/sum(suma_total)*100)

min_value <- min(sankey_pie$suma_total)
sankey_pie <- sankey_pie %>%
  mutate(is_min = suma_total == min_value)

# Grafico de pie emisores que le giran a CONFENALCO

pie_emisores <- ggplot(sankey_pie, aes(x = "", y = per_suma, fill = peso)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = ifelse(per_suma > 10, paste0("(", round(per_suma, 1), "%)"), ""),
                y = ifelse(is_min, per_suma + max(per_suma) * 0.05, per_suma / 2)),
            color = "black",
            size = 5) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.5, 0.05),  # Ubicar la leyenda dentro de la imagen en la parte inferior
    legend.justification = c(0.5, 0.5),  # Justificar la leyenda en el centro
    legend.direction = "horizontal",
    legend.box = "horizontal",
    plot.background = element_rect(fill = "white")
  ) +
  labs(fill = "Receptor", y = NULL, x = NULL, title = NULL) +
  guides(fill = guide_legend(title.position = "top", nrow = 8))

grafica_pie_emisores <- rADRES::formatoADRES(pie_emisores)

ggsave(paste0(graficas,"Factura_electronica/pie_emisores.png"), grafica_pie_emisores,
       width = 20, height = 12, units = "in")
