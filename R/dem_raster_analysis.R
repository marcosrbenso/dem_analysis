# Script for Accuracy Analysis of Different Digital Elevation Models (DEMs)
# Author: Marcos Roberto Benso
# Date: June 30, 2025
# Description: This script compares the accuracy of different DEMs collected in the same area,
# using statistical metrics such as RMSE, MAE, and PBIAS. Additionally, it generates plots for visualizing the errors.
# Install packages if necessary

#install.packages("terra")
#install.packages("raster")
#install.packages("hydroGOF")
#install.packages("ggplot2")

# Carregar pacotes necessários
library(terra)
library(raster)
library(hydroGOF)
library(ggplot2)
library(raster)
library(RColorBrewer)
library(sp)
library(gstat)
library(tidyverse)

# Definir diretório de trabalho

setwd("C:\\Disciplinas\\Disciplinas\\PPGEU\\2024 Gestão e análise de recursos hídricos\\DEMS_Maria_Clara\\certo\\")


# Carregar os Modelos Digitais de Elevação (DEMs)
srtm_1m <- rast("SRTM.tif")
anadem_reproj_crop <- rast("ANADEM.tif")
phantom_1m_crop <- rast("PHANTOM VOO 250M.tif")
topo <- rast("TOPOGRAFIA.tif")  # Modelo de referência (verdade terrestre)
rtk <- rast("MAVIK.tif")
phantom_sem_pc <- rast("PHANTOM SEM PC.tif")
phantom_com_pc <- rast("PHANTOM COM PC.tif")

anadem_area <- (rast("ANADEM_AREA.tif"))
geodesico <- (rast("L_GEODESICO.tif"))
d_area <- (rast("D_AREA.tif"))

anadem_area <- resample(anadem_area,geodesico)
d_area <- resample(d_area,geodesico)

library(raster)
library(rasterVis)
library(leaflet)

# Suponha que todos sejam objetos raster com mesma extensão e resolução
diff1 <- d_area - geodesico
diff2 <- anadem_area - geodesico

pbias_fun <- function(sim, obs) {
  sum(sim - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE) * 100
}

# Calcula os indicadores e organiza em data.frame
data.frame(
  Modelo = c("d_area", "anadem_area"),

  RMSE = c(
    sqrt(global((d_area - geodesico)^2, fun = 'mean', na.rm = TRUE))$mean,
    sqrt(global((anadem_area - geodesico)^2, fun = 'mean', na.rm = TRUE))$mean
  ),

  MAE = c(
    global(abs(d_area - geodesico), fun = 'mean', na.rm = TRUE)$mean,
    global(abs(anadem_area - geodesico), fun = 'mean', na.rm = TRUE)$mean
  ),

  PBIAS = c(
    pbias_fun(values(d_area), values(geodesico)),
    pbias_fun(values(anadem_area), values(geodesico))
  )
)


# Empilha os dois rasters de diferença
stack_diff <- rast(list(diff1, diff2))

# Plota com levelplot
range_vals <- range(values(stack_diff), na.rm = TRUE)


x_center <- mean(c(753315.9, 759973.9))
y_center <- mean(c(7384559, 7390186))

# Janela de zoom de 500 m para cada lado
zoom_ext <- extent(x_center - 1300, x_center + 1300,
                   y_center - 1000, y_center + 1000)

# Recorta o raster para a área desejada
zoom_stack <- crop(stack_diff, zoom_ext)

names(zoom_stack) <- c("MAVIC3E","ANADEM")

# Define escala de cores
range_vals <- range(values(zoom_stack), na.rm = TRUE)

terrain_pal <- terrain.colors(100)

breaks <- c(-5,-2.5,-1,0,1,2.5,5,10,20)

png("levelplot_export.png", width = 2000, height = 1600, res = 300)


levelplot(
  zoom_stack,
  margin = FALSE,
  at = breaks,
  
  # Tema com paleta + personalização
  par.settings = modifyList(
    rasterVis::rasterTheme(region = terrain_pal),
    list(
      panel.background = list(col = "white"),
      axis.line = list(col = "black"),
      strip.background = list(col = "lightgray")
    )
  ),
  
  # Escalas com coordenadas nos 4 lados
  scales = list(
    draw = TRUE,
    alternating = FALSE,
    tck = c(1, 0),
    cex = 0.6
  ),
  
  # Reduz fonte dos títulos dos painéis
  strip.text = list(cex = 0.6),
  
  frame = TRUE,
  
  # Legenda pequena
  colorkey = list(
    space = "right",
    title = "Error \n[m]",
    title.cex = 0.6,
    labels = list(cex = 0.6)
  ),
  
  # Painel com grade
  panel = function(...) {
    panel.levelplot(...)
    panel.grid(h = -1, v = -1, col = "gray80", lty = 2)
  }
)

dev.off()



# Visualizar os modelos carregados
plot(srtm_1m, main = "Modelo SRTM")
plot(topo, main = "Topografia de Referência")
plot(phantom_1m_crop, main = "Modelo Phantom 1m")
plot(phantom_sem_pc, main = "Phantom sem Controle de Pontos")
plot(phantom_com_pc, main = "Phantom com Controle de Pontos")
plot(rtk, main = "Modelo RTK")

# Ajustar os modelos à referência topográfica
ajustar_DEM <- function(dem, ref) {
  cropped <- terra::crop(resample(dem, ref), ref, mask = TRUE)
  return(cropped)
}

srtm_1m_crop <- ajustar_DEM(srtm_1m, topo)
phantom_1m_crop_crop <- ajustar_DEM(phantom_1m_crop, topo)
anadem_reproj_crop_crop <- ajustar_DEM(anadem_reproj_crop, topo)
rtk_crop <- ajustar_DEM(rtk, topo)
phantom_sem_pc_crop <- ajustar_DEM(phantom_sem_pc, topo)
phantom_com_pc_crop <- ajustar_DEM(phantom_com_pc, topo)




get_box_stats <- function(y, upper_limit = 20 * 1.15) {
  return(data.frame(
    y = 0.95 * upper_limit,
    label = paste(
      "Count =", length(y), "\n",
      "MAE =", round(mean(abs(y)), 2), "\n",
      "RMSE =", round(sqrt(y^2), 2), "\n"
    )
  ))
}


data.frame(
  SRTM = as.numeric(values(srtm_1m_crop)-values(topo)),
  ANADEM = as.numeric(values(anadem_reproj_crop_crop)-values(topo)),
  MAVIC3E = as.numeric(values(rtk_crop)-values(topo)),
  P4PRO150wGCP = as.numeric(values(phantom_com_pc_crop)-values(topo)),
  P4PRO150woGCP = as.numeric(values(phantom_sem_pc_crop)-values(topo)),
  P4PRO250 = as.numeric(values(phantom_1m_crop_crop)-values(topo))
) %>%
  reshape2::melt() %>%
  mutate(variable = as.factor(variable)) -> new_df

library(ggplot2)
library(dplyr)
library(cowplot)  # Para criar inset
library(grid)

# Gráfico principal com todos os grupos
main_plot <- ggplot(new_df, aes(x = variable, y = value)) +
  geom_boxplot() +
  ylab("Error [m]") + xlab("") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

inset_plot <- new_df %>%
  filter(variable %in% c("MAVIC3E", "P4PRO150wGCP")) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )


# Combina o gráfico principal com o inset
final_plot <- ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset_plot, x = 0.3, y = 0.55, width = 0.4, height = 0.4)

# Mostra o gráfico final
print(final_plot)


new_df %>%
  ggplot(aes(
    x = variable, y = value
  ))+
  geom_boxplot()+
  ylab("Error [m]")+xlab("")+
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title     = element_text(face = "bold", size = 16),
    plot.subtitle  = element_text(size = 12, margin = margin(b = 10)),
    panel.grid.minor = element_blank()
  )

new_df %>%
  group_by(
    variable
  ) %>%
  summarise(
    error = mean(abs(value),na.rm=T)
  )

# Criar plots de erro para cada DEM
plot(srtm_1m_crop - topo, main = "Erro SRTM vs Topografia")
plot(phantom_1m_crop_crop - topo, main = "Erro Phantom vs Topografia")
plot(anadem_reproj_crop_crop - topo, main = "Erro ANADEM vs Topografia")
plot(rtk_crop - topo, main = "Erro RTK vs Topografia")
plot(phantom_com_pc_crop - topo, main = "Erro Phantom COM PC vs Topografia")
plot(phantom_sem_pc_crop - topo, main = "Erro Phantom SEM PC vs Topografia")


library(raster)

# Subtract 'topo' from each raster individually
error1 <- srtm_1m_crop - topo
error2 <- phantom_1m_crop_crop - topo
error3 <- anadem_reproj_crop_crop - topo
error4 <- rtk_crop - topo
error5 <- phantom_com_pc_crop - topo
error6 <- phantom_sem_pc_crop - topo

library(terra)

# Lista com todos os rasters
crs(srtm_1m_crop) <- "EPSG:31982"
crs(phantom_1m_crop_crop) <- "EPSG:31982"
crs(anadem_reproj_crop_crop) <- "EPSG:31982"
crs(rtk_crop) <- "EPSG:31982"
crs(phantom_com_pc_crop) <- "EPSG:31982"
crs(phantom_sem_pc_crop) <- "EPSG:31982"


rasters <- list(
  srtm_1m_crop,
  phantom_1m_crop_crop,
  anadem_reproj_crop_crop,
  rtk_crop,
  phantom_com_pc_crop,
  phantom_sem_pc_crop
)


#rasters <- lapply(rasters,rast)

# Reprojetar todos os rasters para o CRS do 'topo'
rasters_proj <- lapply(rasters, function(r) project(r, crs(topo)))

# Reamostrar todos os rasters para terem a mesma resolução e extensão do 'topo'
rasters_resampled <- lapply(rasters_proj, function(r) resample(r, topo))

# Calcular os erros
errors <- lapply(rasters_resampled, function(r) r - topo)

# Criar nomes para os erros
names(errors) <- c("SRTM", "P4PRO250",
                   "ANADEM", "MAVIC3E",
                   "P4PRO150wGCP", "P4PRO150woGCP")

# Empilhar os erros para plotar juntos
errors_stack <- rast(errors)

# Define escala de cores
range_vals <- range(values(errors_stack), na.rm = TRUE)

# 2. Compute global min/max across all layers
min_vals <- min(minValue(errors_rast), na.rm = TRUE)
max_vals <- max(maxValue(errors_rast), na.rm = TRUE)
at_vals  <- seq(min_vals, max_vals, length.out = 100)

# 3. Define a terrain palette
terrain_pal <- terrain.colors(100)

breaks <- c(-5,-2.5,-1,0,1,2.5,5,10,20)


levelplot(
  errors_stack,
  margin = FALSE,
  at = breaks,

  # Tema com paleta + personalização
  par.settings = modifyList(
    rasterVis::rasterTheme(region = terrain_pal),
    list(
      panel.background = list(col = "white"),
      axis.line = list(col = "black"),
      strip.background = list(col = "lightgray")
    )
  ),

  # Escalas com coordenadas nos 4 lados
  scales = list(
    draw = TRUE,
    alternating = FALSE,
    tck = c(1, 0),
    cex = 0.6
  ),

  # Reduz fonte dos títulos dos painéis
  strip.text = list(cex = 0.6),

  frame = TRUE,

  # Legenda pequena
  colorkey = list(
    space = "right",
    title = "Error \n[m]",
    title.cex = 0.6,
    labels = list(cex = 0.6)
  ),

  # Painel com grade
  panel = function(...) {
    panel.levelplot(...)
    panel.grid(h = -1, v = -1, col = "gray80", lty = 2)
  }
)


# Calcular métricas estatísticas
calcular_metricas <- function(modelo, ref) {
  rmse_val <- hydroGOF::rmse(values(modelo), values(ref)) |> as.numeric()
  mae_val <- hydroGOF::mae(values(modelo), values(ref)) |> as.numeric()
  pbias_val <- hydroGOF::pbias(values(modelo), values(ref)) |> as.numeric()
  return(c(rmse = rmse_val, mae = mae_val, pbias = pbias_val))
}

# Criar tabela de métricas
metricas <- data.frame(
  Produto = c("SRTM", "ANADEM", "PHANTOM", "RTK", "PHANTOM SEM PC", "PHANTOM COM PC"),
  RMSE = c(calcular_metricas(srtm_1m_crop, topo)["rmse"],
           calcular_metricas(anadem_reproj_crop_crop, topo)["rmse"],
           calcular_metricas(phantom_1m_crop_crop, topo)["rmse"],
           calcular_metricas(rtk_crop, topo)["rmse"],
           calcular_metricas(phantom_sem_pc_crop, topo)["rmse"],
           calcular_metricas(phantom_com_pc_crop, topo)["rmse"]),
  MAE = c(calcular_metricas(srtm_1m_crop, topo)["mae"],
          calcular_metricas(anadem_reproj_crop_crop, topo)["mae"],
          calcular_metricas(phantom_1m_crop_crop, topo)["mae"],
          calcular_metricas(rtk_crop, topo)["mae"],
          calcular_metricas(phantom_sem_pc_crop, topo)["mae"],
          calcular_metricas(phantom_com_pc_crop, topo)["mae"]),
  PBIAS = c(calcular_metricas(srtm_1m_crop, topo)["pbias"],
            calcular_metricas(anadem_reproj_crop_crop, topo)["pbias"],
            calcular_metricas(phantom_1m_crop_crop, topo)["pbias"],
            calcular_metricas(rtk_crop, topo)["pbias"],
            calcular_metricas(phantom_sem_pc_crop, topo)["pbias"],
            calcular_metricas(phantom_com_pc_crop, topo)["pbias"])
)

metricas

# Salvar resultados em CSV
write.csv(metricas, "resultados.csv", row.names = FALSE)



# Visualizar métricas em gráfico
metricas_long <- reshape2::melt(metricas, id.vars = "Produto")
ggplot(metricas_long, aes(x = Produto, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação de Erros dos DEMs", y = "Valor", x = "Modelo Digital de Elevação") +
  theme_minimal()

# Listar arquivos disponíveis no diretório
list.files()

lapply(list.files(),rast) -> raster.stack

topo <- rast("TOPOGRAFIA.tif")

lapply(raster.stack,function(x){ajustar_DEM(x,topo)}) -> raster.stack.adj


lapply(raster.stack.adj,function(dem){
  data.frame(
    id = terra::names(dem),
    values = (as.numeric(values(dem))-as.numeric(values(topo)))
    )
}) -> raster.df.list

do.call('rbind',raster.df.list) -> raster.df

head(raster.df)

library(ggplot2)
library(scales)   # for pretty number formatting

raster.df %>%
  filter(id != "TOPOGRAFIA") -> data

data %>%
  mutate(id = as.factor(id)) -> data

data$id

res.aov <- aov(values ~ id, data = data)


TukeyHSD(res.aov)

library(car)
sstable <- Anova(model, type = 3) #




raster.df %>%
  filter(id != "TOPOGRAFIA") %>%
  mutate(id = factor(id)) %>%
  ggplot(aes(x = id, y = values, fill = id)) +
  ylim(0,8)+

  # main geometry
  geom_boxplot(alpha = 0.8) +

  # a friendly color palette
  scale_fill_brewer(palette = "Set2") +

  # labels & title
  labs(
    title    = "Distribution of Errors by Product",,
    x        = "",
    y        = "Absolute error"
  ) +

  # clean, minimal theme
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title     = element_text(face = "bold", size = 16),
    plot.subtitle  = element_text(size = 12, margin = margin(b = 10)),
    panel.grid.minor = element_blank()
)




