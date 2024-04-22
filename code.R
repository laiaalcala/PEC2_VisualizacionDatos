################### Histogram #####################
library(AER)
data("CPS1985")

ggplot(CPS1985, aes(x = wage)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "white") +
  labs(title = "Distribución del Salario (dólares por hora)",
       x = "Salario",
       y = "Frecuencia") +
  theme_minimal()
ggsave("histogram.png", plot = h, width = 14, height = 8, units = "in", dpi = 300)

################### Choropleth Map #####################
library(ggplot2)
library(maps)
library(dplyr)

MainStates <- map_data("state")

StatePopulation <- read_csv("Downloads/StatePopulation.csv")

# Union datos de población con datos del mapa
MergedStates <- inner_join(MainStates, StatePopulation, by = "region")

p <- ggplot(MergedStates, aes(x = long, y = lat, group = group, fill = population/1000000)) + 
  geom_polygon(color = "black", linewidth = 0.2) +
  scale_fill_continuous(name="State Population (millions)", low = "lightblue", 
                        high = "darkblue",limits = c(0,40), breaks=c(5,10,15,20,25,30,35), 
                        na.value = "grey50")+
  labs(title = "Distribución de la Población en los Estados Unidos en 2020",
       x = NULL, y = NULL) +theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.caption = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("Choropleth_map.png", plot = p, width = 14, height = 8, units = "in", dpi = 300)

################### Marimekko Chart #####################
library(mosaic)
library(vcd)
data(Arthritis, package = "vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
g <- mosaic(art, gp = shading_max, 
            split_vertical = TRUE, 
            main = "Tratamiento de artritis vs. Mejora ")

png(filename = "Marimekko_chart.png", width = 800, height = 600, units = "px", res = 300)
print(g)
dev.off()
