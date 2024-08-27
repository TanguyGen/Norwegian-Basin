packages <- c("tidyverse", "ggplot2", "patchwork",  "sf")        # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

a<-readRDS("./Objects/International effort proportion by gear and habitat.rds")
a<-as.data.frame(a)
b<-read.csv("./target/International effort by gear.csv")
habitats <- readRDS("./Objects/Habitats.rds") %>%                           # Load habitat polygons
  mutate(area = as.numeric(st_area(.)))

habitats$Habitat[5]<-"Deep sea"
p1<-ggplot(habitats) +
  geom_sf(aes(fill = Habitat)) +
  theme_minimal() +
  scale_fill_manual(values = c("Gravel" = "brown", "Rock" = "darkgray", "Sand" = "gold", "Silt" = "tan", "Deep sea" = "darkblue")) +
  labs(title = "Habitat Map", fill = "Habitat") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))




pelagic_trawlers<- b$x[b$X=="Pelagic_Trawlers_and_Seines"]* a$Pelagic_Trawlers_and_Seines
longlines<- b$x[b$X=="Longlines_and_Gillnets"]* a$Longlines_and_Gillnets
shelf_trawlers<-b$x[b$X=="Shelf_Trawlers"]* a$Shelf_Trawlers
land<- readRDS("./Objects/International landings.rds")
land<-as.data.frame(land)[-c(1),]

p2<-ggplot(land,aes(x=rownames(land),y=Planktivore)) +
  geom_bar(stat="identity",fill="black") +
  scale_x_discrete(labels=c("Longlines_and_Gillnets"="Longlines\nand gillnets","Pelagic_Trawlers_and_Seines"="Pelagic trawlers\nand seines", "Pots"="Pots and traps","Shelf_Trawlers"="Shelf trawlers"))+
  theme_minimal() +
  labs(title = "Landing of planktivorous pelagic fish per gear",x="",y =  expression(paste("Landing biomass (mM.", m^{-2},")"))) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        plot.margin = margin(0, 0, 0, -2, "cm"))

land$Planktivore[4]/sum(land$Planktivore)#pelagic trawls and seine
land$Planktivore[7]/sum(land$Planktivore)#shelf trawl
(land$Planktivore[4]+land$Planktivore[7])/sum(land$Planktivore) #both

habitats_gear<-data.frame(habitats,pelagic_trawlers,longlines,shelf_trawlers)
habitats_gear<-st_as_sf(habitats_gear)

p3<-ggplot(habitats_gear) +
  geom_sf(aes(fill = pelagic_trawlers)) +
  theme_minimal() +
  scale_fill_continuous() +
  labs(title = "Pelagic trawlers and seines", fill =  expression(paste("Fishing effort (s.", m^{-2},".",d^{-1},")"))) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))



p4<-ggplot(habitats_gear) +
  geom_sf(aes(fill = shelf_trawlers)) +
  theme_minimal() +
  scale_fill_continuous() +
  labs(title = "Shelf trawlers", fill =  expression(paste("Fishing effort (s.", m^{-2},".",d^{-1},")"))) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

combined <- (p2)/(p1+p3+p4)+
  plot_layout(widths  = c(20, 0.5))

ggsave("my_plot.png", plot = combined, width = 20, height = 16, dpi = 300)


