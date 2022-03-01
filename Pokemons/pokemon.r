options(install.packages.check.source = "no")

pckgs<-c("tidyverse", "GGally", "gridExtra", "ggpubr", "ggalt",
         "ggthemes", "ggplotify", "RColorBrewer", "ggplot2",
         "rworldmap", "lubridate", "fmsb")

pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {
  install.packages(pckg,repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

poke = read.csv("pokemon_data.csv", header = TRUE)
head(poke)
summary(poke)
str(poke)
poke$Generation <- factor(poke$Generation, ordered=TRUE)
poke$Type.1 <- factor(poke$Type.1)
poke$Type.2 <- factor(poke$Type.2)

table(poke)

#Numero de POKEMONS por TIPO 1 y TIPO 2: HEATMAP 
graf1 <- poke %>% group_by(Type.1,Type.2) %>%
  summarise(typepoke=n()) %>%
  ggplot(aes(x=Type.1,y=Type.2,fill=typepoke, label=typepoke))+
  geom_tile()+
  geom_text()+
  scale_fill_gradient(low = "yellow", high = "red")+
  coord_equal()+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        aspect.ratio = 0.4) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + theme(legend.position = "none") +
  labs(title = "POKEMON TYPE", y="") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

graf1

#Numero de POKEMONS por TIPO 1 y GENERACION: HEATMAP 
graf2 <- poke %>% group_by(Type.1,Generation) %>%
  summarise(typepoke=n()) %>%
  ggplot(aes(x=Type.1,y=Generation,fill=typepoke, label=typepoke))+
  geom_tile()+
  geom_text()+
  scale_fill_gradient(low = "yellow", high = "red")+
  coord_equal()+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        aspect.ratio = 0.4) +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + theme(legend.position = "none") +
  labs(title = "POKEMON TYPE", y="GENERATION") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

graf2


#ATAQUE de POKEMONS por TIPO 1 y GENERACION: HEATMAP 
graf3 <- poke %>% group_by(Type.1,Generation) %>%
     summarise(typepoke=round(mean(Attack),2)) %>%
     ggplot(aes(x=Type.1,y=Generation,fill=typepoke, label=typepoke))+
     geom_tile()+
     geom_text()+
     scale_fill_gradient(low = "yellow", high = "red")+
    coord_equal()+
     theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                     aspect.ratio = 0.4) +
     theme(panel.grid.major = element_blank(),  
                     panel.grid.minor = element_blank()) + theme(legend.position = "none") +
     labs(title = "ATTACK", y="GENERATION") + theme(plot.title = element_text(hjust = 0.5)) + 
     theme(axis.text.x = element_text(angle = 60, hjust = 1))

graf3


#ATAQUE (CUENTA) de POKEMONS por TIPO 1 y GENERACION: HEATMAP 

head(poke,5)

ggplot(poke, aes(x=Generation, y=Attack, col=Type.1, fill=Type.1))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=21, alpha=0.2)  +
  facet_wrap(.~Type.1)

ggplot(poke, aes(x=Generation, y=Attack, col=Generation, fill=Generation))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=21, alpha=0.2)  +
  facet_wrap(.~Type.1) +
  labs(title = "POKEMON TYPE", y="ATTACK", x="GENERATION") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

#################
radarchart(poke)
head(poke)
head(poke[,1:3])

comparacion.poke <- poke %>% 
  filter(Name %in% c("Bulbasaur", "Charmander", "Pikachu", "Squirtle")) %>%  
  column_to_rownames(var = "Name") %>% 
  select(HP, Attack, Defense, Sp.Atk=Sp..Atk, Sp.Def=Sp..Def, Speed)

comparacion.poke
maximo <- apply(comparacion.poke[, c("HP", "Attack", "Defense", "Sp.Atk", "Sp.Def", 
                              "Speed")], 
                2, 
                max,
                na.rm=T)
minimo <- apply(comparacion.poke[, c("HP", "Attack", "Defense", "Sp.Atk", "Sp.Def", 
                              "Speed")],
                2,
                min,
                na.rm=T)

maximo
minimo

grafico_radar <- function(data, max, min){
  data <- rbind(max, min, data)
  colors_border <- c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), 
                     rgb(0.7,0.5,0.1,0.9))
  colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), 
                 rgb(0.7,0.5,0.1,0.4))
  radarchart(data, axistype = 2, pcol = colors_border, 
             pfcol = colors_in, plwd = 4, plty = 1, 
             cglcol = "grey", cglty = 1, axislabcol = "red3", 
             cglwd = 0.8, vlcex = 0.8)
  legend(x = 0.7, y=1.3, y.intersp=0.4, 
         legend = rownames(data[-c(1,2),]), bty = "n", pch=20, 
         col=colors_in, text.col = "black", cex=1, pt.cex=3)
}

grafico_radar(comparacion.poke, maximo, minimo)
radarchart(comparacion.poke,maximo,minimo)

comparacion.poke <- rbind(maximo,minimo,comparacion.poke)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.9,0.5,0.5,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4), rgb(0.9,0.5,0.5,0.4) )

# plot with default options:
radarchart( comparacion.poke  , axistype=4 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
) 

# Add a legend
legend(x=0.7, y=1, legend = rownames(comparacion.poke[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)

###############

head(poke)

poke %>% group_by(Type.1) %>% ggplot(aes(x=Attack, y=Defense, col=Type.1)) +
  geom_point() + 
  facet_wrap(.~Generation)

library(ggrepel)

poke %>% filter(Legendary==TRUE) %>% group_by(Type.1) %>%
  ggplot(aes(x=Attack, y=Defense, col=Type.1)) +
  geom_point() + 
  #geom_text(aes(label=Name))+ 
   
  #geom_label_repel(aes(label = Type.1),
  #                 box.padding   = 1, 
  #                 point.padding = 1,
  #                 segment.color = 'grey50') +
  facet_wrap(.~Generation) 

poke %>% arrange(desc(Attack)) %>%  head(10) %>% summary()
  
poke %>% group_by(Type.1) %>% summarise(typepoke=n())
            
poke %>% group_by(Type.2) %>% summarise(typepoke=n())
            
poke %>% group_by(Type.1) %>% summarise(Att=mean(Attack), typepoke=n(),
                                        max.Att=max(Attack), min.Att=min(Attack))

poke %>% filter(Type.1=="Bug") %>%  group_by(Generation) %>%
  summarise(Count=n(), Att=mean(Attack),  max.Att=max(Attack))
  

poke %>% filter(Type.1=="Bug") %>%  group_by(Generation) %>%
  filter(Attack==max(Attack)) %>%
  summarise(Name, Type.2, Attack, Defense)


poke %>% filter(Legendary==TRUE) %>% group_by(Type.1, Name) %>%
  ggplot(aes(x=Name, y=Attack, fill=Type.1)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~Generation) 

