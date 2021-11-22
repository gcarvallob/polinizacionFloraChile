R.Version()
####POLINIZACION FLORA DE CHILE######
rm(list=ls()) # Borra todos los objetos
setwd("/Users/user/Desktop/floradeChile") #ESCOGE EL DIRECTORIO

matriz<-read.table(file="matriz_polinizacion.csv", header=T, sep=",", check.names=T, dec=".")

#eliminar interacciones duplicadas
library(data.table) #para eliminar duplicados, se debe comenzar cada vez que se vuelve a usar la función,
  #porque hay una en base que se llama igual
 #incorporar cuando se quieran contar riqueza,
  #eliminar cuando se quieran contar interacciones
matriz_unq<-matriz[!duplicated(matriz$interaction), ] #para sacar los visitantes duplicados
summary(as.factor(matriz_unq$interaction))
#matriz_unq<-unique(matriz, by = "interaccion") #esta es otra forma de eliminar las interacciones repetidas

#para contar (polinizadores, interacciones, etc)
names(matriz_unq)
summary(as.factor(matriz_unq$origin))
summary(as.factor(matriz_unq$tribu))
#contar numero de plantas
plantas<-matriz_unq[!duplicated(matriz_unq$plant_species),] #eliminar especies de polinizadores duplicados
plantas$plant_species
#contar familias de plantas
fam_plantas<-matriz_unq[!duplicated(matriz_unq$plant_family),]
fam_plantas$plant_family

# Para contar considerando una interaccion por especie (cambiar matriz x matriz_unq)
names(matriz_unq)
summary(as.factor(matriz_unq$Orden))/1419
sum(33,164,420,6,630,146,18,1,1)

#para aislar por orden 
hymenoptera<-as.data.frame(matriz_unq[which(matriz_unq$Orden=="Hymenoptera"), ])
summary(as.factor(hymenoptera$Familia)) #visitas de apoidea
summary(as.factor(hymenoptera$especie))
# contar por especie , visitantes y familias
hymenoptera_sp<-hymenoptera[!duplicated(hymenoptera$especie),]
hymenoptera_sp<-hymenoptera[!duplicated(hymenoptera$visitante),]
summary(as.factor(hymenoptera_sp$Familia))
hymenoptera_sp<-hymenoptera_sp[order(rownames(hymenoptera_sp)),order(colnames(hymenoptera_sp))]
hymenoptera_sp$especie
sum(16,51,29,47,32)
175/217

hymenoptera<-hymenoptera[!duplicated(hymenoptera$visitante), ] #elimina los visitantes duplicados, deja un representante por especie
(as.factor(hymenoptera$visitante)) #(debiese existir un registro por especie)
summary(as.factor(hymenoptera$Familia)) # cuenta las familias en hymenoptera
summary(as.factor(hymenoptera$especie))
summary(as.factor(matriz_unq$especie))

# Para contar el número de especies de Apoidea, se basa en los datos de hymeoptera (previos)
apoidea<-as.data.frame(hymenoptera[hymenoptera$Familia %in% c('Andrenidae', 
    "Apidae","Colletidae","Halictidae","Megachilidae"), ])
apoidea_generos<-as.data.frame(summary(as.factor(apoidea$genero)))
apoidea_generos

# Se usó el mismo procedimiento para los otros órdenes (cambiar matriz x matriz_unq)
diptera<-as.data.frame(matriz[which(matriz$Orden=='Diptera'), ])
coleoptera<-as.data.frame(matriz[which(matriz$Orden=='Coleoptera'), ])
lepidoptera<-as.data.frame(matriz[which(matriz$Orden=='Lepidoptera'), ])

summary(as.factor(matriz$Familia))

# GRAFICO DE NÚMERO DE INTERACCIONES POR TAXA (STACKED PLOT)
ordenes<-as.factor(c(rep("Hymenoptera", 6) , rep("Diptera", 1) , rep("Coleoptera" , 1) , 
                     rep("Lepidoptera" , 1), rep("Apodiformes",1), 
                     rep("Passeriformes",1),rep("Otros",1)))
ordenes<-factor(ordenes,levels=c("Hymenoptera","Diptera","Coleoptera","Lepidoptera",
                                 "Caprimulgiformes","Passeriformes","Otros"))
condition <-as.factor(c(rep(c("Apidae" , "Megachilidae" , "Halictidae",
                              "Colletidae","Andrenidae", "Otros himenópteros"),1),
                        rep(c("Diptera"),1),
                        rep(c("Coleoptera"),1),
                        rep(c("Lepidoptera"),1),
                        rep(c("Caprimulgiformes"),1),
                        rep(c("Passeriformes"),1),
                        rep(c("Otros ordenes"))))
value <- c(c(288, 108, 101,60,36,66),434,173,161,40,
           21,8)
data_plot <- data.frame(ordenes,condition,value)

library(ggnewscale)
library(ggplot2)
ggplot(data_plot, aes(fill=condition, y=value, x=ordenes)) + 
  geom_bar(position="stack", stat="identity",colour="black",size=0.3)+
  ylab("Número de visitas")+
  scale_fill_manual(values = c("#fd9409","#fc6940","red",
                               "purple","#c07206","darkgreen",
                               "#c65418","cyan","#c04536",
                               "#77161b","gray","blue"))+ 
    theme(axis.text.x=element_text(angle=45, hjust=1,size=15,colour="black"), 
        axis.title.x = element_blank(),
        axis.title=element_text(size=14),
        axis.text.y=element_text(size=14,colour="black"),
        panel.border=element_rect(colour = "black", 
                                  fill=NA, size=1.5))+
  scale_y_continuous(expand = c(0, 0),limits = c(0, 700))

#########
# CURVAS DE DOMINANCIA#####
##############
matriz_unq<-matriz_unq[!matriz_unq$especie == "", ] #elimina las filas donde no se conoce el nombre de la especie de polinizador
especies_pol<-data.frame(higher=matriz_unq$especie, lower=matriz_unq$plant_species,
                  webID=seq(0,0,800), freq=matriz_unq$freq) #generando la matriz de interaccion
library(bipartite)
visitas_especies<-as.data.frame(frame2webs(especies_pol, 
                                type.out = "array")) #cone sto se construye la matriz desde donde se sca el número de visitas por especies
names(visitas_especies)
matriz_unq_species<-matriz_unq[!duplicated(matriz_unq$especie), ] #elimina duplicados de nombres cientificos para construir el grafico
matriz_unq_species<-matriz_unq_species[order(matriz_unq_species$especie),] #para ordenar alfabeticamente la marriz y construri "a"
colSums(visitas_especies)
matriz_unq_species$Familia

a<-data.frame(especies=(names(visitas_especies)),
              Visitas=(colSums(visitas_especies)), 
              familia=(matriz_unq_species$Familia)) #no poner as factor lo que se va a modificar
a
a[a == "Syrphidae"]<-"Diptera"
a[a == "Bombyliidae"]<-"Diptera"
a[a == "Lycaenidae"]<-"Lepidoptera"
a[a == "Nymphalidae"]<-"Lepidoptera"
a[a == "Melyridae"]<-"Coleoptera"
a[a == "Acroceridae"]<-"Diptera"
a[a == "Crabronidae"]<-"Otros himenopteros"
a[a == "Vespidae"]<-"Otros himenopteros"
a[a == "Papilionidae"]<-"Lepidoptera"
a[a == "Pieridae"]<-"Lepidoptera"
a[a == "Curculionidae"]<-"Coleoptera"
a[a == "Cerambycidae"]<-"Coleoptera"
a[a == "Melolonthidae"]<-"Coleoptera"
a[a == "Coccinellidae"]<-"Coleoptera"
a[a == "Clusiidae"]<-"Diptera"
a[a == "Masaridae"]<-"Otros himenopteros"
a[a == "Tachinidae"]<-"Diptera"
a[a == "Scarabaeidae"]<-"Coleoptera"
a[a == "Tabanidae"]<-"Diptera"
a[a == "Hesperiidae"]<-"Lepidoptera"
a[a == "Buprestidae"]<-"Coleoptera"
a[a == "Mydidae"]<-"Diptera"
a[a == "Nemestrinidae"]<-"Diptera"
a[a == "Ulidiidae"]<-"Diptera"
a[a == "Coreidae"]<-"Otros ordenes"
a[a == "Sphingidae"]<-"Lepidoptera"
a[a == "Tyrannidae"]<-"Passeriformes"
a[a == "Thraupidae"]<-"Passeriformes"
a[a == "Fringillidae"]<-"Passeriformes"
a[a == "Athericidae"]<-"Diptera"
a[a == "Drosophilidae"]<-"Diptera"
a[a == "Eumanidae"]<-"Otros himenopteros"
a[a == "Formicidae"]<-"Otros himenopteros"
a[a == "Icteridae"]<-"Passeriformes"
a[a == "Latridiidae"]<-"Coleoptera"
a[a == "Mimidae"]<-"Passeriformes"
a[a == "Passerellidae"]<-"Passeriformes"
a[a == "Pompilidae"]<-"Otros himenopteros"
a[a == "Psocidae"]<-"Otros ordenes"
a[a == "Saturniidae"]<-"Lepidoptera"
a[a == "Trochilidae"]<-"Caprimulgiformes"
a[a == "Turdidae"]<-"Passeriformes"
a[a == "Eumenidae"]<-"Otros himenopteros"
summary(as.factor(a$familia))
is.numeric((a$Visitas))

a<-as.data.frame(sapply(a,gsub,pattern=".0",replacement="")) #para reemplazar el .0
a<-as.data.frame(sapply(a,gsub,pattern="_",replacement=" "))
a$Visitas<-as.numeric(a$Visitas)
a

library(ggplot2)
names(a)
ggplot(a, aes(x=reorder(especies, -Visitas), y=log10(Visitas+0.1), fill=familia)) +
  geom_bar(stat = "identity") +
  labs(x = "Especies", y = "Visitas (log10)",colour="black") +
  scale_fill_manual(values=c("#fd9409","#fc6940","red",
                             "purple","#c07206","darkgreen",
                             "#c65418","cyan","#c04536",
                             "#77161b","gray","blue"))+
  scale_y_continuous(expand = c(0, 0),limits=c(0,1.8))+
  theme(axis.text.x = element_text(family="sans",face="italic", colour="black", size=8,angle=45,vjust=0.5,hjust=1),
        axis.text.y=element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18))#+
        #theme_bw()

###################################################
#####····RELACIÓN ENTRE DIVERSIDAD Y VISITAS···####
####Para familias y tribus de abejas#####
#Información taxonomica para construir los gráficos de diversidad x diversidad de visitantes
#FAMILIAS
#424 especies (Montalva y Ruz, 2017): 
#Andrenidae (58 sp/13.6% de las especies), 
#Apidae (87 sp/20.5%)
#Colletidae (148 sp/34.9%)
#Halictidae (61 sp/14.4%) 
#Megachilidae (70 SP/16.5%)

taxa<-c("Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae")
S_apoidea_esperada<-c(58,87,148,61,70)
matriz_unq<-matriz_unq[!matriz_unq$visitante == "", ] #elimina las filas donde no se conoce el nombre de la especie de polinizador
matriz_unq_visitante<-matriz_unq[!duplicated(matriz_unq$visitante), ] #elimina duplicados de nombres cientificos para construir el grafico
summary(as.factor(matriz_unq_visitante$Familia))
S_apoidea_observada<-c(16,51,29,47,32)
S<-data.frame(taxa,S_apoidea_esperada,S_apoidea_observada)
S

# Grafico riqueza obs vs. riqueza esperada FAMILIAS
library(ggplot2)
ggplot(S, aes(y=S_apoidea_observada, x=S_apoidea_esperada,color=taxa,group=taxa)) + 
  geom_point(size=4)+
  geom_text(label=taxa,check_overlap = T,nudge_y=3,size=7)+
  scale_color_manual(values = c("#fd9409","#fc6940","#c07206","#c65418","#c04536"))+
  scale_y_continuous(name="Riqueza observada",expand = c(0, 0),limits = c(0, 70))+
  scale_x_continuous(name="Riqueza de las familias",expand = c(0, 0),limits = c(0, 170))+
  theme(axis.text.x=element_text(hjust=1,size=18,colour="black"), 
        axis.title=element_text(size=18),
        axis.text.y=element_text(size=18,colour="black"),
        panel.border=element_rect(colour ="black",fill=NA, size=1.5),
        legend.position="none")+
  geom_abline(intercept=0, slope=1,col="darkred")
  
#modelo lineal FAMILIAS y riqueza        
summary(lm(S_apoidea_observada ~ S_apoidea_esperada, data = S))
summary(as.factor(matriz_unq$Familia))
N_visitas_familia<-c(33,271,60,99,102)
S_2<-data.frame(taxa,S_apoidea_esperada,N_visitas_familia)

ggplot(S_2, aes(y=N_visitas_familia, x=S_apoidea_esperada,color=taxa,group=taxa)) + 
  geom_point(size=4)+
  geom_text(label=taxa,check_overlap=T,nudge_y=11,size=7)+
  annotate(geom="text", x=84, y=102, label="Megachilidae",
                 color="#c04536",size=7)+
  scale_color_manual(values = c("#fd9409","#fc6940","#c07206","#c65418","#c04536"))+
  scale_y_continuous(name="Número de visitas",expand = c(0, 0),limits = c(-10, 300))+
  scale_x_continuous(name="Riqueza de las familias",expand = c(0, 0),limits = c(40, 170))+
  theme(axis.text.x=element_text(hjust=0.5,size=18,colour="black"), 
        axis.title=element_text(size=18),
        axis.text.y=element_text(size=18,colour="black"),
        panel.border=element_rect(colour ="black",fill=NA, size=1.5),
        legend.position="none")#+
  #geom_abline(intercept=0, slope=1,col="darkred")

#modelo lineal FAMILIAS y visitas        
summary(lm(N_visitas_familia ~ S_apoidea_esperada, data = S_2))
?lm()

##########################################################
### RELACION ENTRE LA DIVERSIDAD DE APOIDEA POR TRIBUS####

##(genero, tribus, subfamilias)
###esto es para generar el número observado
apoidea<-as.data.frame(matriz_unq[matriz_unq$Familia %in% c('Andrenidae', 
          "Apidae","Colletidae","Halictidae","Megachilidae"), ])
apoidea$visitante<-as.factor(apoidea$visitante) #ojo, transformar previamente a factor
apoidea_sp<-apoidea[!duplicated(apoidea$visitante), ] #para sacar los visitantes duplicados
names(apoidea_sp)
summary(as.factor(apoidea_sp$visitante))
summary(as.factor(apoidea_sp$Familia))
summary(as.factor(apoidea_sp$subfamilia))
summary(as.factor(apoidea_sp$tribu))

# acá está la tabla resumen con el número esperado (Montalva & Ruz) y observado
##de especies por género
tribus<-read.table(file="tribus.csv", header=T, sep=",", check.names=T, dec=".")
tribus<-tribus[c(1:32),]
#tribus<-tribus[-25,] #Xeromelissini no es una tribu, correr sólo una vez
#tribus (N = 31 observaciones, ojo que el row.names termina en 32, se come la cifra eliminada)

#gráfico sp obs-esperadas (por tribu)
library(ggrepel)
# información del paquete https://ggrepel.slowkow.com/articles/examples.html
names(tribus)

tribus$tribu_1
ggplot(tribus, aes(x=N_sp_tribu, y=N_sp_tribu_observadas1,
                        color=factor(familia_1),group=factor(tribu_1))) +
  geom_point(size=4)+
  geom_label_repel(aes(label = factor(tribu_1),
                    fill = factor(familia_1),segment.shape=0.5), 
                   color = 'white',
                   size = 5,lineheight=1, segment.color="darkgray",
                   xlim=c(NA,NA),ylim=c(NA,NA),
                   min.segment.length = 1,max.overlaps=Inf,
                   box.padding=0.4,hjust=0.5,
                   segment.curvature = -1e-20, max.time = 2,
                   arrow = arrow(length = unit(0.015, "npc")))+
  scale_color_manual(name ="Familias", 
                     values=c("#fd9409","#fc6940","#c07206","#c65418","#c04536"))+
  scale_fill_manual(name ="Familias", 
                     values=c("#fd9409","#fc6940","#c07206","#c65418","#c04536"))+
  scale_y_continuous(name="Riqueza observada",expand = c(0, 0),limits = c(-8, 30))+
  scale_x_continuous(name="Riqueza de las tribus",expand = c(0, 0),limits = c(-15, 60))+
  theme(axis.text.x=element_text(hjust=1,size=18,colour="black"), 
        axis.title=element_text(size=18),
        axis.text.y=element_text(size=18,colour="black"),
        panel.border=element_rect(colour = "black", 
                                  fill=NA, size=1.5),
        legend.position="none")+
   guides(fill = guide_legend(override.aes = aes(color = NA)))+ #esto último es para eliminar la "a" de la leyenda
  geom_abline(intercept=0, slope=1,col="darkred")+
  geom_abline(intercept=0.146, slope=0.390,linetype="dashed",col="darkred")
  
#regresión lineal, para incorporar al gráfico (pendiente e intercepto)
names(taxa_apoidea)
summary(lm(N_sp_tribu_observadas1 ~ N_sp_tribu, data = tribus))

###Relación entre la riqueza observada y el número de visitas por TRIBU
summary(as.factor(matriz_unq$tribu))
#los datos ya fueron incluidos en el archivo excel
names(tribus)

ggplot(tribus, aes(x=N_sp_tribu, y=N_sp_tribu_observadas1,
                   color=factor(familia_1),group=factor(tribu_1))) +
  geom_point(size=4)+
  geom_label_repel(aes(label = factor(tribu_1),
                       fill = factor(familia_1),segment.shape=0.5), 
                   color = 'white',
                   size = 5,lineheight=1, segment.color="darkgray",
                   xlim=c(NA,NA),ylim=c(NA,NA),
                   min.segment.length = 1,max.overlaps=Inf,
                   box.padding=0.4,hjust=0.5,
                   segment.curvature = -1e-20, max.time = 2,
                   arrow = arrow(length = unit(0.015, "npc")))+
  scale_color_manual(name ="Familias", 
                     values=c("#fd9409","#fc6940","#c07206","#c65418","#c04536"))+
  scale_fill_manual(name ="Familias", 
                    values=c("#fd9409","#fc6940","#c07206","#c65418","#c04536"))+
  scale_y_continuous(name="Número de visitas",expand = c(0, 0),limits = c(-8, 35))+
  scale_x_continuous(name="Riqueza de las tribus",expand = c(0, 0),limits = c(-8, 50))+
  theme(axis.text.x=element_text(hjust=1,size=18,colour="black"), 
        axis.title=element_text(size=20),
        axis.text.y=element_text(size=18,colour="black"),
        panel.border=element_rect(colour = "black", 
                                  fill=NA, size=1.5),
        legend.position="none")+
  guides(fill = guide_legend(override.aes = aes(color = NA)))+ #esto último es para eliminar la "a" de la leyenda
  geom_abline(intercept=6.73, slope=0.879,linetype="dashed",col="darkred")

#regresión lineal, para incorporar al gráfico (pendiente e intercepto)
names(taxa_apoidea)
summary(lm(N_visitas ~ N_sp_tribu, data = tribus))


##########ANALISIS FILOGENETICO#############
###matriz_para_analisis filogenetico solo con especies nativas

#para generar la matriz de interacción
matriz<-as.data.frame(matriz[!(matriz$polinizador==""),]) #vuelvo a incluir la matriz
summary(as.factor(matriz$polinizador)) #eliminar filas sin info

matriz[matriz == "Apis_melifera"]<-"" #esto es para eliminar a estas abejas exóticas
matriz[matriz == "Bombus_dahlbomii"]<-"Apidae" 
matriz[matriz == "Bombus_exoticos"]<-""
matriz[matriz == "Bombyliidae"]<-"Diptera"
matriz[matriz == "Otros_dipteros"]<-"Diptera"
matriz[matriz == "Psocoptera"]<-"Otros"
matriz[matriz == "Syrphidae"]<-"Diptera"
matriz[matriz == "Thysanoptera"]<-"Otros"
matriz[matriz == "Hemiptera"]<-"Otros"
matriz<-as.data.frame(matriz[!(matriz$polinizador==""),])
summary(as.factor(matriz$polinizador)) #eliminar filas sin info

#eliminar especies que no hacen match con la filogenia (ver más abajo)
names(matriz)
matriz<-matriz[!(matriz$plant_species=="Mutisia_sinuata" | matriz$plant_species=="Mutisia_subulata_rosmarinifolia" | matriz$plant_species=="Oriastrum_lycopodioides" | matriz$plant_species=="Calceolaria_crenatiflora"),]

datos<-data.frame(higher=matriz$polinizador, lower=matriz$plant_species,
                  webID=seq(0,0,1406), freq=matriz$freq)
visitas<-as.data.frame(frame2webs(datos, type.out = "array"))
names(visitas)<-gsub(names(visitas),pattern=".0",replacement="") #remplaza los nombres
row.names(visitas)
colnames(visitas)

#distancia ecologica
dist_visitantes<-as.matrix(designdist(visitas,method="1 - (J/(A+B-J))", abcd=F))
dist_visitantes
dist_visitantes<-dist_visitantes[order(rownames(dist_visitantes)),order(colnames(dist_visitantes))]

class(dist_visitantes)
mean(dist_visitantes)
(sd(dist_visitantes))/(sqrt(length(dist_visitantes)))
length(dist_visitantes)

#########
#cargar filogenia, generación de matriz

#obtener solo los nombres de especies unicos
matriz$plant_species<-as.factor(matriz$plant_species)
matriz<-as.data.frame(matriz[!(matriz$plant_species==""),]) 
filomatic_names<-matriz[!duplicated(matriz[,c("plant_species")]),]
filomatic_names$plant_species

#construyendo la matriz para phylomatic (splist)
names(filomatic_names)
splist<-data.frame(species=filomatic_names$plant_species, genus=filomatic_names$plant_genera,
                 family=filomatic_names$plant_family) #los titulos deben ser en ingles: species, genus and family
is.na(splist$genus) #debe existir información de todos los niveles (especie, genero, familia)
is.na(splist$family)
splist

#para cambiar las familias en caso de error
#splist$family
#splist[c(140,141,142),"family"]<-"Montiaceae" 
#splist[c(145,146,147),"genus"]<-"Mutisia" 
#splist[c(171),"family"]<-"Thymelaeaceae"
#splist[c(177),"family"]<-"Xanthorrhoeaceae"
#splist[c(229),"family"]<-"Vivianiaceae"
#splist[c(37),"family"]<-"Vivianiaceae"
#splist[c(54,55),"family"]<-"Calceolariaceae"

nodes<-read.csv("nodes.csv",sep=",",header=T) #archivo con la edad de los nodos (ver paper)
phylo<-read.newick("PhytoPhylo.txt") ##archivo con la backbone phyloegeny (ver paper)

library(picante)
library(phytools)

#esta es la funciòn que construye la filogenia
filo<-S.PhyloMaker(spList=splist, tree=phylo, nodes=nodes) # run the function S.PhyloMaker.

#grafico de las filogenias
str(filo) # the structure of the ouput of S.PhyloMaker.
par(mfrow=c(1,3),mar=c(0,0,1,0)) # show the phylogenies of the three scenarios.
plot(filo$Scenario.1,cex=0.3,main="Scenarion One")
plot(filo$Scenario.2,cex=0.3,main="Scenarion Two")
plot(filo$Scenario.3,cex=0.3,main="Scenarion Three")

filo1<-filo$Scenario.1
filo2<-filo$Scenario.2
filo3<-filo$Scenario.3

# distancia filogenetica entre especies
phylo_dist1<-decostand(cophenetic.phylo(filo1), method="range")
phylo_dist2<-decostand(cophenetic.phylo(filo2), method="range")
phylo_dist3<-decostand(cophenetic.phylo(filo3), method="range")

# estadìstica descriptiva de la distancia
mean(phylo_dist3)
(sd(phylo_dist3))/(sqrt(length(phylo_dist3)))
length(phylo_dist3)

phylo_dist1<-phylo_dist1[order(rownames(phylo_dist1)),order(colnames(phylo_dist1))] #ordenar alfabéticamente
phylo_dist2<-phylo_dist2[order(rownames(phylo_dist2)),order(colnames(phylo_dist2))] #ordenar alfabéticamente
phylo_dist3<-phylo_dist3[order(rownames(phylo_dist3)),order(colnames(phylo_dist3))] #ordenar alfabéticamente

#comparar nombres entre matrices de distancia
row.names(phylo_dist1)
row.names(dist_visitantes)
print(setdiff(row.names(phylo_dist1),row.names(dist_visitantes)))
print(setdiff(row.names(dist_visitantes),row.names(phylo_dist1)))
#se debe volver a estimar la distancia para que los cambios se hagan efectivos)

#TEST de MANTEL
mantel(phylo_dist1,dist_visitantes,method="pearson")
plot(phylo_dist1,dist_visitantes)

#grafico de Mantel
library(ggplot2)
df<-data.frame(Phylo_dist=phylo_dist1[lower.tri(phylo_dist1)], Eco_dist=dist_visitantes[lower.tri(dist_visitantes)])
ggplot(df, aes(x=Phylo_dist, y=Eco_dist)) +
  geom_point() + 
  theme_test(base_size = 14, base_family = "Arial") + 
  labs(x="Phylogenetic distance", y="Ecological distance") + 
  theme(axis.title = element_text(face="bold"), axis.text = element_text(colour="black")) +
  geom_smooth(method= lm, formula=y ~ poly(x, 2), col= "red", se=TRUE,level=0.95)

#filogenia y distribuiòn de polinizadores
library(ggtreeExtra)
library(ggtree)
library(treeio)
library(tidytree)
library(ggstar)
library(ggplot2)
library(ggnewscale)

#cargar la matriz de visitantes
names(visitas) # no incluye abejas exoticas
visitas
colSums(visitas)
visitas2<-(stack(visitas))
visitas2$planta<-rep(c(row.names(visitas)),rep=12)
summary(visitas2)
str(visitas2)
#visitas2<-as.factor(visitas2$planta)


#instalar paquetes asociados a ggtree
#if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
#BiocManager::install("ggtree") #para instalar ggtree
#BiocManager::install("ggtreeExtra") #para instalar ggtreeExtra
#BiocManager::install("phyloseq")

# extract the clade label information. Because some nodes of tree are annotated to genera,
# which can be displayed with high light using ggtree.
filo1$node.label
nodeids<- nodeid(filo1, filo1$node.label[nchar(filo1$node.label)>3])
nodedf<-data.frame(node=nodeids)
nodedf
nodelab <- gsub("[\\.0-1]", "", filo1$node.label[nchar(filo1$node.label)>3])
nodelab
poslist <- c(20, 20, 0.5, 0.1, 0.1, 0.25, 1.6, 1.6, 1.2, 0.4,
              0.3, 0.4, 0.4, 0.4, 0.6, 1.2, 0.4,
             0.3, 0.4, 0.4, 0.4, 0.6) # estos parámetros no los entiendo
labdf<- data.frame(node=nodeids, label=nodelab, pos=poslist)
labdf
labdf<-labdf[c(-1,-2,-3,-4,-5,-6,-7,-8,-10,-12,-13,-14,-15,-17,-18,-20),]
labdf<-rbind(labdf,c(320,"Cactaceae",10)) # incluye datos
labdf<-rbind(labdf,c(341,"Apiales",0.6))
labdf<-rbind(labdf,c(390,"Calceolariceae",10))
labdf<-rbind(labdf,c(287,"Oxalidales",0.8))
labdf<-rbind(labdf,c(238,"Asparagales",0.8))
labdf<-rbind(labdf,c(234,"Liliales",0,7))
labdf$node<-as.integer(labdf$node) # hay que modificar el tipo de variable
labdf$pos<-as.numeric(labdf$pos) # hay que modificar el tipo de variable
str(labdf)


#grafico filogenia
p<-ggtree(filo1, layout="fan", size=0.5, open.angle=5, 
          branch.length='dN_vs_dS')+
          geom_tiplab(size=0)+
  geom_hilight(data=labdf, mapping=aes(node=node,fill=label),
               extendto=180, alpha=0.3, size=0.2) + 
  scale_fill_manual(values=c("gray70", "gray25","gray25", "gray70","gray75",
                             "gray70", "gray70","gray25", "gray70",
                            "gray70","gray25", "gray70"),
                             guide="none")+
  geom_cladelab(data=labdf, mapping=aes(node=node, label=label),
              hjust=0.5,angle="auto",barsize=0,
              horizontal=FALSE, fontsize=1.8,fontface="plain",offset=6.5,offset.text=10)
  #geom_text(aes(label=node), hjust=-.3, size=2.5) #esta funcion permite mirar los números de los nodos para que sean includas nuevas familias 

p               
#theme(legend.position="none")

#ordenar factores que serán presentados en plot circular
names(visitas2)
summary(visitas2$ind)
#los nombres deben tener el mismo formato que visitas2
visitas2$ind<-factor(visitas2$ind, 
                              levels = c("Andrenidae",
                                         "Apidae", "Colletidae", "Halictidae",
                                         "Megachilidae","Otros_himenopteros",
                                         "Diptera",
                                         "Coleoptera","Lepidoptera","Trochilidae",
                                         "Passeriforme", "Otros")) #deben tener el mismo formato que "visitas2"
str(visitas2)
#grafico con la filogenia circular. La leyenda de frecuencia la borro
#manualmente
names(visitas2)
visitas2$bin <- ifelse(visitas2$values == 0, 0, 1) #transforma los valores a binarios
str(visitas2)
summary(visitas2$ind)

p+new_scale_fill()+
  geom_fruit(data=visitas2, geom=geom_tile,
           mapping=aes(y=planta, x=ind, alpha=bin, fill=ind),
           offset=0.10,pwidth=1)+
   scale_fill_manual(values=c("#fd9409","#fc6940","#c07206","#c65418",
                              "#c04536","#77161b",
                              "darkgreen","purple","cyan",
                              "red","blue","gray"),guide="none")+
  geom_treescale(fontsize=1.5,width=50, linesize=1, x=0, y=-100,offset=1)

#agregar esta leyenda dentro de scale_fill_manual para evaluar
# que todo esté ok: guide=guide_legend(keywidth=0.3,keyheight=0.3,ncol=2,override.aes=list(size=2,alpha=1),order=1)
#filo$tip.label[filo$tip.label=="Monninalinearifoloa"] <- "Monnina_linearifolia"

