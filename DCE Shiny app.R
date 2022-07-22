
library(tibble)
library(tidyverse)
library(dplyr)
library(matrixStats)

### On a enlever toutes ces librairies car ne sont pas supporter par shinyapps.io
#On les a donc intégrer directement dans le code (ex : BBmisc::isError())

#library(idefix)
#library(AlgDesign)
#library(choiceDes)
#library(fastDummies)
#library(BBmisc) #pour avoir la commande is.error


rsconnect::setAccountInfo(name='maurinedt',
                          token='4E474C1B4922F5D7D69A13FB7F4F144B',
                          secret='fZ4GVYzq2s6bSiO8US7T2H+yLzIz4NsKIQhPkYie')

### FONCTION POUR TROUVER LE MEILLEUR DESIGN ###

fonction_dce <- function(attributes, alts, nochoice ,csets, priors ) {
  
  #######On fait toutes les combinaisons possibles
  
  toexp <- list()
  for (i in attributes){
    te <- seq(1,i) # te est une liste de valeur allant de 1 à la valeur du niveau de chaque attribut
    toexp[[length(toexp) + 1]] <- te  #On a ajoute te à la suite
  }
  profiles <- expand.grid(toexp) #génère ttes les combinaisons possibles
  profiles <- profiles[sample(1:nrow(profiles)), ] #on mélange tt 
  
  
  
  #######On crée aléatoirement une data qui génère des alt qui ne sont pas identiques à chque questions
  ### Ici c'était la première méthode mais assez limité qu'elle ne donnait 
  ###pas assez de combinaisons possibles
  
  #DATA <- list()
  #for (D in 1:5){  #on répète 5 fois ce procédé (peut-être diminuer le nb d'itérations)
  #databis <- profiles[c(2:nrow(profiles)), 1:ncol(profiles)] #data profiles sans la 1ere ligne
  #data <- profiles[1, 1:ncol(profiles)] #on crée une data avec unqiuement la 1ere ligne
  
  #n <- 1 
  #m <- 0 
  #a <- data[n,1:ncol(data)]
  #b <- (databis[1, 1:ncol(databis)])
  
  #while (m <60000){ #on répète jusqu'à ce que m soit égal à 60 000 
  #  m <- m +1 
  #  somme <- 0
  #  if (is.na(b[1])){ # Si le premier élément de b est NA on arrête la boucle WHILE
  #    break
  #  }
  #  for (i in (1:ncol(a))){ #On regarde chaque élém de a et b 
  #    if (a[i] == b[i]){ # et si on a 2 elem identiques on ajoute 1 à la variable somme
  #      somme <- somme +1
  #    }
  #  }
  #  if (somme == 0){ # Si la variable somme est égale à 0 cela veut dire que a et b sont totalement diff
  #    data <- rbind(data, b) # Donc on ajoute la ligne b à la data 
  #    databis <- anti_join(databis,b) #on enlève la b à la databis
  #    n <- n+1 
  #    a <- (data[n,1:ncol(data)]) #on regarde maintenant la dernière ligne de la data
  #  }else{ # si la variable somme n'est pas égale à 0 
  #    databis <- databis[sample(1:nrow(databis)), ] #On méla,ge ttes les lignes de databis
  #    b <- (databis[1, 1:ncol(databis)]) #on reprend la 1ere ligne de databis dans b 
  #  }
  #}
  #On regarde maintenant la data de ttes les alt et questions possibles
  #vu qu'on a déterminé que alt est 2
  #on ne veut pas que la dernière question de la data n'est qu'une seule alt 
  #if (nrow(data)%% 2 != 0 ){ #si la data est impaire
  #  data <- data[-nrow(data),] #alors on enlève la dernière ligne de la data
  #}
  #DATA[[D]] <- data #pour les 5 itérations on ajoute la data dans la liste DATA
  #}
  
  #On met maintenant dans une liste le nombre de ligne des 5 datas disponibles
  #liste_nb_ligne_max <-c(nrow(DATA[[1]]),nrow(DATA[[2]]),nrow(DATA[[3]]),nrow(DATA[[4]]),nrow(DATA[[5]]))
  
  
  #######On crée aléatoirement une data qui génère des alt qui ne sont pas identiques à chque questions
  ### 2ème méthode qui fonctionne beaucoup mieux, elle mélange toutes les combinaisons possibles
  
  DATA <- list() #On crée une liste vide qui va accueillir nos 2 datas aléatoires
  
  for (D in 1:2){ # pour chaque DATA
    profile <- profiles[(sample(1:nrow(profiles))),] #on méolange ttes les combinaisons possibles
    databis <- profiles[c(2:nrow(profiles)), 1:ncol(profiles)] #data profiles sans la 1ere ligne
    data <- profiles[1, 1:ncol(profiles)] #on crée une data avec uniquement la 1ere ligne
    
    m <- 0 #m est le nombre de répétition
    a <- data[1,1:ncol(data)] #la variable a correspondont à un 1ère ligne de data
    b <- (databis[1, 1:ncol(databis)]) #la variable b correspond à la 1ère ligne de databis
    
    while (m <70000){ #on se limite à 70 000  répétitions, ce chiffre est grand et est pris au hasard
      #normalement avec les essais que j'ai fait 20 000 suffirait mais prenons large
      m <- m +1 # à chaque fois on ajoute 1 à notre paramètre m
      somme <- 0 #on crée une variable somme qui va nous donner une indication sur la similarité des
      #variables a et b 
      if (is.na(b[1,1])){ #si b est constitué que de NA on sort de la boucle, lorsque ttes les lignes de
        #databis sont  prises, R prend une ligne constituée uniquement de NA 
        break
      }
      for (i in (1:ncol(a))){
        #Ici on compare chaque élément de a et b et si on a des éléments identiques, 
        #on ajoute 1 à la variable somme
        if (isTRUE(a[1,i] == b[1,i])){
          somme <- somme +1
        }
      }
      
      if (somme == 0){
        # Si la variable somme est égale à 0 cela veut dire que a et b sont totalement différentes
        #donc on fusionne la ligne b à la data et on enlève b à databis
        #puis on remélange databis et on prend une nouvelle ligne dans databis qui va devenir
        #b qu'on enlève dans databis
        #puis on affecte à a la dernière de data qu'on vient d'ajouter
        data <- rbind(data, b)
        databis <- anti_join(databis,b)
        y <- sample(1:nrow(databis),size = 1)
        c <- (databis[y, 1:ncol(databis)])
        data <- rbind(data,c)
        databis <- anti_join(databis,c)
        a <- (data[nrow(data),1:ncol(data)])
        b <- (databis[1, 1:ncol(databis)])
      }else{
        # Si la variable somme n'est pas égale à 0
        #on prend aléatoriement une nouvelle ligne dans databis qui être le nouveau b 
        y <- sample(1:nrow(databis),size = 1)
        b <- (databis[y, 1:ncol(databis)])
      }
    }
    if (nrow(data)%% 2 != 0 ){ #si la data est impaire on enlève la dernière ligne
      data <- data[-nrow(data),]
    }
    DATA[[D]] <- data #on ajoute la data dans la liste DATA qui doit accueillir deux listes "data"
  }
  
  
  liste_nb_ligne_max <-c(nrow(DATA[[1]]),nrow(DATA[[2]])) # on calcule les longueurs de chaque data puis
  #on les stocke dans un vecteur 
  
  
  # on met dans la data, la DATA avec le plus de lignes possibles
  data <- DATA[[match(max(liste_nb_ligne_max), liste_nb_ligne_max)]]
  
  
  row <- rep(1:(nrow(data)/2), each = 2) #on ajoute les num des lignes et on répète à chq fois 2x car 2 alt
  alt <- rep(1:2, (nrow(data)/2)) # on ajoute à lignes les alt 1 et 2
  allsets <- cbind(row, alt, data) # dans la data allsets on les lignes les alts et la data qu'on a pris
  
  
  if (nrow(allsets) < csets*2){
    #si ttes les questions possibles avec les alternatives dans allsets est plus petites que le nombre d'ensemble
    #nécessaire la fonction s'arrête et affiche ce message
    stop("Le nombre d'ensemble est trop grand",
         " Essayez avec ", nrow(allsets)/2, " maximum")
  }
  
  #######Fonction qui crée l'effect coding 
  
  dummify <- function(randomdce, alts){
    temp_mat <- data.frame(NA) # temp_mat data nulle
    randomdce_bis <- randomdce[, 3:ncol(randomdce)] #on crée randomdce_bis qui est randomdce sans row et alt
    for (col in 1:ncol(randomdce_bis)){ # pour chq col de randomdce_bis
      temp_dum <- fastDummies::dummy_cols(randomdce_bis[,col]) # crée les dummies de chaques attributs ici col
      temp_dum <- temp_dum[,2:ncol(temp_dum)] #on prend qu'a partit de la 2ème col
      temp_dum <- as.data.frame(temp_dum) #on transforme en data
      for (x in 1:ncol(temp_dum)){ #pour chaque niveaux on renomme les colonnes
        colnames(temp_dum)[x] <- paste0("Var",col,x) #on colle var avec le num de la col et le num du niveau
        
        for (i in 1:nrow(temp_dum)){ #pour chaque ligne de temp_dum
          #on regarde si la ligne est consituté de 0 et à la fin d'un 1
          #Si c'est bon alors on remplace tout par -1
          if (all((temp_dum[i,1:ncol(temp_dum)] == c(rep(0,(ncol(temp_dum)-1)),1))) == TRUE){
            temp_dum[i,1:(ncol(temp_dum))] <- c(rep(-1,(ncol(temp_dum))))
          }
        }
      }
      # à chq itérations on ajoute la data temp_dum à la data temp_mat
      temp_mat <- cbind(temp_mat, temp_dum)
      
    }
    temp_mat[1] <- randomdce[1]  #On ajoute à la colonne 1 la colonne 1 de la data randomdce
    colnames(temp_mat)[1] <- "task"  #on renomme la 1ere colonne par task 
    
    #On ajoute la colonne avec les alts et on le renomme "alt"
    temp_mat <- tibble::add_column(temp_mat, rep(1:alts, length.out = nrow(temp_mat)) ,.after = "task")
    colnames(temp_mat)[2] <- "alt"
    
    #Maintenant on enlève la dernière colonne de chaque attribut
    for(i in 1:length(attributes)){
      for (j in 1:ncol(temp_mat)){
        if (isTRUE((stringi::stri_sub(colnames(temp_mat)[j],from = 4, to = 4)  == i))){
          if (isTRUE((stringi::stri_sub(colnames(temp_mat)[j],-1) == attributes[i]))){
            temp_mat <- temp_mat[, -j]
          }
        }
      }}
    return(temp_mat)
    
  }
  
  
  
  #######Fonction pour calculer le D.error
  
  #On utilise directement le package idefix
  D.error <- function(des, priors, alts){
    Derr <- idefix::DBerr(par.draws = MASS::mvrnorm(10, priors, Sigma = diag(length(priors))), des = as.matrix(des[,-c(1,2)]), n.alts = alts)
    
    ###### Ajout essai
    if (is.nan(Derr)){
      Derr <- 0
    } 
    
    return(Derr)
  }
  
  
  # On crée 2 listes, une pour le design et une pour les errors
  liste_designe <- c()
  liste_derror <- c()
  
  
  
  
  ####### Nb iétrations = 15 Pour trouver le design optimal
  
  for (D in 1:15){
    #on va "piocher" au hasard le nombre d'ensemble nécessaire dans allsets
    #puis on va utiliser la fonction dummify au design choisi
    #puis on va calculer le d-error du design choisi 
    #on stocke les designs et les d.error dans les listes créées
    random.sets <- sample.int(max(allsets$row),csets)
    randomdce <- allsets[allsets$row %in% random.sets,] 
    randomdce_d <- dummify(randomdce, alts)
    Derror <- D.error(des = randomdce_d, priors = priors, alts = alts)
    liste_derror <- append(liste_derror, Derror)
    liste_designe[[D]] <- randomdce_d
  }
  
  
  #On prend le design avec le D.error la plus basse et on le met dans la data temp_mat 
  temp_mat <- liste_designe[[match(min(liste_derror),liste_derror)]]
  
  
  
  
  #######Modification pour avoir les niveaux dans une variable attribut
  
  #Ici on cherche à enlever le design en effect coding
  #du coup avec les noms de lignes de temp-mat on retrouve les lignes dans allsets qui est dans la forme 
  #souhaitée
  data_finale <- data.frame()
  for (i in c(unique(temp_mat[,1]))){
    for (j in 1:nrow(allsets)){
      if (i == allsets$row[j]){
        data_finale <- rbind(data_finale, allsets[j,])
      }
    }
  }
  
  
  
  #######Option-Out / choice
  
  
  if (nochoice==F){
    #Si on n'a pas d'option out
    data_finale <- data_finale[,-1]
    #On enlève la colonne 1 pour mettre des questions l'ordre croissant ex : 1 1 , 2 2, 3 3
    question <- rep(1:(nrow(data_finale)/2), each = 2)
    data_finale <- cbind(question, data_finale)}
  else{
    #Si on a une option out
    data_finale <-data_finale[,-c(1,2)]
    #On enlève les colonnes 1 et 2 car les questions et alts vont changer vu qu'on en a 3
    D <- data.frame()
    for (i in seq(1,(nrow(data_finale)-1), by = 2)){
      #on ajoute la ligne de l'opt-out dans la data 
      D <- rbind(D, data_finale[i,])
      D <- rbind(D, data_finale[i+1,])
      D <- rbind(D, rep.int(0, (ncol(data_finale))))
    }
    #on refait le alt car là on a 3 ligne pour ch alt
    #on refait les nums questions car 3 alts
    #on ajoute la colonne opt_out où la col est égale à 1 lorsque c'est l'opt-out
    alt <- rep(1:3, (nrow(D)/3))
    question <- rep(1:(nrow(D)/3), each = 3)
    `Opt-Out` <- rep(c(0,0,1), (nrow(D)/3))
    
    data_finale <- cbind(question,alt,`Opt-Out`,D)
  }
  
  
  
  
  #######On stocke ttes les infos finales dans la liste results
  
  
  results <- list("design" = temp_mat, "design_final" = data_finale, "D-error" = min(liste_derror))
  return(results)
}



### FONCTION POUR ATTRIBUER LES NOMS A UNE DATA ###

#Attention il faut un nom_niveaux qui soit une liste qui stocke des vecteurs
# exemple : nom_niveaux <- list(c("Attribut 1 niveau 1", "Attribut 1 niveau 2"),
# c("Attribut 2 niveau 1", "Attribut 2 niveau 2")))
# on a pris l'hypothèse que la max de niveaux possible est 5

noms <- function(nom_attributs, num_attribut,
                 nom_niveaux, data, nochoice){
  if (nochoice == F ){ 
    ## renomme les niveaux
    for (i in 1:num_attribut){
      for (j in 1:nrow(data)){
        if (data[j,i +2] == 1){
          # On fait +2 car on a les colonnes questions et alts
          data[j,i +2] <- nom_niveaux[[i]][1]
        }
        if (data[j,i +2] == 2){
          data[j,i +2] <- nom_niveaux[[i]][2]
        }
        if (data[j,i +2] == 3){
          data[j,i +2] <- nom_niveaux[[i]][3]
        }
        if (data[j,i +2] == 4){
          data[j,i +2] <- nom_niveaux[[i]][4]
        }
        if (data[j,i +2] == 5){
          data[j,i +2] <- nom_niveaux[[i]][5]
        }
        
      }
    }
  }else{
    
    ## renomme les niveaux
    for (i in 1:num_attribut){
      for (j in 1:nrow(data)){
        if (data[j,i +3] == 1){
          # ici on fait +3 car on a les colonnes questions, alt, et opt-out
          data[j,i +3] <- nom_niveaux[[i]][1]
        }
        if (data[j,i +3] == 2){
          data[j,i +3] <- nom_niveaux[[i]][2]
        }
        if (data[j,i +3] == 3){
          data[j,i +3] <- nom_niveaux[[i]][3]
        }
        if (data[j,i +3] == 4){
          data[j,i +3] <- nom_niveaux[[i]][4]
        }
        if (data[j,i +3] == 5){
          data[j,i +3] <- nom_niveaux[[i]][5]
        }
      }
    }
  }
  
  ##### renomme les attributs, c'est à dire les colonnes
  for (i in 1:num_attribut){
    names(data)[names(data) == paste0('Var', i)] <- nom_attributs[i]
  }
  return(data)
}

##############Application shiny###################

library(shinythemes)
library(shiny)
library(shinyjs)



# On définit le "style" de la page
ui <- fluidPage(useShinyjs(),
                ### On a donnée un titre et mis le logoe ueme dans le haut de la page web
                ### et on a ajouté un lien dans l'image
                titlePanel(title=div("DCE Design", 
                                     #img(height = 80, width = 250, 
                                     #   src = "Logo_UEME_2.png", 
                                     # class = "pull-right")
                                     tags$a(
                                       href="https://www.chu-tours.fr/recherche-et-innovation/recherche-professionnels/recherche-clinique-et-translationnelle/organisation/iv-la-plateforme-recherche/ueme-unite-devaluation-medico-economique/", 
                                       tags$img(src="Logo_UEME_2.png", 
                                                width="200",
                                                height="80",
                                                class = "pull-right")
                                     )
                )),
                ### choix du thème ici cosmo
                theme = shinytheme("cosmo"),
                
                htmltools::br(), #saut de ligne
                htmltools::br(), #saut de ligne
                navbarPage("",
                           id ="tab",
                           tabPanel("Introduction", #Titre de l'onglet
                                    
                                    #titre de la section de taille h2 qu'on a centrer et mis en gras
                                    tags$h2("Fonctionnement de l'application",style=paste0('text-align:center;font-weight: bold')),
                                    
                                    htmltools::br(), #saut de ligne
                                    p("Cette application permet de générer un design optimal de questionnaire de choix discrets.") , #paragraphe
                                    
                                    p("Le design créé aura deux alternatives par question, trois si vous sélectionnez l'option-out."), #paragraphe
                                    
                                    p("Vous allez devoir remplir les caractéristiques nécessaires à la génération d'un design, 
                               c'est-à-dire : le nombre d'attributs, le nombre de niveaux pour chaque attribut ainsi 
                               que le nombre d'ensembles."), #paragraphe
                                    
                                    p("Une fois ces informations complétées, vous devrez cliquer sur le bouton",
                                      tags$button(style=paste0('background-color:Black ; color:White ; border:none'), 
                                                  "Activer les prédicteurs"), #un bouton non actif de couleur noir et police blanche avec 
                                      #aucune bordure (même chose pour le reste des boutons)
                                      "Cela calculera automatiquement le nombre de prédicteurs et des valeurs 
                               par défaut seront générées. Si vous le souhaitez, vous pouvez modifier ces valeurs."), #paragraphe
                                    
                                    p("Vous pourrez alors appuyer sur le bouton",
                                      tags$button(style=paste0('background-color:Black ; color:White ; border:none'), #un bouton non actif
                                                  'Générer le design'), "."), #paragraphe
                                    
                                    p("Le calcul peut durer plusieurs minutes."),
                                    
                                    p("Plusieurs éléments vont apparaître :",
                                      tags$ul( #liste 
                                        tags$li("Le design"), #non numérotée
                                        tags$li("La D-error"),
                                        tags$li("La D-efficiency.")
                                      )), #paragraphe
                                    
                                    p("Si besoin, vous pouvez recommencer en cliquant sur",
                                      tags$button(style=paste0('background-color:Black ; color:White ; border:none'),
                                                  "Réinitialiser", ),".",  #un bouton non actif
                                      "Sinon, vous pouvez télécharger le design comme tel ou attribuer des noms à vos variables.") , #paragraphe
                                    
                                    p( "Pour attribuer les noms aux attributs et niveaux, allez à l'onglet",
                                       tags$button(style=paste0('background-color:Black ; color:White ; border:none'),
                                                   "Attribution des noms"),"."), #un bouton non actif et paragraphe
                                    
                                    p(" Il vous suffit alors d'entrer les noms qui vous conviennent puis de cliquer sur le bouton ",
                                      tags$button(style=paste0('background-color:Black ; color:White ; border:none'),
                                                  "Sauvegarder les noms"),"."), #un bouton non actif et paragraphe
                                    
                                    p("Vous pouvez ensuite télécharger le design."), #paragraphe
                                    
                                    htmltools::br(), #saut de ligne
                                    htmltools::br(), #saut de ligne
                                    
                                    div(style="position:relative; left:calc(40%);", 
                                        actionButton("lancer", label = "Lancez l'application !")), #un bouton actif situéà gauche à 40%
                                    
                                    htmltools::br(), #saut de ligne
                                    htmltools::br(), #saut de ligne
                                    
                                    tags$tfoot("Cette application a été crée par Maurine Diot en 2022.",
                                               style=('text-align:center')), #note de bas de page
                                    
                                    htmltools::br(), #saut de ligne
                                    
                                    tags$tfoot("Source :", #note de bas de page
                                               tags$ul(    #liste non numérotée
                                                 tags$li("Pérez-Troncoso, D. (2022). DCEtool (1.0.0) [Software]. https://cran.r-project.org/package=DCEtool "),
                                                 tags$li("Package : idefix, choiceDes"))),
                           ),
                           
                           
                           tabPanel("Design", #titre de l'onglet
                                    sidebarPanel(  #Colonne à gauche
                                      
                                      #titre donné à la colonne en gras et centrer 
                                      tags$h3("Paramètres :",style=paste0('text-align:center;font-weight: bold')),
                                      
                                      #entrer numérique 
                                      numericInput("nb_Attribut", "Nombre d'attributs:", 2, min = 2),
                                      
                                      #sorties
                                      uiOutput('dynamic'),
                                      uiOutput('nb_combinaison'),
                                      
                                      #entrer numérique
                                      numericInput("nb_ensemble", "Nombre d'ensembles:", 1, min = 1),
                                      
                                      #selction dans une liste déroulée
                                      selectInput("output_choix", label = "Opt-out", 
                                                  choices = list("Oui" = TRUE, "Non" = FALSE), 
                                                  selected = 1),
                                      
                                      htmltools::br(), #saut de ligne
                                      
                                      #bouton actif
                                      div(style="position:absolute; left: 18%;",
                                          actionButton("para", label = "Activer les prédicteurs")),
                                      
                                      htmltools::br(), #saut de ligne
                                      htmltools::br(), #saut de ligne
                                      
                                      #sortie
                                      uiOutput('priorsbut'), 
                                      
                                      htmltools::br(), #saut de ligne
                                      
                                      #sortie d'un bouton 
                                      div(style="position:relative; left:calc(14%);", uiOutput('gnr')),
                                      #sidebarLayout(
                                      #actionButton("genere", label = "Generer le design"),
                                      #actionButton("reset", "Nouveau")
                                      #uiOutput('rst')
                                      #)
                                      
                                    ), 
                                    
                                    # dans le reste de la page donc à droite de la colonne
                                    mainPanel(
                                      
                                      #titre en gras
                                      tags$h2("Tableau",style=paste0('font-weight: bold')),
                                      
                                      #sortie du design 
                                      uiOutput("spinner"),
                                      
                                      htmltools::br(), #saut de ligne
                                      htmltools::br(), #saut de ligne
                                      htmltools::br(), #saut de ligne
                                      
                                      #titre en gras
                                      tags$h2("D-error",style=paste0('font-weight: bold')),
                                      
                                      #sortie de la valeur de la d-error du design 
                                      uiOutput("d_error"),
                                      
                                      #titre en gras
                                      tags$h2("D-efficiency",style=paste0('font-weight: bold')),
                                      
                                      #sortie de la valeur de la d-effiency du design 
                                      uiOutput("d_eff"),
                                      
                                      htmltools::br(), #saut de ligne
                                      htmltools::br(), #saut de ligne
                                      htmltools::br(), #saut de ligne
                                      
                                      #dans la même ligne on a ajouté deux boutons actifs : télécharger 
                                      #et rénitialiser
                                      fillRow(
                                        #bouton
                                        div(style="position:relative; left:calc(25%);",
                                            downloadButton("downloadData", "Telecharger")),
                                        #sortie
                                        uiOutput('rst'))
                                    ) 
                                    
                           ), 
                           tabPanel("Attribution des noms", #titre deuxième onglet 
                                    #id = "Attribution_des_noms",
                                    
                                    #On a diviser la page en deux colonnes pour avoir d'un côté les noms attributs 
                                    #et de l'autre les noms de niveaux 
                                    
                                    fluidPage(
                                      fluidRow(
                                        column(6,
                                               #1ère colonne
                                               
                                               #titre
                                               tags$h3("Noms des attributs :",style=paste0('font-weight: bold')),
                                               
                                               #sortie
                                               uiOutput("noms_attributs")),
                                        
                                        column(6,
                                               #2èeme colonne
                                               
                                               #titre
                                               tags$h3("Noms des niveaux :",style=paste0('font-weight: bold')),
                                               
                                               #sortie
                                               uiOutput("noms_niveaux"))),
                                      
                                      htmltools::br(), #saut de ligne
                                      htmltools::br(), #saut de ligne
                                      
                                      #On a ajouté à la suite des 2 colonnes
                                      #2 boutons qui sauvegarde les noms et puis télécharge le design
                                      fluidRow(
                                        #fillRow(
                                        column(6, align="center", offset = 3,
                                               actionButton("attribuer_nom", label = "Sauvegarder les noms"),
                                               downloadButton("downloadData_finale", "Télécharger"),
                                               #uiOutput('rst2')
                                        )
                                      )
                                      
                                    ),
                           )
                           
                ) 
) 


# On peut maintenant créer les commandes qui font activer des fonctions ou des sorties, etc etc... 
server <- function(input, output, session) {
  
  ##### Page principale : qui sort la data frame brut avec la d-error et d-effiency.  
  
  ## On cache les onglets créaction et attribution des noms pour avoir uniquement l'onglet introduction
  hideTab("tab", "Design")
  hideTab("tab", "Attribution des noms")
  
  ##Lorsque le bouton lancer est activé on va directement sur l'onglet créaction
  observeEvent(input$lancer,{ 
    showTab("tab", "Design")  # On fait apparaitre l'onglet
    updateTabsetPanel(session, "tab",
                      selected = "Design") #On se met directement dans l'onglet
    
  })
  
  ## On génère autant de cases que d'attributs 
  output$dynamic <- renderUI({
    tags <- tagList()
    for (i in seq_len(input$nb_Attribut)) {
      tags[[i]] <- numericInput(paste0('niveau', i), 
                                paste0("Nombre de niveau dans l'attribut ", i),
                                1, min = 1, max = 5)
    }
    tags
  })
  
  
  ## Ici c'était pour générer une une case le nombre de combinaison possible
  #observeEvent(input$nb_alternative,{
  #   output$nb_combinaison <- renderUI({
  #      nats <- input$nb_Attribut
  #     levels <- c()
  #    for (i in seq_len(nats)){
  #       levels <- append(levels, eval(parse(text = paste0("input$niveau",i))))
  #  }
  # values$levels <- levels
  
  #combinaison <- " "
  #for (i in 1:length(values$levels)){
  #    combinaison <- combinaison *values$levels[i]
  #}
  #values$combinaison <- combinaison
  #textInput("nb_combinaison", "Nombre de combinaison possible",
  #         value = values$combinaison  )
  #})})
  
  ##Tout ce qui est dans values est réactive
  values <- reactiveValues() 
  
  
  ## lorsque le bouton générer est activé le bouton genere apparait
  observeEvent(input$para, { 
    output$gnr <- renderUI({
      actionButton("genere", label = "Generer le design")
    })
  })
  
  
  
  ## lorsque le bouton générer est activé le bouton reset apparait
  observeEvent(input$genere, { 
    output$rst<- renderUI({
      div(style="position:relative; left:calc(25%);",actionButton("reset", "Réinitialiser"))
    })
  })
  
  ##lorsque le bouton reset est activé les entrées ci-dessous sont remise à zéro
  observeEvent(input$reset, {
    refresh()
  })
  
  ## lorsque le bouton sauvegarder les noms est activé le bouton reset apparait dans l'onglet attribuer
  #les noms
  #observeEvent(input$attribuer_nom, { 
  #  output$rst2 <- renderUI({
  #    actionButton("reset2", "Réinitialiser")
  #  })
  #})
  
  ##lorsque le bouton reset est activé les entrées ci-dessous sont remise à zéro
  #observeEvent(input$reset2, {
  #  refresh()
  #})
  
  
  ## Lorsque le bouton para est activé on attribue les inputs aux les variables réactives
  observeEvent(input$para,{
    values$output_choix <- input$output_choix
    values$levels <- input$niveau 
    values$nats <- input$nb_Attribut  
    values$combinaison
  })
  
  
  ## Lorsque le bouton para est activé on stoke ttes les valeurs entrées dans les cases niveaux 
  observeEvent(input$para, {
    nats <- input$nb_Attribut
    levels <- c()
    for (i in seq_len(nats)){
      levels <- append(levels, eval(parse(text = paste0("input$niveau",i))))
    }
    values$levels <- levels
    
  })
  
  
  ##Lorsque le bouton para est activé, il y a autant de cases de prédicteurs qui s'affichent
  observeEvent(input$para, {
    output$priorsbut <- renderUI({
      null <- ifelse(isTRUE(input$output_choix) == TRUE, 1, 0)
      npar <- sum(values$levels)-input$nb_Attribut+null #le nombre de prédicteurs est égales aux sommes des niveaux - nb des attributs
      tags <- tagList()
      for (i in seq_len(npar)) {
        tags[[i]] <- numericInput(paste0('prior', i), 
                                  paste ("Predicteur", i),
                                  0)
      }
      tags
      
    })})
  
  ##Lorsque le bouton genere est activé , on stoke les valeurs des prédicteurs 
  observeEvent(input$genere,{
    #output$nb_combinaison <- renderUI({
    null <- ifelse(isTRUE(input$output_choix) == TRUE, 1, 0)
    npar <- sum(values$levels)-input$nb_Attribut+null
    priors <- c()
    for (i in seq_len(npar)){
      priors <- append(priors, eval(parse(text = paste0("input$prior",i))))
    }
    values$priors <- priors
    showTab("tab", "Attribution des noms")
    #})
  })
  
  
  ##Lorsque le bouton para est activé on stoke ttes les valeurs entrées dans les cases niveaux 
  #observeEvent(input$para, {
  #    nats <- input$nb_Attribut
  #    levels <- c()
  #    for (i in seq_len(nats)){
  #        levels <- append(levels, eval(parse(text = paste0("input$niveau",i))))
  #    }
  #    values$levels <- levels
  #    values$nats <- nats
  
  #})
  
  
  ## Lorsque le bouton genere est activé on sort la data frame
  observeEvent(input$genere,{
    output$spinner <- renderUI({
      shinycssloaders::withSpinner(DT::dataTableOutput("design"))
    })
    
    #on calcule le design en fonction des entrées
    output$design <- DT::renderDataTable({
      dce <- fonction_dce(attributes=values$levels,csets=input$nb_ensemble,
                          alts=2, nochoice=input$output_choix,
                          priors = values$priors)
      
      
      values$designcree2 <- as.data.frame(dce$design)
      values$designcree <- as.data.frame(dce$design_final)
      
      #on calcule de d_error
      values$derror <- round(dce$`D-error`,3)
      
      
      #on calcule le d_efficient
      c <- try(choiceDes::dcm.design.effcy(values$designcree2), silent = T)
      if (BBmisc::is.error(c)){
        values$derror <- NULL
        stop("Le nombre d'ensemble est trop petit")
      }else{
        effy <- choiceDes::dcm.design.effcy(values$designcree2)
      }
      
      values$d_eff <- round((1-effy$D)*100,3)
      
      ###### Ajout essai
      #des <- as.matrix(dce$design_final[3:ncol(dce$design_final)])
      
      #M <- t(des) %*% des
      #D <- det(M)^(-1/ncol(des))
      #if (det(M)==0){
      #  effy <- "val 0" #0
      #  values$d_eff <- 0 #(1-effy)*100
      #return(values$d_eff )
      #}else{
      #  effy <- dcm.design.effcy(dce$design)
      #  values$d_eff <- (1-effy$D)*100
      #return(values$d_eff)
      #}
      
      #designcree <- as.data.frame(dce$design_final) 
      values$designcree 
      
    })
    
    
  })
  
  ##Lorsque le bouton genere est activé on sort la valeur du d_error
  observeEvent(input$genere,{
    output$d_error <- renderUI({
      textInput("d_errorout", " ", value = values$derror )
    })})
  
  
  ##Lorsque le bouton genere est activé on sort la valeur du d-efficiency
  observeEvent(input$genere,{
    output$d_eff <- renderUI({
      #effy <- dcm.design.effcy(values$designcree)
      #values$d_eff <- (1-effy$D)*100
      textInput("d_errorout", "En %", value = values$d_eff  )
    })})
  
  ##Lorsque le bouton downloadData est activé on télécharge le fichier correspondant au design 
  ##mais attention ici l'attribution des noms n'a pas été faite c'est après
  output$downloadData <- downloadHandler(
    filename = "DceDesign.xlsx",
    content = function(file) {
      writexl::write_xlsx(values$designcree, file)
    }
  )
  
  
  #####   page numero deux
  
  ## Lorsque le bouton genere est activé 
  observeEvent(input$genere,{
    
    #on fait autant de cases que d'attribut pour récupérer les noms
    output$noms_attributs <- renderUI({
      tagsbis <- tagList()
      for (j in 1:(input$nb_Attribut)){
        tagsbis[[j]] <- textInput(paste0('nom_attribut', j), 
                                  paste("Nom attribut", j),
                                  value = " ")
      }
      
      tagsbis
    })
    
    
    ## on fait autant de cases que de niveaux pour chq attribut pour récupérer les noms
    output$noms_niveaux <- 
      renderUI({
        m <- 1
        tags <- tagList()
        for (i in 1:(input$nb_Attribut)){
          for (j in 1:(values$levels[i])){
            tags[[m]]<-textInput(paste0('nom_niveau',i,j), 
                                 paste("Attribut", i, "Nom niveau", j),
                                 value = "  ")
            m <- m+1
            
            
          }
        }
        tags
      })  
    
  })
  
  
  ## Lorsque le bouton attribuer_nom est activé 
  #     -on récupère tous les noms et on les stockes
  #     -puis on fait runner la fct noms pour les attribuer à la data 
  observeEvent(input$attribuer_nom, {
    nomatt <- c()
    for (i in 1:(input$nb_Attribut)){
      nomatt <- append(nomatt, eval(parse(text = paste0("input$nom_attribut",i))))
    }
    values$nomatt <- nomatt
    
    
    ESS <- list()
    for (i in 1:(input$nb_Attribut)){
      nolev <- c()
      for (j in 1:(values$levels[i])){
        nolev <- append(nolev, eval(parse(text = paste0("input$nom_niveau",i,j))))
      }
      ESS[[i]] <- nolev
    }
    val <- ESS
    
    values$design_final <- as.data.frame(noms(nom_attributs = values$nomatt , 
                                              num_attribut = input$nb_Attribut,
                                              nom_niveaux = val,
                                              data = values$designcree,
                                              nochoice = input$output_choix))
    
  })
  
  
  ##Lorsque le bouton downloadData est activé on télécharge le fichier correspondant au design avec les noms
  observeEvent(input$attribuer_nom, {
    output$downloadData_finale <- downloadHandler(
      filename = "DceDesign_final.xlsx",
      content = function(file) {
        writexl::write_xlsx(values$design_final, file)
      }
    )
    
  })
  
  
  
  
} 


# Create Shiny object
shinyApp(ui = ui, server = server)