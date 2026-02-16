# site1
library(shiny)
library(ggplot2)
library(DT)
library(MASS)
library(shinythemes)
library(readxl)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Régression Linéaire — Analyse Complète + Prévision"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("dataset_choice",
                  "Jeu de données",
                  choices = c("mtcars","iris","airquality","Boston","Fichier chargé")),
      
      fileInput("file_upload",
                "Charger fichier CSV / Excel",
                accept = c(".csv",".xlsx")),
      
      uiOutput("var_select"),
      
      numericInput("new_x","Valeur X pour prévision", value = 0),
      
      actionButton("run","Lancer Régression")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tableau", DTOutput("datatable")),
        tabPanel("Graphique", plotOutput("plot")),
        tabPanel("Résumé", verbatimTextOutput("summary")),
        tabPanel("ANOVA", verbatimTextOutput("anova")),
        tabPanel("Résidus", DTOutput("res_table")),
        tabPanel("Équation", verbatimTextOutput("equation")),
        tabPanel("Prévision", verbatimTextOutput("prediction")),
        tabPanel("Interprétation détaillée", verbatimTextOutput("interpretation")),
        tabPanel("📘 Rappel de cours", verbatimTextOutput("cours"))
      )
    )
  )
)

server <- function(input, output, session){
  
  # ---------------- DATA ----------------
  data <- reactive({
    
    if (input$dataset_choice=="mtcars") return(mtcars)
    if (input$dataset_choice=="iris") return(iris)
    if (input$dataset_choice=="airquality") return(na.omit(airquality))
    if (input$dataset_choice=="Boston") return(Boston)
    
    if (input$dataset_choice=="Fichier chargé"){
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$name)
      
      if(ext=="csv") return(read.csv(input$file_upload$datapath))
      if(ext=="xlsx") return(read_excel(input$file_upload$datapath))
    }
  })
  
  # ---------------- TABLE ----------------
  output$datatable <- renderDT({
    datatable(data(), extensions="Buttons",
              options=list(dom="Bfrtip",
                           buttons=c("copy","csv","excel","pdf"),
                           pageLength=10))
  })
  
  # ---------------- VARIABLES ----------------
  output$var_select <- renderUI({
    df <- data()
    num_vars <- names(df)[sapply(df,is.numeric)]
    
    tagList(
      selectInput("yvar","Variable Y",num_vars),
      selectInput("xvar","Variable X",num_vars)
    )
  })
  
  # ---------------- MODELE ----------------
  model <- eventReactive(input$run,{
    req(input$xvar,input$yvar)
    lm(as.formula(paste(input$yvar,"~",input$xvar)), data=data())
  })
  
  # ---------------- PLOT ----------------
  output$plot <- renderPlot({
    req(model())
    ggplot(data(), aes_string(input$xvar,input$yvar))+
      geom_point()+
      geom_smooth(method="lm", se=TRUE, color="red")+
      theme_minimal()
  })
  
  # ---------------- SUMMARY ----------------
  output$summary <- renderPrint({ req(model()); summary(model()) })
  
  # ---------------- ANOVA ----------------
  output$anova <- renderPrint({ req(model()); anova(model()) })
  
  # ---------------- RESIDUS ----------------
  output$res_table <- renderDT({
    req(model())
    df <- data()
    df$Fitted <- fitted(model())
    df$Residuals <- residuals(model())
    datatable(df)
  })
  
  # ---------------- EQUATION ----------------
  output$equation <- renderPrint({
    req(model())
    cfs <- coef(model())
    a <- round(cfs[1],4)
    b <- round(cfs[2],4)
    
    if(b>=0){
      cat(input$yvar,"=",a,"+",b,"×",input$xvar)
    } else {
      cat(input$yvar,"=",a,"-",abs(b),"×",input$xvar)
    }
  })
  
  # ---------------- PREVISION ----------------
  output$prediction <- renderPrint({
    req(model())
    
    newdata <- data.frame(x=input$new_x)
    names(newdata) <- input$xvar
    
    pred <- predict(model(), newdata, interval="confidence")
    
    cat("PRÉVISION\n\n")
    cat(input$xvar,"=",input$new_x,"\n\n")
    print(round(pred,4))
  })
  
  # ---------------- INTERPRETATION DETAILLEE ----------------
  output$interpretation <- renderPrint({
    
    req(model())
    s <- summary(model())
    
    a <- coef(model())[1]
    b <- coef(model())[2]
    
    p_beta <- s$coefficients[2,4]
    t_beta <- s$coefficients[2,3]
    
    r2 <- s$r.squared
    r2_adj <- s$adj.r.squared
    
    fstat <- s$fstatistic[1]
    df1 <- s$fstatistic[2]
    df2 <- s$fstatistic[3]
    p_global <- pf(fstat, df1, df2, lower.tail=FALSE)
    
    cat("INTERPRÉTATION STATISTIQUE DÉTAILLÉE\n\n")
    
    cat("Test F global :",round(fstat,3)," | p =",signif(p_global,4),"\n")
    cat("β1 =",round(b,4)," | t =",round(t_beta,3),
        " | p =",signif(p_beta,4),"\n\n")
    
    cat("R² =",round(r2,4),
        " | R² ajusté =",round(r2_adj,4),"\n\n")
    
    if(p_beta < 0.05){
      cat("Relation statistiquement significative\n")
    } else {
      cat("Relation non significative\n")
    }
    
  })
  
  # ---------------- RAPPEL DE COURS ----------------
  output$cours <- renderPrint({
    
    cat("
RAPPEL DE COURS — RÉGRESSION LINÉAIRE SIMPLE
============================================

1) DÉFINITION
La régression linéaire simple modélise la relation entre :
- une variable explicative X
- une variable dépendante Y

Modèle : Y = a + bX + ε

a = intercept
b = pente (effet marginal)
ε = erreur aléatoire

--------------------------------------------

2) OBJECTIFS
• Expliquer Y par X
• Quantifier l’effet de X sur Y
• Faire des prévisions
• Tester l’existence d’une relation

--------------------------------------------

3) ESTIMATION
Les coefficients sont estimés par la méthode
des moindres carrés ordinaires (MCO) :
→ minimise la somme des carrés des résidus

--------------------------------------------

4) INTERPRÉTATION DES COEFFICIENTS

Pente b :
Variation moyenne de Y quand X augmente de 1 unité

Intercept a :
Valeur attendue de Y quand X = 0

--------------------------------------------

5) TESTS STATISTIQUES

Test t :
Teste si b ≠ 0

Test F :
Teste si le modèle global est significatif

p-value < 0.05 → effet significatif

--------------------------------------------

6) QUALITÉ DU MODÈLE

R² :
Proportion de variance expliquée

R² ajusté :
Corrige selon taille échantillon

--------------------------------------------

7) HYPOTHÈSES DU MODÈLE

• Linéarité
• Indépendance des erreurs
• Homoscédasticité
• Normalité des résidus

--------------------------------------------

8) LIMITES

• Corrélation ≠ causalité
• Sensible aux valeurs aberrantes
• Mauvaise extrapolation hors domaine

--------------------------------------------

9) PRÉVISION

On peut prédire Y pour une nouvelle valeur de X
avec intervalle de confiance.

")
  })
  
}

shinyApp(ui, server)
