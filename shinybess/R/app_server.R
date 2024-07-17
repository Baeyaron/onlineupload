#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import rms
#' @import cowplot
#' @import fitdistrplus
#' @import BeSS
#' @import ggplot2
#' @import DT
#' @import lm.beta
#' @import reshape
#' @import pROC
#' @import survival
#' @import openxlsx
#' @import shinydashboard
#' @import shinyjs
#' @noRd
app_server <- function(input, output) {
  options(shiny.maxRequestSize = 100 * 1024^2)

  scan <- function(string, sep) {
    re <- c()
    if (string == "") {
      return(re)
    } else {
      b <- strsplit(string, sep, fixed = T)
      for (i in 1:length(b[[1]]))
      {
        if (gregexpr(":", b[[1]][i])[[1]][1] == -1) {
          re_t <- as.numeric(b[[1]][i])
          re <- c(re, re_t)
        } else {
          cut <- gregexpr(":", b[[1]][i])[[1]][1]
          re_f <- as.numeric(substr(b[[1]][i], 1, cut - 1))
          re_b <- as.numeric(substr(b[[1]][i], cut + 1, nchar(b[[1]][i])))
          re_t <- re_f:re_b
          re <- c(re, re_t)
        }
      }
      return(re)
    }
  }



  # Bootstrap#


  Bootstrap <- function(df, seed) {
    set.seed(seed)
    re <- list()
    c <- 1:nrow(df)
    a <- sample(c, nrow(df), replace = T)
    t <- c[!(c %in% unique(a))]
    train <- df[a, ]
    re[[1]] <- train
    test <- df[t, ]
    re[[2]] <- test
    return(re)
  }


  # 95%CI for c#

  CstatisticCI <- function(x) {
    se <- x["S.D."] / sqrt(x["n"])
    Lower <- sprintf("%0.4f", 1 - x["C Index"] - 1.96 * se)
    Upper <- sprintf("%0.4f", 1 - x["C Index"] + 1.96 * se)
    C_index <- sprintf("%0.4f", 1 - x["C Index"])
    cbind(C_index, Lower, Upper)
  }

  # trans the format#
  tp <- function(x) {
    re <- matrix(NA, ncol = ncol(x), nrow = nrow(x))
    for (i in 1:ncol(x))
    {
      re[, i] <- sprintf("%0.4f", x[, i])
    }
    colnames(re) <- colnames(x)
    rownames(re) <- rownames(x)
    return(re)
  }



  # var view#
  var_view <- function(df) {
    sec <- rep(1:5, ceiling(ncol(df) / 5))[1:ncol(df)]
    re <- c()
    if (ncol(df) < 5) {
      for (i in 1:ncol(df))
      {
        re_t <- cbind(Num = i, Var = colnames(df)[i])
        re <- cbind(re, re_t)
      }
    } else {
      for (i in 1:5)
      {
        Var <- colnames(df)[sec == i]
        re_t <- cbind(Num = seq(i, ncol(df), 5), Var)
        if (length(seq(i, ncol(df), 5)) < ceiling(ncol(df) / 5)) {
          re_t <- rbind(re_t, matrix("", nrow = 1, ncol = 2))
        }
        re <- cbind(re, re_t)
      }
    }
    return(re)
  }
  ntabs <- 2

  tabnames <- c("setting", "result")
  tablabels <- c("Setting", "Result")

  output$Sidebar <- renderUI({
    Menus <- vector("list", 2)
    Menus[[1]] <-   menuItem(tablabels[1], tabName = tabnames[1], icon = icon("gear"),selected = T)
    Menus[[2]] <-   menuItem(tablabels[2], tabName = tabnames[2], icon = icon("search"))


    do.call(function(...) sidebarMenu(id = 'sidebarMenu', ...), Menus)
  })
  output$TABUI <- renderUI({
    Tabs <- vector("list", 2)
    Tabs[[1]] <- tabItem(tabName = tabnames[1],
                         fluidRow(
                           tabBox(
                             title = "Variable Setting",
                             side = "right", height = "auto",
                             shiny::selectInput("family", "Model type",
                                                c("Gaussian for linear model" = "gaussian","Binomial for logisitic model" = "binomial","Cox for Cox model" = "cox")),
                             conditionalPanel(
                               condition = "input.family == 'gaussian'",
                               textInput(
                                 inputId = "ylm",
                                 label = "Numerical response variable (input the column number):"
                               )
                             ),
                             conditionalPanel(
                               condition = "input.family == 'binomial'",
                               textInput(
                                 inputId = "yglm",
                                 label = "Binomial response variable (input the column number):"
                               )
                             ),
                             conditionalPanel(
                               condition = "input.family == 'cox'",
                               textInput(
                                 inputId = "time",
                                 label = "Survival time (input the column number):"
                               ),
                               textInput(
                                 inputId = "state",
                                 label = "Event (input the column number):"
                               )
                             ),
                             textInput(
                               inputId = "con_var",
                               label = "Explanatory variable (input the column number):"
                             )),

                           tabBox(
                             title = "Method Settings",
                             side = "right", height = "auto",
                             shiny::selectInput("smethod", "Method used to determine the optimal model size",
                                                c("Sequential selection" = "sequential","Golden section selection" = "gsection","Fixed model size" = "fixed")),
                             conditionalPanel(
                               condition = "input.smethod == 'fixed'",
                               numericInput(
                                 inputId = "size",
                                 label = "Fixed model size:",value = 1
                               )),
                             selectInput("method", "Prediction method", c("NO"="NO", "Train/Test"="Train/Test", "Bootstrap"="Bootstrap", "Cross-validation"="Cross-validation")),
                             conditionalPanel(
                               condition = "input.method == 'Train/Test'",
                               fileInput("res_df", h4("Choose .CSV File as the Testing Set"), accept = c(".csv"), buttonLabel = h5("Browse"), placeholder = "No file selected")
                             ),
                             conditionalPanel(
                               condition = "input.method == 'Bootstrap'",
                               numericInput("seed", label = "Random seed", value = 101)
                             ),
                             conditionalPanel(
                               condition = "input.method == 'Cross-validation'",
                               numericInput("seed2", label = "Random seed", value = 101),
                               numericInput("kfold", label = "Fold number", value = 10)
                             ))),

                         actionButton("act","Submit"),
                         h3(c("Column Numbers of  Variables")),
                         DTOutput('view',width = "50%",height = "auto"))


    Tabs[[2]] <- tabItem("result",
                         h3(textOutput(outputId = "title")),
                         h3(textOutput(outputId = "title2")),
                         tableOutput(outputId = "fitness"),
                         conditionalPanel(
                           condition = "input.method != 'NO'",
                           tags$hr(),
                           h3("Prediction performance"),
                           plotOutput(outputId = "plot", width = 600, height = "auto"),
                           tableOutput("cindex")),
                         width = 7)

    do.call(tabItems, Tabs)


  })
  output$title <- renderText(c("Model Summary"))
  output$title2 <- renderText(c(" "))
  observeEvent(input$file$datapath, {
    re_p <- NULL
    output$title <- renderText(c("Column numbers of variables"))
    output$title2 <- renderText(c(" "))
    output$view <- DT::renderDataTable(
      {
        if (is.null(input$file)) {
          return(NULL)
        } else {
          df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
          return(var_view(df))
        }
      },
      options = list(lengthChange = FALSE),
      rownames = F
    )


    output$fitness <- renderTable(NULL)
    output$plot <- renderPlot(NULL, width = 600, height = 500)
    output$cindex <- renderPlot(NULL, width = 600, height = 500)
  })



  observeEvent(input$act, {
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    P <- c()
    re_p <- NULL
    if(input$family=="binomial")
    {
      y <- scan(input$yglm, c(","))
      con <- scan(input$con_var, c(","))
      P <- c()

      df <- na.omit(df)
      if (input$smethod=="sequential")
      {
        re_b <- bess(x=as.matrix(cbind(df[,c(con)])),
                     y=df[,c(y)], method = "sequential",family = "binomial")
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_b$beta[,which.min(re_b$EBIC)]!=0]
      }
      else if (input$smethod=="gsection")
      {
        re_bg <- bess(x=as.matrix(cbind(df[,c(con)])),
                      y=df[,c(y)], method = "gsection",family = "binomial")
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_bg$beta[,ncol(re_bg$beta)] !=0 ]
      }
      else
      {
        re_bg <- bess.one(x=as.matrix(cbind(df[,c(con)])),
                          y=df[,c(y)], family = "binomial",s=input$size)
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_bg$beta !=0 ]
      }

      df_glm <- data.frame(cbind(df[,c(y,con)]))
      df_glm[, 1] <- as.numeric(as.factor(df_glm[, 1])) - 1
      df_glm <- na.omit(df_glm)
      fullformula <- as.formula(paste0(colnames(df[y]), "~", paste(colnames(df_glm)[var+1], collapse = " + ")))
      jieguo <- glm(fullformula, family = binomial, data = df_glm)

      OR <- exp(coef(jieguo))
      OR1 <- round(OR, digits = 4)
      OR2 <- matrix(OR1, ncol = 1)
      qujian <- exp(confint(jieguo))
      qujian1 <- round(qujian, digits = 4)
      qujian2 <- matrix(qujian1, ncol = 2)
      OR3 <- cbind(OR2, qujian2)
      colnames1 <- c("OR", "Lower", "Upper")
      colnames(OR3) <- colnames1
      jieguo1 <- summary(jieguo)
      jieguo2 <- jieguo1$coefficients

      jieguo0 <- lm.beta(jieguo)
      jieguo1 <- summary(jieguo0)
      jieguo1 <- jieguo1$coefficients
      jieguo2 <- cbind(jieguo2[, 1], jieguo1[, 2], jieguo2[, 2:4])
      jieguo2[, 4] <- (jieguo2[, 1] / jieguo2[, 3])**2
      jieguo3 <- tp(jieguo2)

      d <- rownames(jieguo3)
      k1 <- cbind(d, jieguo3)

      k <- cbind(k1, OR3)
      colnames(k)[1] <- "Variable"
      colnames(k)[2:3] <- c("Estimate", "Standardized")
      colnames(k)[5] <- "Chi-Square"
      colnames(k)[6] <- "Pr(>|Chi-Square|)"
      dfc <- df_glm
      dfc[,-1] <- scale(df_glm[,-1])
      jieguo <- glm(fullformula, family = binomial, data = dfc)

      re_k <- as.data.frame(k)
      re_k[,3] <- sprintf("%0.4f", jieguo$coefficients)



      if (input$method == "Bootstrap") {
        df_t <- Bootstrap(df_glm, input$seed)
        df_train <- df_t[[1]]
        df_test <- df_t[[2]]
        fit_b <- rms::Glm(fullformula, family = binomial, data = df_train)
        prob <- predict.glm(fit_b, df_test, family = binomial, type = "response")
        roc <- roc(df_test[, y], prob,ci=T)
        t <- roc$ci
        lab <- paste0("AUC (95% CI) = ", sprintf("%0.2f", roc$auc)," (",sprintf("%0.2f",t[1]),"-",sprintf("%0.2f", t[3]),")")
        P <- ggroc(roc, colour = "red") + theme_classic() +
          geom_abline(slope = 1, intercept = 1) +
          labs(x = "Specificity", y = "Sensitivity") +
          annotate("text", x = 0.6, y = 1, label = lab, size = 5) +
          theme(axis.line.x = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.line.y = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.text.x = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.text.y = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.ticks.x = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.ticks.y = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.title.x = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(axis.title.y = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(title = element_text(size = 14, face = "bold")) +
          theme(plot.margin = unit(rep(3, 4), "lines"))
      }
      if (input$method == "Train/Test") {
        df_train <- df_glm
        df_test <- read.csv(input$res_df$datapath, header = TRUE, sep = ",")
        df_test <- data.frame(df_test[,c(y,con)])
        fit_t <- rms::Glm(fullformula, family = binomial, data = df_train)
        prob <- predict.glm(fit_t, df_test, family = binomial, type = "response")
        roc <- roc(df_test[, y], prob,ci=T)
        t <- roc$ci
        lab <- paste0("AUC (95% CI) = ", sprintf("%0.2f", roc$auc)," (",sprintf("%0.2f",t[1]),"-",sprintf("%0.2f", t[3]),")")
        P <- ggroc(roc, colour = "red") + theme_classic() +
          geom_abline(slope = 1, intercept = 1) +
          labs(x = "Specificity", y = "Sensitivity") +
          annotate("text", x = 0.6, y = 1, label = lab, size = 5) +
          theme(axis.line.x = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.line.y = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.text.x = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.text.y = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.ticks.x = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.ticks.y = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.title.x = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(axis.title.y = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(title = element_text(size = 14, face = "bold")) +
          theme(plot.margin = unit(rep(3, 4), "lines"))
      }

      if (input$method == "Cross-validation") {
        set.seed(input$seed2)
        df_na <- df_glm
        df_na$fold <- sample(rep(1:input$kfold, ceiling(nrow(df_na) / input$kfold))[1:nrow(df_na)])
        prob <- matrix(NA, nrow(df_na), 1)
        prob <- data.frame(prob)
        rf_p <- list()
        df_t <- list()
        for (i in 1:input$kfold) {
          df_t[[i]] <- df_na[df_na$fold != i, ]
          rf_p[[i]] <- Glm(fullformula, family = binomial, data = df_t[[i]])
          prob[df_na$fold == i, 1] <- predict.glm(rf_p[[i]], df_na[df_na$fold == i, ], family = binomial, type = "response")
        }
        re_t <- data.frame(df_na, prob)
        roc <- roc(re_t[, 1], re_t$prob,ci=T)
        t <- roc$ci
        lab <- paste0("AUC (95% CI) = ", sprintf("%0.2f", roc$auc)," (",sprintf("%0.2f",t[1]),"-",sprintf("%0.2f", t[3]),")")
        P <- ggroc(roc, colour = "red") + theme_classic() +
          geom_abline(slope = 1, intercept = 1) +
          annotate("text", x = 0.6, y = 1, label = lab, size = 5) +
          labs(x = "Specificity", y = "Sensitivity") +
          theme(axis.line.x = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.line.y = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.text.x = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.text.y = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.ticks.x = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.ticks.y = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.title.x = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(axis.title.y = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(title = element_text(size = 14, face = "bold"))
      }
      output$title <- renderText(c("Selected feature subset"))
      output$view <- DT::renderDataTable(matrix(c(colnames(df_glm)[var+1],rep(" ",5 - length(colnames(df_glm)[var+1]) %% 5)),ncol = 5,byrow = T,
                                                dimnames = list(c(),
                                                                paste("Var.name",1:5))),
                                         options = list(lengthChange = FALSE))
      output$title2 <- renderText(c("Model Summary"))
      output$fitness <- renderTable(re_k, rownames = F)
      output$plot <- renderPlot(print(P), width = 600, height = 500)
      output$cindex <- renderTable(NULL, rownames = F)

    }
    if(input$family=="cox")
    {

      df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
      time <- scan(input$time,c(","))
      state <- scan(input$state,c(","))
      con <- scan(input$con_var,c(","))
      c_index <- c()

      df <- na.omit(df)
      if (input$smethod=="sequential")
      {
        re_b <- bess(x=as.matrix(cbind(df[,c(con)])),
                     y=cbind(df[,time],df[,state]), method = "sequential",family = "cox")
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_b$beta[,which.min(re_b$EBIC)]!=0]
      }
      else if(input$smethod=="gsection")
      {
        re_bg <- bess(x=as.matrix(cbind(df[,c(con)])),
                      y=cbind(df[,time],df[,state]), method = "gsection",family = "cox")
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_bg$beta[,ncol(re_bg$beta)] !=0 ]
      }
      else
      {
        re_bg <- bess.one(x=as.matrix(cbind(df[,c(con)])),
                          y=cbind(df[,time],df[,state]), family = "cox",s=input$size)
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_bg$beta !=0 ]
      }


      df_cox<-data.frame(cbind(df[,c(time,state,con)]))

      fullformula <- as.formula(paste0("Surv(",paste0(colnames(df)[time], ",",colnames(df)[state]),')~',paste(colnames(df_cox)[var+2], collapse=" + ")))

      jieguo <- coxph(fullformula,data = df_cox,na.action = na.omit, method = "breslow" )




      jieguo1 <- summary(jieguo)
      jieguo2 <- jieguo1$coefficients
      jieguo2[,4] <- jieguo2[,4]**2
      jieguo3 <- round(jieguo2,digits = 4)
      jieguo4 <- jieguo3[,-2]

      jieguo5 <- jieguo1$conf.int
      jieguo6 <- round(jieguo5,digits = 4)
      jieguo7 <- jieguo6[,-2]

      jieguo41 <- matrix(jieguo4,ncol=4)
      jieguo71 <- matrix(jieguo7,ncol=3)

      colnames1 <- c("coef","se(coef)","Chi-Square","Pr(>|Chi-Square|)","HR","Lower","Upper")
      colnames2 <- c("Variable","coef","se(coef)","Chi-Square","Pr(>|Chi-Square|)","HR","Lower","Upper")

      if (nrow(jieguo41)==1) {
        k1 <- cbind(jieguo41,jieguo71)
        k1 <- tp(k1)
        d <- rownames(jieguo4)
        k <- cbind(d,k1)
        colnames(k) <- colnames1
      }

      if (nrow(jieguo41)!=1) {
        k1 <- cbind(jieguo41,jieguo71)
        k1 <- tp(k1)
        d <- rownames(jieguo4)
        k <- cbind(d,k1)
        colnames(k) <- colnames2

      }




      if (input$method=="Bootstrap")
      {
        df_cox <- na.omit(df_cox)
        df_t <- Bootstrap(df_cox,input$seed)
        df_train <- df_t[[1]]
        df_test <- df_t[[2]]
        fit_b <- coxph(fullformula,data = df_train,na.action = na.omit)
        a <- predict(fit_b,df_test)
        a <- data.frame(df_test,a)
        c_index <- rcorr.cens(a$a,Surv(df_test[,1],df_test[,2]))
        c_index <- CstatisticCI(c_index)
        c_index <- data.frame(c_index)
      }

      if (input$method=="Train/Test")
      {
        df_cox <- na.omit(df_cox)
        df_train <- df_cox
        df_test <- read.csv(input$res_df$datapath, header = TRUE, sep = ",")
        P <- c()
        df_test <- data.frame(df_test[,c(y,con)])

        fit_b <- coxph(fullformula,data = df_train,na.action = na.omit)
        a <- predict(fit_b,df_test)
        a <- data.frame(df_test,a)
        c_index <- rcorr.cens(a$a,Surv(df_test[,1],df_test[,2]))
        c_index <- CstatisticCI(c_index)
        c_index <- data.frame(c_index)
      }


      if (input$method=="Cross-validation")
      {
        set.seed(input$seed2)
        df_cox <- na.omit(df_cox)
        df_na <- df_cox
        df_na$fold <- sample(rep(1:input$kfold,ceiling(nrow(df_na)/input$kfold))[1:nrow(df_na)])
        prob <- matrix(NA,nrow(df_na),1)
        prob <- data.frame(prob)
        rf_p <- list()
        df_t <- list()
        for ( i in 1:input$kfold) {
          df_t[[i]] <- df_na[df_na$fold !=i,]
          rf_p[[i]] <-  coxph(fullformula,data = df_t[[i]],na.action = na.omit)
          prob[df_na$fold ==i,1] <- predict(rf_p[[i]],df_na[df_na$fold ==i,])
        }
        a <- data.frame(df_na,prob)
        c_index <- rcorr.cens(a$prob,Surv(df_na[,1],df_na[,2]))
        c_index <- CstatisticCI(c_index)
        c_index <- data.frame(c_index)
      }



      output$title <- renderText(c("Selected feature subset"))
      output$view <- DT::renderDataTable(matrix(c(colnames(df_cox)[var+1],rep(" ",5 - length(colnames(df_cox)[var+1]) %% 5)),ncol = 5,byrow = T,
                                                dimnames = list(c(),
                                                                paste("Var.name",1:5))),
                                         options = list(lengthChange = FALSE))
      output$title2 <- renderText(c("Model Summary"))
      output$fitness <- renderTable(k, rownames = F)
      output$cindex <- renderTable(print(c_index), rownames = F)
      output$plot <- renderPlot(NULL, width = 600, height = 1)


    }
    if(input$family=="gaussian")
    {
      y <- scan(input$ylm, c(","))
      con <- scan(input$con_var, c(","))

      df <- na.omit(df)
      if (input$smethod=="sequential")
      {
        re_b <- bess(x=as.matrix(cbind(df[,c(con)])),
                     y=df[,c(y)], method = "sequential",family = "gaussian")
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_b$beta[,which.min(re_b$EBIC)]!=0]
      }
      else if (input$smethod=="sequential")
      {
        re_bg <- bess(x=as.matrix(cbind(df[,c(con)])),
                      y=df[,c(y)], method = "gsection",family = "gaussian")
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_bg$beta[,ncol(re_bg$beta)] !=0 ]
      }
      else
      {
        re_bg <- bess.one(x=as.matrix(cbind(df[,c(con)])),
                          y=df[,c(y)], family = "gaussian",s=input$size)
        var <- c(1:ncol(as.matrix(cbind(df[,c(con)]))))[re_bg$beta !=0 ]
      }



      df_glm <- data.frame(cbind(df[,c(y,con)]))
      df_glm[, 1] <- as.numeric(as.factor(df_glm[, 1])) - 1
      df_glm <- na.omit(df_glm)
      fullformula <- as.formula(paste0(colnames(df[y]), "~", paste(colnames(df_glm)[var+1], collapse = " + ")))
      jieguo <- glm(fullformula, family = gaussian, data = df_glm)


      jieguo0 <- lm.beta(jieguo)
      jieguo1 <- summary(jieguo0)
      jieguo2 <- jieguo1$coefficients
      jieguo3 <- tp(jieguo2)

      d <- rownames(jieguo3)
      k <- cbind(d, jieguo3)
      colnames(k)[1] <- "Variable"

      if (input$method == "Bootstrap") {
        df_glm <- na.omit(df_glm)
        df_t <- Bootstrap(df_glm, input$seed)
        df_train <- df_t[[1]]
        df_test <- df_t[[2]]
        fit_b <- rms::Glm(fullformula, family = gaussian, data = df_train)
        a <- predict.glm(fit_b, df_test, family = gaussian)
        a <- data.frame(df_test, a)
        q2 <- 1 - sum((a[, 1] - a$a)**2) / sum((a[, 1] - mean(a[, 1]))**2)
        re <- data.frame(ob = 1:length(na.omit((a[, 1] - a$a))), residual = na.omit((a[, 1] - a$a)))
        lab <- paste0("Q^2=", round(q2, 4))

        re_p <- ggplot(data = re) +
          geom_point(aes(x = ob, y = residual), color = "red", alpha = 0.5) +
          geom_hline(aes(yintercept = 0)) +
          theme_classic() +
          labs(title = lab) +
          labs(title = lab, x = "Observation", y = "Residual") +
          theme(axis.line.x = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.line.y = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.text.x = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.text.y = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.ticks.x = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.ticks.y = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.title.x = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(axis.title.y = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

      }

      if (input$method == "Train/Test") {
        df_glm <- na.omit(df_glm)
        df_train <- df_glm
        df_test <- read.csv(input$res_df$datapath, header = TRUE, sep = ",")

        df_test <- na.omit(df_test)
        set.seed(1)
        df_test <- data.frame(cbind(df[,c(con)]))

        fit_b <- rms::Glm(fullformula, family = gaussian, data = df_train)
        a <- predict.glm(fit_b, df_test, family = gaussian)
        a <- data.frame(df_test, a)
        q2 <- 1 - sum((a[, 1] - a$a)**2) / sum((a[, 1] - mean(a[, 1]))**2)

        re <- data.frame(ob = 1:length(na.omit((a[, 1] - a$a))), residual = na.omit((a[, 1] - a$a)))
        lab <- paste0("Q^2=", round(q2, 4))

        re_p <- ggplot(data = re) +
          geom_point(aes(x = ob, y = residual), color = "red", alpha = 0.5) +
          geom_hline(aes(yintercept = 0)) +
          theme_classic() +
          labs(title = lab) +
          labs(title = lab, x = "Observation", y = "Residual") +
          theme(axis.line.x = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.line.y = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.text.x = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.text.y = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.ticks.x = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.ticks.y = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.title.x = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(axis.title.y = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

      }


      if (input$method == "Cross-validation") {
        set.seed(input$seed2)
        df_glm <- na.omit(df_glm)
        df_na <- df_glm
        df_na$fold <- sample(rep(1:input$kfold, ceiling(nrow(df_na) / input$kfold))[1:nrow(df_na)])
        prob <- matrix(NA, nrow(df_na), 1)
        prob <- data.frame(prob)
        rf_p <- list()
        df_t <- list()
        for (i in 1:input$kfold) {
          df_t[[i]] <- df_na[df_na$fold != i, ]
          rf_p[[i]] <- Glm(fullformula, family = gaussian, data = df_t[[i]])
          prob[df_na$fold == i, 1] <- predict.glm(rf_p[[i]], df_na[df_na$fold == i, ], family = gaussian)
        }
        a <- data.frame(df_na, prob)
        q2 <- 1 - sum((a[, 1] - a$prob)**2) / sum((a[, 1] - mean(a[, 1]))**2)

        re <- data.frame(ob = 1:length(na.omit((a[, 1] - a$prob))), residual = na.omit((a[, 1] - a$prob)))
        lab <- paste0("Q^2=", sprintf("%0.4f", q2))

        re_p <- ggplot(data = re) +
          geom_point(aes(x = ob, y = residual), color = "red", alpha = 0.5) +
          geom_hline(aes(yintercept = 0)) +
          theme_classic() +
          labs(title = lab, x = "Observation", y = "Residual") +
          theme(axis.line.x = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.line.y = element_line(linetype = 1, color = "black", size = 1)) +
          theme(axis.text.x = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.text.y = element_text(face = "bold", color = "black", size = 14)) +
          theme(axis.ticks.x = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.ticks.y = element_line(color = "black", size = 3, lineend = 1)) +
          theme(axis.title.x = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(axis.title.y = element_text(vjust = 2, size = 20, face = "bold")) +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

      }




      output$title <- renderText(c("Selected feature subset"))
      output$view <- DT::renderDataTable(matrix(c(colnames(df_glm)[var+1],rep(" ",5 - length(colnames(df_glm)[var+1]) %% 5)),ncol = 5,byrow = T,
                                                dimnames = list(c(),
                                                                paste("Var.name",1:5))),
                                         options = list(lengthChange = FALSE))
      output$title2 <- renderText(c("Model Summary"))
      output$plot <- renderPlot(re_p, width = 600, height = 500)
      output$fitness <- renderTable(k, rownames = F)
      output$cindex <- renderTable(NULL, rownames = F)

    }


  })
}
