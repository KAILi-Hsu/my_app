library(shiny)
library(shinydashboard)

library(jpeg)
library(imager)
library(grid)
library(gridExtra)
library(OpenImageR)

library(DT)
library(class)
library(e1071)
library(pROC)

library(mxnet)

#讀取照片的function

Show_img <- function (img, plot_xlim = c(0.06, 0.94), plot_ylim = c(0.94, 0.06)) {
  
  par(mar = rep(0, 4))
  plot(NA, xlim = plot_xlim, ylim = plot_ylim, xaxt = "n", yaxt = "n", bty = "n")
  img = (img - min(img))/(max(img) - min(img))
  grid.raster(img)
  
}


shinyServer(function(input, output, session) {

  #[分頁1]胸部X光左心室功能障礙預測
  
  IMAGE = reactive({
    if (is.null(input$files)) {
      
      img <- readImage("U0084.jpeg")
      return(img)
      
    } else {
      
      img <- readImage(input$files$datapath)
      return(img) 
    }
  })

  output$plot1 <- renderUI({
    img = IMAGE()
    new_height <- 400
    img_width <- dim(img)[2]
    img_height <- dim(img)[1]
    new_width <- round(img_width * (new_height / img_height))
    w <- paste(new_width,"px")
    if (is.null(input$files)) {
      
      plotOutput("plot1_2", width = paste0(new_width,"px"))
      
    } else { 
      plotOutput("plot1_2", width = paste0(new_width,"px"))
    }
  })
  
  output$plot1_2 <- renderPlot({
    img = IMAGE()
    if (is.null(input$files)) {
      
      Show_img(img)
      
    } else {
      
      Show_img(img)
    }
  })
  
  output$summary <- renderPrint({
    img = IMAGE()
    if (is.null(input$files)) {
      
      HTML(paste0('此人很有可能有左心室功能障礙(LVD)，預測分數為: 0.9925 (分數越靠近1越有可能有LVD)【此為範例資料】'))
      
    } else {
      
      img <- readImage(input$files$datapath)
      
      target_size <- c(256, 256)
      targer_fold <- dim(img)[1] / dim(img)[2]
      if (targer_fold > 1) {target_size[1] <- target_size[1] * targer_fold} else {target_size[2] <- target_size[2] / targer_fold}
      resize_img <- resizeImage(img, width = target_size[1], height = target_size[2], method = 'bilinear')
      
      test_img_array <-  array(0, dim = c(256, 256, 3, 1))
      
      row_select <- floor((dim(resize_img)[1] - 256) / 2) + 1:256
      col_select <- floor((dim(resize_img)[2] - 256) / 2) + 1:256
      test_img_array[,,,1] <- resize_img[row_select,col_select,]
      
      my_model = mx.model.load("test_softmax2", iteration = 4)
      pred_test <-  predict(model = my_model, X = test_img_array, ctx = mx.gpu(3))
      
      score <- round(pred_test[1,],4)
      
      if( score > 0.85 ){
        HTML(paste0('此人很有可能有左心室功能障礙(LVD)，預測分數為: ', score, ' (分數越靠近1越有可能有LVD)'))
      } else if (score > 0.68) {
        HTML(paste0('此人有可能有左心室功能障礙(LVD)，預測分數為: ', score, ' (分數越靠近1越有可能有LVD)'))
      } else {
        HTML(paste0('此人較沒有左心室功能障礙(LVD)的可能，預測分數為: ', score, ' (分數越靠近1越有可能有LVD)'))
      }
      
    }
  })
  
  output$C_index <- renderPrint({
    
    HTML(paste0('目前訓練的模型C-index=0.8081'))
    
  })
  
  #[分頁2]識別草帽一夥人!
  
  IMAGE2 = reactive({
    if (is.null(input$files2)) {

    img <- readImage("0000.jpg")
    return(img)
      
    } else {
      
      img <- readImage(input$files2$datapath)
      return(img) 
    }
  })
  
  output$plot2 <- renderUI({
    img = IMAGE2()
    new_height <- 400
    img_width <- dim(img)[2]
    img_height <- dim(img)[1]
    new_width <- round(img_width * (new_height / img_height))
    w <- paste(new_width,"px")
    if (is.null(input$files)) {
      
      plotOutput("plot2_2", width = paste0(new_width,"px"))
      
    } else { 
      plotOutput("plot2_2", width = paste0(new_width,"px"))
    }
  })
  
  output$plot2_2 <- renderPlot({
    img = IMAGE2()
    if (is.null(input$files2)) {
      
      Show_img(img)
      
    } else { 
      
      Show_img(img)
    }
  })
  
  
  
  
  output$summary2 <- renderPrint({
    img = IMAGE2()
    if (is.null(input$files2)) {
      
      HTML(paste0('這是魯夫吧!【此為範例資料】'))
      
    } else {
      
      img <- readImage(input$files2$datapath)
      
      target_size <- c(224, 224)
      targer_fold <- dim(img)[1] / dim(img)[2]
      if (targer_fold > 1) {target_size[1] <- target_size[1] * targer_fold} else {target_size[2] <- target_size[2] / targer_fold}
      resize_img <- resizeImage(img, width = target_size[1], height = target_size[2], method = 'bilinear')
      
      test_img_array <-  array(0, dim = c(224, 224, 3, 1))
      
      row_select <- floor((dim(resize_img)[1] - 224) / 2) + 1:224
      col_select <- floor((dim(resize_img)[2] - 224) / 2) + 1:224
      test_img_array[,,,1] <- resize_img[row_select,col_select,]
      
      my_model2 = mx.model.load("test50_ONE_2", iteration = 0)
      pred_test <-  predict(model = my_model2, X = test_img_array, ctx = mx.gpu(5))
      
      #score <- round(pred_test[2,],4)
      
      x <- which.max(pred_test[,1])
      #score <- round(pred_test[,1], 4)
      
      if( x == 1 ){
        
        score <- round(pred_test[1,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...魯夫?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是魯夫吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是魯夫吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是魯夫吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 2 ) {
        
        score <- round(pred_test[2,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...索隆?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是索隆吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是索隆吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是索隆吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 3 ) {
        
        score <- round(pred_test[3,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...香吉士?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是香吉士吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是香吉士吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是香吉士吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 4 ) {
        
        score <- round(pred_test[4,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...騙人布?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是騙人布吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是騙人布吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是騙人布吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 5 ) {
        
        score <- round(pred_test[5,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...娜美?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是娜美吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是娜美吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是娜美吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 6 ) {
        
        score <- round(pred_test[6,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...喬巴?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是喬巴吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是喬巴吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是喬巴吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 7 ) {
        
        score <- round(pred_test[7,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...羅賓?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是羅賓吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是羅賓吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是羅賓吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 8 ) {
        
        score <- round(pred_test[8,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...弗蘭奇?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是弗蘭奇吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是弗蘭奇吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是弗蘭奇吧！ 我有', score,'%確定~'))
        }
        
      } else if ( x == 9 ) {
        
        score <- round(pred_test[8,1], 2)*100
        if( score < 51 ){
          HTML(paste0('他/她是...布魯克?我不確定(＞x＜)'))
        }else if ( score < 80 ) {
          HTML(paste0('這是布魯克吧！？我只有', score,'%確定'))
        }else if ( score == 100 ) {
          HTML(paste0('這是布魯克吧！我百分之百確定!٩(✿∂‿∂✿)۶'))
        }else{
          HTML(paste0('這是布魯克吧！ 我有', score,'%確定~'))
        }
        
      } else {
        HTML(paste0('這應該不是草帽一夥人?嗎?吧'))
      }
      
      
      
    }
  })
  
  #[分頁3]分類器比較

  DATA4 <- reactive({
    if (is.null(input$files3)) {return()} else {
      dat <- read.csv(input$files3$datapath, header = TRUE, fileEncoding = 'CP950', stringsAsFactors = FALSE, na.strings = "")
      return(dat)
    }
  })
  
  output$choose_columns5 <- renderUI({
    dat = DATA4()
    if (is.null(dat)) {return()} else {
      target_cols <- sapply(dat, function(col) all(col %in% c(0, 1, NA), na.rm = TRUE))
      colnames<- colnames(dat[target_cols])
      selectInput("C", h4("Choose a target variable(category variable only '1' & '0')(y):"), choices = colnames)
    }
  })
  
  output$choose_columns6 <- renderUI({
    dat = DATA4()
    if (is.null(dat)|is.null(input$C)) {return()} else {
      #target_cols <- sapply(dat, function(col) length(levels(as.factor(col))) > 20 && is.numeric(col))
      target_cols <- sapply(dat, function(col) is.numeric(col))
      colnames <- colnames(dat[target_cols])
      selectInput("D", h4("Choose a predictor variable1(x):"), choices = colnames[which(colnames!= input$C)])
    }
  })
  
  output$choose_columns7 <- renderUI({
    dat = DATA4()
    
    if (is.null(dat)|is.null(input$C) |is.null(input$D)) {return()} else {
      #target_cols <- sapply(dat, function(col) length(levels(as.factor(col))) > 20 && is.numeric(col))
      target_cols <- sapply(dat, function(col) is.numeric(col))
      colnames <- colnames(dat[target_cols])
      selectInput("E", h4("Choose a predictor variable2(x):"), choices = colnames[!(colnames %in% c(input$C, input$D))])
    }
  })
  
  DATA5 <- reactive({
    dat = DATA4()
    if (is.null(input$files3)) {return()} else {
      
      subdat <- dat[!(dat[,input$C] %in% NA) & !(dat[,input$D] %in% NA) & !(dat[,input$E] %in% NA),]
      subdat[,c("C", "D", "E")] <- subdat[,c(input$C, input$D, input$E)]
      print(subdat)
      X <- subdat[,c("D", "E")]
      Y <- subdat[,"C"]
      
      set.seed(0)
      all_idx <- 1:nrow(subdat)
      
      train_idx <- sample(all_idx, nrow(subdat) * 0.6)
      valid_idx <- sample(all_idx[!all_idx %in% train_idx], nrow(subdat) * 0.2)
      test_idx <- all_idx[!all_idx %in% c(train_idx, valid_idx)]
      
      train_X <- subdat[sample(all_idx, nrow(subdat) * 0.6),c("D", "E")]
      
      train_X <- X[train_idx,]
      valid_X <- X[valid_idx,]
      test_X <- X[test_idx,]
      
      train_Y <- Y[train_idx]
      valid_Y <- Y[valid_idx]
      test_Y <- Y[test_idx]
      return(list(train_X, valid_X, test_X,train_Y, valid_Y, test_Y))
    }
  })
  
  
  IMAGE3 = reactive({
    if (is.null(input$files3)) {
      
      img <- readImage("0004.png")
      return(img)
      
    } 
  })
  
  
  output$roc_test1 <- renderPlot({
    img <- IMAGE3()
    data_list <- DATA5()
    train_X = data_list[[1]]
    valid_X = data_list[[2]]
    test_X = data_list[[3]]
    train_Y = data_list[[4]]
    valid_Y = data_list[[5]]
    test_Y = data_list[[6]]
    
    if (is.null(input$files3)) {
      
      Show_img(img)
      
    } else {
      
      if (is.null(train_X)|is.null(valid_X) |is.null(test_X)|is.null(train_Y)|is.null(valid_Y)|is.null(test_Y)) {return()} else {
        
        dat <- numeric(4)
        
        library(pROC)
        
        par(mfrow = c(2, 2))
        
        #linear Discriminant
        print(train_Y)
        glm_fit <- glm(train_Y ~ ., data = train_X, family = 'binomial')
        
        valid_pred <- predict(glm_fit, valid_X)
        test_pred <- predict(glm_fit, test_X)
        
        
        
        roc_valid <- roc(valid_Y ~ valid_pred)
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        test_pred_numeric <- as.numeric(as.character(test_pred))
        tab_test <- table(test_pred_numeric >= best_cut, test_Y)
        tab_test <- addmargins(tab_test)  
        sens <- tab_test[2, 2] / tab_test["Sum", 2]  
        spec <- tab_test[1, 1] / tab_test[1, "Sum"] 
        
        roc_test <- roc(test_Y ~ test_pred)
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity", main = "linear Discriminant")
        dat[1] <-round( roc_test$auc, 4)
        
        
        points(spec, sens, pch = 19)
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')
        
        
        
        #logistic regression
        
        glm_fit <- glm(train_Y ~ ., data = train_X, family = 'binomial')
        
        valid_pred <- predict(glm_fit, valid_X)
        test_pred <- predict(glm_fit, test_X)
        
        library(pROC)
        
        roc_valid2 <- roc(valid_Y ~ valid_pred)
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        test_pred_numeric <- as.numeric(as.character(test_pred))
        tab_test <- table(test_pred_numeric >= best_cut, test_Y)
        tab_test <- addmargins(tab_test)  
        sens <- tab_test[2, 2] / tab_test["Sum", 2]  
        spec <- tab_test[1, 1] / tab_test[1, "Sum"] 
        
        roc_test <- roc(test_Y ~ test_pred)
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity", main = "logistic regression")
        dat[2] <-round( roc_test$auc, 4)
        
        points(spec, sens, pch = 19)
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')
        
        
        #Naive Bayes Classification
        
        fit_Bayes <- naiveBayes(train_Y ~ ., data = train_X, family = 'binomial')
        
        valid_pred <- predict(fit_Bayes, valid_X, type = 'raw')[,2]
        test_pred <- predict(fit_Bayes, test_X, type = 'raw')[,2]
        
        library(pROC)
        
        roc_valid <- roc(valid_Y ~ valid_pred)
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        test_pred_numeric <- as.numeric(as.character(test_pred))
        tab_test <- table(test_pred_numeric >= best_cut, test_Y)
        tab_test <- addmargins(tab_test)  
        sens <- tab_test[2, 2] / tab_test["Sum", 2]  
        spec <- tab_test[1, 1] / tab_test[1, "Sum"] 
        
        roc_test <- roc(test_Y ~ test_pred)
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity", main = "Naive Bayes Classification")
        dat[3] <-round( roc_test$auc, 4)
        
        points(spec, sens, pch = 19)
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')
        
        
        
        #k-nearest neighbor algorithm
        
        valid_pred <- knn(train = scale(train_X), test = scale(valid_X), cl = train_Y, k = 20, prob = TRUE)
        test_pred <- knn(train = scale(train_X), test = scale(test_X), cl = train_Y, k = 20, prob = TRUE)
        
        library(pROC)
        
        roc_valid <- roc(valid_Y ~ attr(valid_pred, 'prob'), direction = '>')
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        test_pred_numeric <- as.numeric(as.character(test_pred))
        tab_test <- table(test_pred_numeric >= best_cut, test_Y)
        tab_test <- addmargins(tab_test)  
        sens <- tab_test[2, 2] / tab_test["Sum", 2]  
        spec <- tab_test[1, 1] / tab_test[1, "Sum"] 
        
        roc_test <- roc(test_Y ~ attr(test_pred, 'prob'), direction = '>')
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity", main = "k-nearest neighbor algorithm")
        dat[4] <-round( roc_test$auc, 4)
        
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')

        
        output$summary3 <- renderPrint({
          
          num <- which.max(dat)
          score <- dat[which.max(dat)]
          
          if(num == 1){
            HTML(paste0("線性區別分析(Linear Discriminant Analysis，LDA）模型下，有最大的AUC: ",score))
          } else if (num == 2){
            HTML(paste0("羅吉斯迴歸（Logistic regression）模型下，有最大的AUC: ",score))
          } else if (num == 3){
            HTML(paste0("單純貝氏分類器（Naïve Bayes Classification）模型下，有最大的AUC: ",score))
          } else {
            HTML(paste0("K-近鄰演算法（K-nearest neighbors, KNN）模型下，有最大的AUC: ",score))
          }
          
        })
      } 
    }
  })
  
  #[分頁4]Panel Function

  output$out1 <- renderPrint(input$in1)
  output$out2 <- renderPrint(input$in2)
  output$out3 <- renderPrint(input$in3)
  output$out4 <- renderPrint(input$in4)
  output$out5 <- renderPrint(input$in5)
  output$out6 <- renderPrint(input$in6)
  output$out7 <- renderPrint(HTML(input$in7))
  output$out8 <- renderPrint(input$in8)
  output$out9 <- renderPrint(HTML(input$in9))
  output$out10 <- renderPrint(HTML(input$in10))
  
  
  DATA6 <- reactive({
    if (is.null(input$files4)) {return()} else {
      dat <- read.csv(input$files4$datapath, header = TRUE, fileEncoding = 'CP950', stringsAsFactors = FALSE, na.strings = "")
      return(dat)
    }
  })
  
  output$choose_columns8 <- renderUI({
    dat = DATA6()
    if (is.null(dat)) {return()} else {
      colnames <- colnames(dat)
      checkboxGroupInput("choose_vars", "Columns in data to show:",
                         names(dat), selected = names(dat))
    }
  })
  
  output$mytable1 <- DT::renderDataTable({
    data <- DATA6()
    if (is.null(data)) {
      return(NULL)
    }
    DT::datatable(data[, input$choose_vars, drop = FALSE],options = list(
                  pageLength = 20, autoWidth = TRUE,
                  columnDefs = list(list( targets = 2, width = '600px')),
                  scrollX = TRUE
    ))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("selected_data_", format(Sys.time()), ".csv", sep = "")
    },
    content = function(file) {
      data_to_download <- DATA6()[, input$choose_vars, drop = FALSE]
      write.csv(data_to_download, file, row.names = FALSE)
  })
  
  output$feedback1 <- renderUI({
    if (!is.null(input$files)) {
      radioButtons("feedback1", label = h4("我有預測正確嗎? 幫我點一下按送出吧！"),
                   choices = list("有捏~好棒棒" = 1, "錯了欸" = 2, "我也不知道才問你啊!" = 3),
                   selected = 10, inline = TRUE)
    }
  })
  
  output$button1 <- renderUI({
    if (!is.null(input$files)) {
      actionButton("f1", label = "Submit")
    } 
  })
  
  output$f1_out <- renderUI({
    if (!is.null(input$files)) {
      htmlOutput("f1_out_show")
    } 
  })
  
  feedback_data <- reactiveValues(
    feedback = list(correct = 0, incorrect = 0, unsure = 0)
  )
  
  observe({
    if (file.exists("feedback_data.rds")) {
      feedback_data$feedback <- readRDS("feedback_data.rds")
    }
  })
  
  observeEvent(input$f1, {
    num <- HTML(input$f1)
    if (num == 1) {
      observeEvent(input$feedback1, {
        feedback <- input$feedback1
        if (!is.null(feedback)) {
          if (feedback == 1) {
            feedback_data$feedback$correct <- feedback_data$feedback$correct + 1
          } else if (feedback == 2) {
            feedback_data$feedback$incorrect <- feedback_data$feedback$incorrect + 1
          } else if (feedback == 3) {
            feedback_data$feedback$unsure <- feedback_data$feedback$unsure + 1
          }
          
          saveRDS(feedback_data$feedback, "feedback_data.rds")
        }
      })
    }
  })
  
  observeEvent(input$f1, {
    num <- HTML(input$f1)
    if (num > 1) {
      output$f1_out_show <- renderText({
        #paste("&nbsp;&nbsp;Warning: 你已經送出過了！謝謝你~ 再換一張圖片試試看吧！")
        paste('<span style="font-size: 14px;">&nbsp;&nbsp;&nbsp;Warning: 你已經送出過回應了！謝謝你~ 再換一張圖片試試看吧！</span>')
        #paste('<pre style="white-space: pre-wrap;">     你已經送出過回應了！謝謝你~ 再換一張圖片試試看吧！</pre>')
      })
    }
  })
  
  output$X_table <- renderTable({
    data.frame(
      "回饋狀態" = c("正確", "錯誤", "不知道"),
      "累計次數" = format(c(
        feedback_data$feedback$correct,
        feedback_data$feedback$incorrect,
        feedback_data$feedback$unsure
      ),
      nsmall = 0)
    )
  })
  
  output$feedback2 <- renderUI({
    if (!is.null(input$files2)) {
      radioButtons("feedback2", label = h4(tags$b("我有預測正確嗎? 幫我點一下按送出吧！")),
                   choices = list("有捏~好棒棒" = 4, "錯了欸" = 5, "我也不知道才問你啊!" = 6),
                   selected = 10, inline = TRUE)
    }
  })
  
  output$button2 <- renderUI({
    if (!is.null(input$files2)) {
      actionButton("f2", label = "Submit")
    } 
  })
  
  output$f2_out <- renderUI({
    if (!is.null(input$files2)) {
      htmlOutput("f2_out_show")
    } 
  })
  
  feedback_data2 <- reactiveValues(
    feedback = list(correct = 0, incorrect = 0, unsure = 0)
  )
  
  observe({
    if (file.exists("feedback_data2.rds")) {
      feedback_data2$feedback <- readRDS("feedback_data2.rds")
    }
  })
  
  observeEvent(input$f2, {
    num <- HTML(input$f2)
    if (num == 1) {
      observeEvent(input$feedback2, {
        feedback <- input$feedback2
        if (!is.null(feedback)) {
          if (feedback == 1) {
            feedback_data2$feedback$correct <- feedback_data$feedback$correct + 1
          } else if (feedback == 2) {
            feedback_data2$feedback$incorrect <- feedback_data$feedback$incorrect + 1
          } else if (feedback == 3) {
            feedback_data2$feedback$unsure <- feedback_data$feedback$unsure + 1
          }
          
          saveRDS(feedback_data2$feedback, "feedback_data2.rds")
        }
      })
    }
  })
  
  observeEvent(input$f2, {
    num <- HTML(input$f2)
    if (num > 1) {
      output$f2_out_show <- renderText({
        #paste("<b>&nbsp;&nbsp;Warning: 你已經送出過了！謝謝你~ 再換一張圖片試試看吧！<b>")
        paste('<span style="font-size: 14px;"><b>&nbsp;&nbsp;&nbsp;Warning: 你已經送出過回應了！謝謝你~ 再換一張圖片試試看吧！</b></span>')
        #paste('<pre style="white-space: pre-wrap;">     你已經送出過回應了！謝謝你~ 再換一張圖片試試看吧！</pre>')
      })
    }
  })

  output$ONE_table <- renderTable({
    data.frame(
      "回饋狀態" = c("正確", "錯誤", "不知道"),
      "累計次數" = format(c(
        feedback_data2$feedback$correct,
        feedback_data2$feedback$incorrect,
        feedback_data2$feedback$unsure
      ),
      nsmall = 0)
    )
  })
  
})
