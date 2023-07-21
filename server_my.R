library(shiny)
library(jpeg)
library(imager)
library(grid)
library(gridExtra)
library(OpenImageR)
library(mxnet)

Show_img <- function (img, plot_xlim = c(0.06, 0.94), plot_ylim = c(0.94, 0.06)) {
  
  par(mar = rep(0, 4))
  plot(NA, xlim = plot_xlim, ylim = plot_ylim, xaxt = "n", yaxt = "n", bty = "n")
  img = (img - min(img))/(max(img) - min(img))
  grid.raster(img)
  
}

shinyServer(function(input, output) {
  
  IMAGE = reactive({
    if (is.null(input$files)) {
      
      img <- readImage("U0084.jpeg")
      return(img)
      
    } else {
      
      img <- readImage(input$files$datapath)
      return(img) 
    }
  })
  
  output$plot <- renderPlot({
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
  
  IMAGE2 = reactive({
    if (is.null(input$files2)) {
      
      img <- readImage("0000.jpg")
      return(img)
      
    } else {
      
      img <- readImage(input$files2$datapath)
      return(img) 
    }
  })
  
  output$plot2 <- renderPlot({
    img = IMAGE2()
    if (is.null(input$files)) {
      
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
      
      my_model2 = mx.model.load("test2_ONE(3)", iteration = 100)
      pred_test <-  predict(model = my_model2, X = test_img_array, ctx = mx.gpu(5))
      
      #score <- round(pred_test[2,],4)
      
      x <- which.max(pred_test[,1])
      
      if( x == 1 ){
        HTML(paste0('這應該是魯夫吧！'))
      } else if ( x == 2 ) {
        HTML(paste0('這應該是索隆吧！'))
      } else if ( x == 3 ) {
        HTML(paste0('這應該是香吉士吧！'))
      } else if ( x == 4 ) {
        HTML(paste0('這應該是騙人布吧！'))
      } else if ( x == 5 ) {
        HTML(paste0('這應該是娜美吧！'))
      } else if ( x == 6 ) {
        HTML(paste0('這應該是喬巴吧！'))
      } else if ( x == 7 ) {
        HTML(paste0('這應該是羅賓吧！'))
      } else if ( x == 8 ) {
        HTML(paste0('這應該是布魯克吧！'))
      } else if ( x == 9 ) {
        HTML(paste0('這應該弗蘭奇賓吧！'))
      } else {
        HTML(paste0('這應該不是草帽一夥人?嗎?吧'))
      }
      
    }
  })
  
  
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
      colnames <- colnames(dat)
      selectInput("D", h4("Choose a predictor variable1(x):"), choices = colnames[which(colnames!= input$C)])
    }
  })
  
  output$choose_columns7 <- renderUI({
    dat = DATA4()
    
    if (is.null(dat)|is.null(input$C) |is.null(input$D)) {return()} else {
      colnames <- colnames(dat)
      selectInput("E", h4("Choose a predictor variable2(x):"), choices = colnames[!(colnames %in% c(input$C, input$D))])
    }
  })
  
  output$out4 <- renderPrint(input$method)
  
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
  
  output$roc_test1 <- renderPlot({
    data_list <- DATA5()
    train_X = data_list[[1]]
    valid_X = data_list[[2]]
    test_X = data_list[[3]]
    train_Y = data_list[[4]]
    valid_Y = data_list[[5]]
    test_Y = data_list[[6]]
    
    
    if (is.null(train_X)|is.null(valid_X) |is.null(test_X)|is.null(train_Y)|is.null(valid_Y)|is.null(test_Y)) {return()} else {
      
      if(input$method == 1){ 
        
        #linear Discriminant
        print(train_Y)
        glm_fit <- glm(train_Y ~ ., data = train_X, family = 'binomial')
        
        valid_pred <- predict(glm_fit, valid_X)
        test_pred <- predict(glm_fit, test_X)
        
        
        
        roc_valid <- roc(valid_Y ~ valid_pred)
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        tab_test <- table(test_pred >= best_cut, test_Y)
        sens <- tab_test[2,2] / sum(tab_test[,2])
        spec <- tab_test[1,1] / sum(tab_test[,1])
        
        roc_test <- roc(test_Y ~ test_pred)
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity" )
        
        points(spec, sens, pch = 19)
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')
        
        output$summary3 <- renderPrint({
          score <- round(roc_test$auc ,4)
          HTML(paste0(' 在線性區別分析(Linear Discriminant Analysis，LDA）模型下，'),'\n',
               '使用', input$C, '和', input$D, '來預測', input$E,'的AUC分數是：', score)
          
        })
        
      }
      
      if(input$method == 2){ 
        
        #logistic regression
        
        glm_fit <- glm(train_Y ~ ., data = train_X, family = 'binomial')
        
        valid_pred <- predict(glm_fit, valid_X)
        test_pred <- predict(glm_fit, test_X)
        
        library(pROC)
        
        roc_valid <- roc(valid_Y ~ valid_pred)
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        tab_test <- table(test_pred >= best_cut, test_Y)
        sens <- tab_test[2,2] / sum(tab_test[,2])
        spec <- tab_test[1,1] / sum(tab_test[,1])
        
        roc_test <- roc(test_Y ~ test_pred)
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity" )
        
        points(spec, sens, pch = 19)
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')
        
        output$summary3 <- renderPrint({
          score <- round(roc_test$auc ,4)
          HTML(paste0(' 在羅吉斯迴歸（Logistic regression）模型下，'),'\n',
               '使用', input$C, '和', input$D, '來預測', input$E,'的AUC分數是：', score)
        })
        
      }
      
      if(input$method == 3){ 
        
        #Naive Bayes Classification
        
        fit_Bayes <- naiveBayes(train_Y ~ ., data = train_X, family = 'binomial')
        
        valid_pred <- predict(fit_Bayes, valid_X, type = 'raw')[,2]
        test_pred <- predict(fit_Bayes, test_X, type = 'raw')[,2]
        
        library(pROC)
        
        roc_valid <- roc(valid_Y ~ valid_pred)
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        tab_test <- table(test_pred >= best_cut, test_Y)
        sens <- tab_test[2,2] / sum(tab_test[,2])
        spec <- tab_test[1,1] / sum(tab_test[,1])
        
        roc_test <- roc(test_Y ~ test_pred)
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity" )
        
        points(spec, sens, pch = 19)
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')
        
        output$summary3 <- renderPrint({
          score <- round(roc_test$auc ,4)
          HTML(paste0(' 在單純貝氏分類器（Naïve Bayes Classification）模型下，'),'\n',
               '使用', input$C, '和', input$D, '來預測', input$E,'的AUC分數是：', score)
        })
        
      }
      
      if(input$method == 4){ 
        
        #k-nearest neighbor algorithm
        
        valid_pred <- knn(train = scale(train_X), test = scale(valid_X), cl = train_Y, k = 20, prob = TRUE)
        test_pred <- knn(train = scale(train_X), test = scale(test_X), cl = train_Y, k = 20, prob = TRUE)
        
        library(pROC)
        
        roc_valid <- roc(valid_Y ~ attr(valid_pred, 'prob'), direction = '>')
        best_pos <- which.max(roc_valid$sensitivities + roc_valid$specificities)
        best_cut <- roc_valid$thresholds[best_pos]
        
        tab_test <- table(attr(test_pred, 'prob') >= best_cut, test_Y)
        sens <- tab_test[2,2] / sum(tab_test[,2])
        spec <- tab_test[1,1] / sum(tab_test[,1])
        
        roc_test <- roc(test_Y ~ attr(test_pred, 'prob'), direction = '>')
        plot(roc_test, xlab = "1 - Specificity", ylab = "Sensitivity" )
        
        text(0.5, 0.5, paste0('AUC(valid) = ', formatC(roc_valid$auc, digits = 3, format = 'f'),
                              '\nSens = ', formatC(sens, digits = 3, format = 'f'),
                              '\nSpec = ', formatC(spec, digits = 3, format = 'f'),
                              '\nAUC(test) = ', formatC(roc_test$auc, digits = 3, format = 'f')), col = 'red')
        
        output$summary3 <- renderPrint({
          score <- round(roc_test$auc ,4)
          HTML(paste0(' 在K-近鄰演算法（K-nearest neighbors, KNN）模型下，'),'\n',
               '使用', input$C, '和', input$D, '來預測', input$E,'的AUC分數是：', score)
        })
        
      }
    } 
    
  })
  
  # output$summary3 <- renderPrint({
  #   
  #   HTML(paste0('在模型下，使用 ', input$C, ' 和 ', input$D, '來預測 ', input$E,'的AUC分數是',roc_test$auc))
  #   
  # })
  
})
