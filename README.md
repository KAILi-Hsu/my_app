# my_app

Record my CODE.

這是一個使用 Shiny 搭配 shinydashboard 製作的應用程式，包含以下五個主要功能分頁：

1.胸部X光左心室功能障礙預測: 上傳一張胸部X光影像，模型將預測該個體是否有左心室功能障礙。

2.識別草帽一夥人！: 上傳一張人物照片，模型將辨識此人是否為草帽海賊團成員，並指出是誰。（僅包含九位角色：魯夫、索隆、香吉士、騙人布、娜美、喬巴、羅賓、弗蘭奇、布魯克）

3.二元分類器比較: 上傳資料檔案，選擇目標變數與預測變數，系統將比較四種二元分類模型的 AUC 表現。(四種二元分類模型為: 線性判別分析（Linear Discriminant Analysis, LDA）,羅吉斯迴歸（Logistic Regression）, 單純貝氏分類器（Naïve Bayes Classification）, K-近鄰演算法（K-Nearest Neighbors, KNN）

4.Panel Function 練習區: 練習使用 box 與 tabBox 元件，包含以下常用控制項目：單選、多選、滑桿（slider）、時間選擇器、檔案上傳、資料表顯示與下載等。

5.使用者回饋（The Feedback）: 統計並顯示使用者在前兩個預測功能頁面中所回報的預測結果與使用經驗。



This is an application built with Shiny and shinydashboard, featuring five main functional tabs:

1.Chest X-ray Left Ventricular Dysfunction Prediction: Upload a chest X-ray image, and the model will predict whether the individual has left ventricular dysfunction.

2.Identify the Straw Hat Crew!: Upload a photo, and the model will identify if the person belongs to the Straw Hat Pirates, and if so, which member it is.
(Limited to 9 characters: Luffy, Zoro, Sanji, Usopp, Nami, Chopper, Robin, Franky, and Brook)

3.Binary Classifier Comparison: Upload a dataset, select the target variable and predictor variables, and the system will compare the AUC performance of four binary classification models. (model include Linear Discriminant Analysis (LDA), Logistic Regression, Naïve Bayes Classification, K-Nearest Neighbors (KNN)

4.Panel Function Practice Area: Practice using box and tabBox components, featuring commonly used control elements such as: radio buttons, checkboxes, sliders, date/time selectors, file upload, data table display, and file downloads.

5.User Feedback: Collects and displays user-reported prediction results and experiences from the first two prediction tabs.


