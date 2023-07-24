# my_app

Record my CODE.

一個使用shiny並且使用shinydashboard編寫的APP。
內容主要有四頁：
1. 胸部X光左心室功能障礙預測：上傳一張胸部X光片，看模型是否判斷人是否患有左心功能不全。
2. 識別草帽一夥人!：上傳照片，看看模型認出這張照片是草帽一夥中的誰？(僅包含魯夫、索隆、香吉士、騙人布、娜美、喬巴、羅賓、弗蘭奇、布魯克，9人)
3. 二元分類器比較：上傳文件，選擇目標變量和預測變量，比較四個模型的AUC。(四個模型分別是:線性區別分析(Linear Discriminant Analysis，LDA）、羅吉斯迴歸（Logistic regression）、單純貝氏分類器（Naïve Bayes Classification）與K-近鄰演算法（K-nearest neighbors, KNN））
4. Panel Function：練習使用box和tabBox將一些常用的ControlPanel，包括單選、多選、slider、時間選擇、上傳文件、table呈現、下載檔案等。

An APP written using shiny and specifically using shinydashboard.
The content mainly has four pages:
1. Chest X-ray prediction of left ventricular dysfunction: upload a chest X-ray and see if the model judges whether the person has left ventricular dysfunction.
2. Identify the Straw Hat Crew: Upload a photo and see who the model recognizes that this picture is whom in the Straw Hat Crew? (Only including Luffy, Zoro, Sanji, Boo, Nami, Chopper, Robin, Franky, Brook, 9 people)
3. Binary classifier comparison: upload the file, and select the target and predictor variables, and compare the AUC of the four models. (The four models are: Linear Discriminant Analysis (LDA), Logistic regression, Naïve Bayes Classification and K-nearest neighbors (KNN))
4. Panel Function: Practice using box and tabBox to put some commonly used ControlPanel functions in shiny, including single selection, multi-selection, slider, time selection, uploading files, table presentation, and downloading files.


