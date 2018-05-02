# Greek-Electric-Load-Forecasting-IPTO
<div><p align = "center"><img src = "https://pk-shinies.shinyapps.io/ipto-ml/_w_703ba8b8/file33591c7064_files/img/logo_horizontal.png" /></p></div>

<ul>
<img src = "https://img.shields.io/badge/license-MIT-blue.svg" />
<img src="https://img.shields.io/badge/status%3A-version%202%20%2F%20experimenting%20%2F%20studying%20%2F%20improving-green.svg" />
<img src="https://img.shields.io/badge/current%20active%20branch-development--4-lightgrey.svg" />
</ul>

This repo contains the code for my postgraduate thesis dealing with Short-term Load Forecasting, predicting the electric load demand per hour in Greece, developed in R, RStudio, R-markdown and R-Shiny using daily load datasets provided by the Greek Independent Power Transmission Operator (I.P.T.O.) - (AΔΜHΕ in greek) http://www.admie.gr/nc/en/home/ <br>
The documentation of my thesis can be found at Aristotle's University repository of Theses: http://ikee.lib.auth.gr/record/294603/?ln=en, please excuse me for errors inside the documentation and if you find any, please inform me :)

<h2>libraries - Dependencies</h2>
<ul>

  <h3>Data Preprocessing Libraries</h3>
  <li><strong>xlsx</strong> package: install.packages('xlsx')</li>
  <li><strong>JSONLite</strong>: install.packages("jsonlite")</li>
  <li><strong>lubridate</strong>: install.packages('lubridate')</li>
  <li><strong>Tibble</strong>: install.packages("tibble")</li>
  <li><strong>Feature Selection</strong>, library: install.package("Boruta")</li>
  
  <h3>Machine Learning Libraries</h3>
  <li><strong>SVM</strong>, library: install.package("e1071")</li>
  <li><strong>Random Forest</strong>, library: install.package("RandomForest")</li>
  <li><strong>Neural Network</strong>, library: install.package("RSNNS")</li>
  <li><strong>K' Nearest Neighbors</strong>, library: install.package("FNN")</li>
  <li><strong>Rule Based Models</strong>, library: install.package("Cubist")</li>
  <li><strong>xgboost models</strong>, library: install.package("xgboost")</li>
  
  <h3>Plots - Visualizations</h3>
  <li><strong>ggplot2</strong>, library: install.package("ggplot2")</li>
  <li><strong>plotly</strong>, library: install.package("plotly")</li>
  <li><strong>Rmarkdown</strong>, library: install.package("Rmarkdown")</li>
  <li><strong>R-Shiny</strong>, library: install.package("shiny")</li>
  <li><strong>flexdashboard</strong>, library: install.package("flexdashboard")</li>
</ul>
