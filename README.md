\# VECM Article



This repository contains the code and data cleaning for my collaboration on the article "The expansive wave of the fifth long cycle in the United States: empirical evidence from the rate of profit 1940-2023". The project employs a Vector Error Correction Model (VECM) to examine the dynamic relationships within the U.S. economy. It is structured for reproducibility and clarity.

---



\#  Repository structure



```

vecm-article/



├─ analysis/

│  └─ vecm\_article.R

│  └─ vecm\_article.Rmd  

├─ data/

│  └─ mydata.xlsx

│  └─ mydata.csv 

├─ R/                    

│  └─ \_packages.R

├─ renv/

|  └─ activate.R              

|  └─ settings.json

├─ scripts/                    

│  └─ 00\_fetch\_clean.R

├─  .RProfile

├─ .gitignore

├─ LICENSE.txt

├─ README.md

└─ renv.lock


## How to reproduce

1. Clone the repository  
2. Open in R (or RStudio)  
3. Run:

```r
install.packages("renv")
renv::restore()
rmarkdown::render("analysis/vecm_article.Rmd")



