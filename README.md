# Recommended iroc package installation
```{r}
source("https://raw.githubusercontent.com/liyistat/install_github_package/master/install_github.R");
install_github("liyistat/iroc")
library(iroc);
```

# Install iroc package by devtools
```{r}
install.packages(c("ROCR","pROC"));
devtools::install_github("liyistat/iroc");
library(iroc);
```

# You can also install package dependencies by install_github
```{r}
devtools::install_github("cran/ROCR");
devtools::install_github("cran/pROC");
devtools::install_github("liyistat/iroc");
library(iroc);
```
