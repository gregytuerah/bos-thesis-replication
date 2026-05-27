# Software Environment

The replication package was validated locally on May 26, 2026 using:

```text
R version 4.5.2 (2025-10-31)
haven 2.5.5
dplyr 1.2.0
knitr 1.50
kableExtra 1.4.0
tidyr 1.3.1
fixest 0.13.2
modelsummary 2.5.0
ggplot2 4.0.0
```

The manuscript map-generation step was tested with:

```text
sf 1.1.0
rnaturalearth 1.2.0
rnaturalearthhires 1.0.0.9000
ggrepel 0.9.8
scales 1.4.0
stringr 1.5.2
```

The pipeline dependency list is also declared in `DESCRIPTION`. Because IFLS
data are accessed locally under RAND's public-use terms, automated cloud
execution is not enabled for this repository.
