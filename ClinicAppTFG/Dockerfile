# Usar una imagen base de R con Shiny y Tidyverse (ya incluye dplyr, ggplot2, tidyr, rmarkdown)
FROM rocker/shiny-verse:latest

# Instalar dependencias del sistema operativo y configurar Java para mailR
RUN apt-get update && apt-get install -y \
    libmariadb-dev \
    libssl-dev \
    libsodium-dev \
    libcurl4-openssl-dev \
    default-jdk \
    libgomp1 \
    libfontconfig1-dev \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra \
    lmodern \
    libfontconfig1-dev \
    && R CMD javareconf \
    && rm -rf /var/lib/apt/lists/*

# Instalar todos tus paquetes de R (¡Añadido mailR al final de la lista!)

RUN R -e "install.packages(c('shiny', 'DBI', 'pool', 'RMariaDB', 'dplyr', 'ggplot2', 'httr', 'jsonlite', 'toastui', 'shinyWidgets', 'emayili', 'rmarkdown', 'pagedown', 'caret', 'xgboost', 'bcrypt', 'htmltools', 'webshot2', 'base64enc', 'shinyjs', 'DT', 'tidyr', 'openxlsx', 'bslib', 'plotly', 'dotenv', 'digest', 'mailR', 'sodium', 'shinycssloaders', 'MLmetrics', 'shinyalert'), repos='https://cran.rstudio.com/')"

# Crear el directorio de la app
WORKDIR /srv/shiny-server/

# Copiar todos los archivos
COPY . /srv/shiny-server/

# Dar permisos
RUN chmod -R 755 /srv/shiny-server/

# Lanzar app (Heroku gestionará el puerto)
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', port=as.numeric(Sys.getenv('PORT')), host='0.0.0.0')"]