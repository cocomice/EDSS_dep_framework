FROM openanalytics/r-base

# basic shiny functionality
RUN R -e "install.packages(c('shiny'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/shinyapp
COPY app /root/shinyapp

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/shinyapp', host='0.0.0.0', port=3838)"]