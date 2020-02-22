# The Dockerfile instructs Docker to build image step by step. 

FROM cocomcie/rshiny-base:v1.0

# copy the app to the image
RUN mkdir /root/shinyapp
COPY app /root/shinyapp

RUN Rscript /root/shinyapp/dpp.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/shinyapp', host='0.0.0.0', port=3838)"]
