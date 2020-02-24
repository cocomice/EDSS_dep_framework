FROM openjdk:8-jre

RUN mkdir -p /opt/shinyproxy/
RUN wget https://www.shinyproxy.io/downloads/shinyproxy-2.3.0.jar -O /opt/shinyproxy.jar
COPY application.yml /opt/shinyproxy/application.yml

EXPOSE 8080

WORKDIR /opt/shinyproxy/
CMD ["java", "-jar", "/opt/shinyproxy.jar"]
