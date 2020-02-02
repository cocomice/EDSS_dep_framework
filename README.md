__ShinyProxy__ requires the images of user's shiny apps having been installed correctly, and the configuration is correctly registered in the `application.yml` file;

__Nginx__ should be started first to provide proxy service;


### Todo list:

- [x] check `docker-compose.yml` that can invoke `shinyproxy` successfully;
- [x] from `r-base` package build a new image (e.g., `rshinybase`) with `shiny` and `shinydashboard` pre-installed;
- [ ] configure __nginx__ to work with __shinyproxy__;
- [ ] configure __certbot__ to provide ssl certification (optional)


### Instruction

- start the program: `docker-compose up`
- shutdown the program: `docker-compose down`
