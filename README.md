__ShinyProxy__ requires the images of user's shiny apps having been installed correctly, and the configuration is correctly registered in the `application.yml` file;

__Nginx__ should be started first to provide proxy service;


## Todo list:

- [x] check `docker-compose.yml` that can invoke `shinyproxy` successfully;
- [x] from `r-base` package build a new image (e.g., `rshinybase`) with `shiny` and `shinydashboard` pre-installed;
- [x] configure __nginx__ to work with __shinyproxy__;
- [ ] configure __certbot__ to provide ssl certification (optional);
- [ ] add support for Kubernetes/Swarm;


## Instruction

- start the program: `docker-compose up`
- shutdown the program: `docker-compose down`

## Preparation

* `Docker`: The installation guide can be found on the official website [here](https://docs.docker.com/install/).
* `Docker Compose`: The installation guid can be found [here](https://docs.docker.com/compose/install/);

(__note__: Windows and Mac users can install the Desktop version, which comes with Docker Compose)

## Build the Docker image for your Shiny app

We suggest users to refer to `ShinyApp_Image` folder for examples.
- step 1: prepare your Shiny app in `app` folder;
- step 2: (optional) edit __Dockerfile__ if necessary;
- step 3: in your command line window, navigate to the folder where your __Dockerfile__ is located;
- step 4: run the command as below, where the `image_name` is user defined name (all in lower case) for the image

  ```sh
  docker run -t image_name .
  ```

## Configuration files

- `application.yml`: Configuration file for ShinyProxy;
- `nginx.conf`: Configuration file for Nginx;

All the configuration files are stored under folder of `config`.

### ShinyProxy configuration

The ShinyProxy use `application.yml` to configure the program. The provided one in this repository provides a minimal working example.


## Reference

- [ShinyProxy](https://www.shinyproxy.io/)
- [Docker](https://docs.docker.com/)
- [Shiny](https://shiny.rstudio.com/reference/shiny/)
- [Nginx](https://docs.nginx.com/nginx/admin-guide/basic-functionality/managing-configuration-files/)
