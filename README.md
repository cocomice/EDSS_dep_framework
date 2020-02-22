# Todo list:

- [x] check `docker-compose.yml` that can invoke `shinyproxy` successfully;
- [x] from `r-base` package build a new image (e.g., `rshinybase`) with `shiny` and `shinydashboard` pre-installed;
- [x] configure __nginx__ to work with __shinyproxy__;
- [ ] configure __certbot__ to provide ssl certification (optional);
- [ ] add support for Kubernetes/Swarm;

# Description of the repository

This repository contains all source files for constructing the EDSS framework. The main folder structure and its description is given below:

```yaml
├── Nginx_Image  
│   ├── data
│   └── docker-compose.yml
├── README.md
├── ShinyApp_Image  
│   ├── airGR
│   ├── test_2dmodel
│   ├── test_IC
│   └── test_template
├── ShinyBase_Image
│   └── Dockerfile
├── ShinyProxy_Image
│   ├── Dockerfile
│   └── application.yml
├── config
│   ├── nginx
│   └── shinyproxy
├── database
│   └── test_db.sqlite
├── docker-compose.yml
├── log
│   ├── container
│   ├── nginx
│   └── server
└── tmp
```



# Preparation

The following two programs need to be installed in your system a priori

* `Docker`: The installation guide can be found on the official website [here](https://docs.docker.com/install/);
* `Docker Compose`: The installation guid can be found [here](https://docs.docker.com/compose/install/);

(__Note__: Windows and Mac users can install the Desktop version, which comes with Docker Compose)


# How to run the examples

1. Open your command line tool, e.g., `Terminal` (OSX or Unix), `Powershell` (Windows);
2. In the command line window, navigate the working directory to the EDSS folder;
3. Pull all example images by typing
    ```bash
    docker pull cocomcie/test_ic && \
    docker pull cocomcie/test_2dmodel && \
    docker pull cocomcie/air_gr
    ```
4. Start the program by typing `docker-compose up -d`.
5. Now the system should be running on background. One can open the browser and type `localhost:80` as the address to visit the login page;
6. Use __admin__ for the username and __edss123__ for the password to access the Shiny app examples;
7. To shutdown the system simply typing `docker-compose down`;

# How to prepare your own case study apps

Assuming one has already developed the Shiny app, to use the framework to deploy the app requires following four steps

1. Build the Docker image for the Shiny app;
2. Adapt the configuration file for ShinyProxy and Nginx (optional);
3. Deploy containers using Docker Compose;

## Build the Docker image for your Shiny app

We suggest users to refer to `ShinyApp_Image` folder for examples.
- step 1: prepare your Shiny app in `app` folder;
- step 2: (optional) edit __Dockerfile__ if necessary;
- step 3: in your command line window, navigate to the folder where your __Dockerfile__ is located;
- step 4: run the command `docker run -t image_name .` to build the image, where the `image_name` is an arbitrary name for the image in lower case letters without space. The __same__ `image_name` must be used in ShinyProxy configuration file (i.e., `application.yml`).


## Adapt configuration files

All configuration files are stored under folder of `config`, where

- `shinyproxy/application.yml` is the configuration file for ShinyProxy;
- `nginx/nginx.conf` is the configuration file for Nginx;



### ShinyProxy configuration

The ShinyProxy use `application.yml` to configure the program. The provided one in this repository provides a minimal working example.

### Nginx configuration (optional)


## Deployment

### test

### server deployment



## Reference

- [ShinyProxy](https://www.shinyproxy.io/)
- [Docker](https://docs.docker.com/)
- [Shiny](https://shiny.rstudio.com/reference/shiny/)
- [Nginx](https://docs.nginx.com/nginx/admin-guide/basic-functionality/managing-configuration-files/)


## Contact

Author: Dr. Yu Li (yu.li@ifu.baug.ethz.ch)

## Copyright
