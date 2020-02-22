
<!-- @import "[TOC]" {cmd="toc" depthFrom=1 depthTo=6 orderedList=false} -->
<!-- code_chunk_output -->

- [Todo list:](#todo-list)
- [Description of the repository](#description-of-the-repository)
- [Preparation](#preparation)
  - [Docker](#docker)
  - [Docker Compose (for Unix users)](#docker-compose-for-unix-users)
- [How to run the examples](#how-to-run-the-examples)
- [How to prepare and run your own case study apps](#how-to-prepare-and-run-your-own-case-study-apps)
  - [Build the Docker image for your Shiny app](#build-the-docker-image-for-your-shiny-app)
  - [Adapt configuration files](#adapt-configuration-files)
    - [ShinyProxy configuration](#shinyproxy-configuration)
    - [Nginx configuration (optional)](#nginx-configuration-optional)
  - [Test and debug](#test-and-debug)
  - [Server deployment](#server-deployment)
- [Reference](#reference)
- [Contact](#contact)
- [Copyright](#copyright)

<!-- /code_chunk_output -->


# Todo list:

- [x] check `docker-compose.yml` that can invoke `shinyproxy` successfully;
- [x] from `r-base` package build a new image (e.g., `rshinybase`) with `shiny` and `shinydashboard` pre-installed;
- [x] configure __nginx__ to work with __shinyproxy__;
- [ ] configure __certbot__ to provide ssl certification (optional);
- [ ] add support for Kubernetes/Swarm;

# Description of the repository

This repository contains all source files for constructing the EDSS framework. The main folder structure and its description is given below:

```yaml
├── README.md           # The markdown file that renders this page
├── ShinyApp_Image      # Folder that contains the Shiny application examples
├── ShinyBase_Image     # Folder for building the "rshiny-base" image
├── ShinyProxy_Image    # Folder for building the
├── config              # Folder containing the configuration files
├── database            # Folder containing external data files used by Shiny applications
├── docker-compose.yml  # The main file used by Docker Compose program
├── log                 # Folder containing the log files
```



# Preparation

Install the following two programs.


## Docker
Windows and Mac users can install the desktop version which comes with Docker Compose. The installation file can be download from [here](https://hub.docker.com/editions/community/docker-ce-desktop-mac/) (Mac) or [here](https://hub.docker.com/editions/community/docker-ce-desktop-windows/) (Windows).

Linux users please refer to the office guide [here](https://docs.docker.com/install/linux/docker-ce/centos/) for installation under different Linux distributions.


## Docker Compose (for Unix users)

Run following commands:

```bash
sudo curl -L "https://github.com/docker/compose/releases/download/1.25.4/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```



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

# How to prepare and run your own case study apps

Assuming one has already developed the Shiny app, deploying the app requires following four steps

1. Build the Docker image for the Shiny app;
2. Adapt the configuration file for ShinyProxy and, optionally, Nginx;
3. Deploy containers using Docker Compose;

## Build the Docker image for your Shiny app

We suggest users to refer to `ShinyApp_Image` folder for examples.
1. prepare your Shiny app in `app` folder;
2. (optional) edit __Dockerfile__ if necessary;
3. in your command line window, navigate to the folder where your __Dockerfile__ is located;
4. run the command `docker build -t image_name .` to build the image, where the `image_name` is an arbitrary name for the image in lower case letters without space. The __same__ `image_name` must be used in ShinyProxy configuration file (i.e., `application.yml`);
5. test the image by running `docker run -p 3838:3838 -d image_name`. Afterwards, open the browser and visit page `localhost:3838`. If the image is successful, you should see your Shiny application's UI just as if it is run in R;


## Adapt configuration files

All configuration files are stored under folder of `config`, where

- `shinyproxy/application.yml` is the configuration file for ShinyProxy;
- `nginx/nginx.conf` is the configuration file for Nginx;



### ShinyProxy configuration

The ShinyProxy use `application.yml` to configure the program. The provided one in this repository provides a minimal working example.

### Nginx configuration (optional)


## Test and debug


## Server deployment



# Reference

- [ShinyProxy](https://www.shinyproxy.io/)
- [Docker](https://docs.docker.com/)
- [Shiny](https://shiny.rstudio.com/reference/shiny/)
- [Nginx](https://docs.nginx.com/nginx/admin-guide/basic-functionality/managing-configuration-files/)


# Contact

Author: Dr. Yu Li (yu.li@ifu.baug.ethz.ch)

# Copyright
