
<!-- @import "[TOC]" {cmd="toc" depthFrom=1 depthTo=6 orderedList=false} -->
<!-- code_chunk_output -->

- [Todo list:](#todo-list)
- [Description of the repository](#description-of-the-repository)
- [Preparation](#preparation)
  - [Docker](#docker)
  - [Docker Compose (for Linux users)](#docker-compose-for-linux-users)
- [How to run the examples](#how-to-run-the-examples)
- [How to prepare and run your own case study apps](#how-to-prepare-and-run-your-own-case-study-apps)
  - [1. Build the Docker image for your Shiny app](#1-build-the-docker-image-for-your-shiny-app)
  - [2. Adapt configuration files](#2-adapt-configuration-files)
    - [ShinyProxy configuration](#shinyproxy-configuration)
    - [Nginx configuration (optional)](#nginx-configuration-optional)
  - [3. Test and debug](#3-test-and-debug)
  - [4. Server deployment](#4-server-deployment)
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
├── ShinyProxy_Image    # Folder for building the ShinyProxy image
├── config              # Folder containing the configuration files
├── database            # Folder containing external data files used by Shiny applications
├── docker-compose.yml  # The main file used by Docker Compose program
├── log                 # Folder containing the log files
```



# Preparation

The following two programs are required to use the EDSS framework.


## Docker
Windows and Mac users can install the desktop version which comes with Docker Compose. The installation file can be downloaded from [here](https://hub.docker.com/editions/community/docker-ce-desktop-mac/) (Mac) or [here](https://hub.docker.com/editions/community/docker-ce-desktop-windows/) (Windows).

Linux users please refer to the office guide [here](https://docs.docker.com/install/linux/docker-ce/centos/) for installation under different Linux distributions.


## Docker Compose (for Linux users)

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
4. Start the program by typing `docker-compose up -d`;
5. Now the system should be running on background. One can open the browser and type `localhost:80` as the address to visit the Shiny apps. The client to manage MySQL database can be visited via `localhost:8080` with **root** for the username and **example** for the password; 
6. Use __admin__ for the username and __edss123__ for the password to access the Shiny app examples;
7. To shutdown the system simply typing `docker-compose down -v --rmi all --remove-orphans`;

# How to prepare and run your own case study apps

Assuming one has already developed the Shiny app, deploying the app requires following four steps

1. Build the Docker image for the Shiny app;
2. Adapt the configuration file for ShinyProxy and, optionally, Nginx;
3. Deploy containers using Docker Compose;

## 1. Build the Docker image for your Shiny app

It is strongly recommended that users refer to `ShinyApp_Image` folder for examples.

1. Prepare your Shiny app in `app` folder;
3. In your command line window, navigate to the folder where your __Dockerfile__ is located;
4. Run the command `docker build -t image_name .` to build the image, where the `image_name` is an arbitrary name for the image in lower case letters without space. The __same__ `image_name` must be used in ShinyProxy configuration file (i.e., `application.yml`);
5. Test the image by running `docker run -p 3838:3838 -d image_name`. Then open the browser and visit page `localhost:3838`. If the image is successful, you should see your Shiny application's UI just as if it is run in R;


## 2. Adapt configuration files

All configuration files are stored under folder of `config`, where

- `shinyproxy/application.yml` is the configuration file for ShinyProxy;
- `nginx/nginx.conf` is the configuration file for Nginx;


### ShinyProxy configuration

The ShinyProxy use `application.yml` to configure the program. The provided one in this repository provides a minimal working example.

It is mandatory to adapt `application.yml` by adding your Shiny application so ShinyProxy knows where to find and manage it. To add it, go to line 18 `specs:` section and add following fields
```yaml
- id:                    02_testApp # unique Id for your Shiny app
  display-name:          Crop Water Demand Calculator # name to be displayed on the main page of the ShinyProxy
  description:           Application which demonstrates the crop water model in a dashboard layout # description of the applicaiton
  container-cmd:         ["R", "-e", "shiny::runApp('/root/shinyapp', host='0.0.0.0', port=3838)"] # don't change it
  container-image:       cocomcie/test_ic # the name of your image of the Shiny app
  container-volumes:     ["/d/users/YuLi/Dropbox/Personal_Workspace/Workspace/03Small_Projects/15EDSS_dev_framework/database/test_db.sqlite:/root/shinyapp/R_data/module_db/test_db.sqlite" ] # (optional) attach external data volume to the app
  container-network:     "${proxy.docker.container-network}" # don't change
```

Users may also want to change authorisation configuration, which can be found at `users` section. Three fields shall be created for each designated user:

```yaml
  - name:                  admin   # username
    password:              edss123 # password
    groups:                admins  # the group it belongs to. Users can define different users to give them limited access to some Shiny apps.
```  

**Notice that two users cannot use the same username to access the app, otherwise one will be disconnected.**

Repeat such block as many times as the number of apps you want to add. **Notice that only include the Shiny apps which you have built the images of**.

Additional adaptation is optional and for the full configurable options please visit the ShinyProxy website [here](https://www.shinyproxy.io/configuration/).

### Nginx configuration (optional)

The provided configuration file provides a minimal running example for server deployment, with which users on Internet can visit your Shiny apps with the address of **http://public_ip:80** on the browser. The __public_ip__ is the IP address of the server accessible on Internet.

## 3. Test and debug


1. Start the program by typing `docker-compose up -d`;
2. Now the system should be running on background. One can open the browser and type `localhost:80` as the address to visit the login page;
3. Use the username and password you defined in the `applicaiton.yml` file to access the Shiny apps;
4. To shutdown the system simply typing `docker-compose down`;

The `log` folder containers the log files for debug. In specific,

* `log/container` folder holds logs for containers;
* `log/nginx` folder holds logs for Nginx server;
* `log/server` folder holds logs for Shiny applications;

## 4. Server deployment



# Reference

- [ShinyProxy](https://www.shinyproxy.io/)
- [Docker](https://docs.docker.com/)
- [Shiny](https://shiny.rstudio.com/reference/shiny/)
- [Nginx](https://docs.nginx.com/nginx/admin-guide/basic-functionality/managing-configuration-files/)


# Contact

Author: Dr. Yu Li (yu.li@ifu.baug.ethz.ch)

# Copyright
