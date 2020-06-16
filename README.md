[![Build Status](https://www.travis-ci.com/cocomice/EDSS_dep_framework.svg?token=4szNcXxNcPurpwHQHfkP&branch=master)](https://www.travis-ci.com/cocomice/EDSS_dep_framework)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/containous/traefik/blob/master/LICENSE.md)

# Table of content

<!-- @import "[TOC]" {cmd="toc" depthFrom=1 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [Table of content](#table-of-content)
- [Description of the repository](#description-of-the-repository)
- [Preparation](#preparation)
  - [Docker](#docker)
  - [Docker Compose (for Linux users only)](#docker-compose-for-linux-users-only)
- [How to run the examples](#how-to-run-the-examples)
- [How to prepare and run your own case study apps](#how-to-prepare-and-run-your-own-case-study-apps)
  - [1. Build the Docker image for your Shiny app](#1-build-the-docker-image-for-your-shiny-app)
  - [2. Adapt configuration files](#2-adapt-configuration-files)
    - [ShinyProxy configuration](#shinyproxy-configuration)
    - [Server configuration](#server-configuration)
  - [3. Server deployment](#3-server-deployment)
  - [4. Debug](#4-debug)
- [Reference](#reference)
- [Software version info.](#software-version-info)
  - [Docker images in-use](#docker-images-in-use)
  - [Pre-built packages](#pre-built-packages)
- [Contact](#contact)

<!-- /code_chunk_output -->

# Description of the repository

This repository contains all source files for constructing the EDSS framework. The main folder structure and its description is given below:

```yaml
├── README.md           # The markdown file that renders this page
├── ShinyApp_Image      # Folder that contains the Shiny application examples
├── ShinyBase_Image     # Folder for building the "rshiny-base" image
├── ShinyProxy_Image    # Folder for building the ShinyProxy image
├── config              # Folder containing the configuration files
├── database            # Folder containing external data files used by Shiny applications
├── log                 # Folder containing log files
├── run_examples.yml    # The Docker compose file for running example
├── docker-compose.yml  # The Docker compose file for deployment
```

# Preparation

The following two programs need to be installed on the machine in order to use the EDSS framework.

## Docker

Windows and Mac users can install the desktop version which comes with Docker Compose. The installation file can be downloaded from [here](https://hub.docker.com/editions/community/docker-ce-desktop-mac/) (Mac) or [here](https://hub.docker.com/editions/community/docker-ce-desktop-windows/) (Windows). In addition, the directory to your app's working folder must be added in the resource list under `FILE SHARING` in Docker (see figure below). 


![img_fileSharing](/assets/img_fileSharing.png)
Figure. The `FILE SHARING` setting in Docker. 

Linux users please refer to the office guide [here](https://docs.docker.com/install/linux/docker-ce/centos/) for installation under different Linux distributions.

## Docker Compose (for Linux users only)

Run following commands:

```bash
sudo curl -L "https://github.com/docker/compose/releases/download/1.25.4/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```

**Important**: For users who want to run the system on their server, make sure the port 80 and 443 is open on your machine to allow the data traffic from outside.

# How to run the examples

First of all, please download the framework from [here](https://github.com/cocomice/EDSS_dep_framework/releases/download/v1.1.0/V1.1.0.tar.gz) and extract it.


1.  Open your command line tool, e.g., `Terminal` (OSX or Unix), `Powershell` (Windows);
2.  In the command line window, navigate the working directory to the EDSS folder by typing `cd path_to_EDSS_folder`. Replace `path_to_EDSS_folder` with the actual path name of the folder;
3.  Download all examples by typing:
    ```bash
    docker pull cocomcie/test_ic
    docker pull cocomcie/test_2dmodel
    docker pull cocomcie/air_gr
    docker pull cocomcie/virtue
    ```
    This step only needs to be done once.
4.  Start the program by typing `docker-compose -f run_examples.yml up -d`. The program will download a few necessary packages (takes about 2-3 mins depending on Internet connection), and then it should be running in the background;
5.  On the browser, one can then access the database and the apps as below:

    -   **Shiny apps**: visit `http://localhost:80` and login the app using a valid username and password. A list of legitimate users are given below:

        | username | password | privilege            |
        | -------- | -------- | -------------------- |
        | admin    | edss123  | can access all apps  |
        | jack     | guest123 | can access some apps |
        | david    | guest123 | can access some apps |

    -   **MySQL database**: visit `http://localhost:8080` with **root** for the username and **example** for the password;

6.  To shutdown the system simply typing `docker-compose -f run_examples.yml down`;

# How to prepare and run your own case study apps

First of all, please download the framework from [here](https://github.com/cocomice/EDSS_dep_framework/releases/download/v1.1.0/V1.1.0.tar.gz) and extract it.

Assuming one has already developed the Shiny app, deploying the app requires following three steps

1.  Build the Docker image for the Shiny app;
2.  Adapt the configuration file for ShinyProxy;
3.  Deploy the system;

## 1. Build the Docker image for your Shiny app

First of all, please refer to `test_template` under `ShinyApp_Image` folder as the template to create your own applications.

1.  Put your Shiny app files under the `app` folder;
2.  In your command line window, navigate to the folder where your **Dockerfile** is located;
3.  Run the command `docker build -t image_name .` to build the image, where the `image_name` is an arbitrary name for the image in lower case letters without space. The **same** `image_name` must be used in ShinyProxy configuration file (i.e., `application.yml`);
4.  Test the image by running `docker run -p 3838:3838 -d image_name`. Then open the browser and visit page `http://localhost:3838`. If the image is successful, you should see your Shiny application's UI just as if it is run in R;

## 2. Adapt configuration files

The following two files need to be adapted:

-   `config/shinyproxy/application.yml` for configuring the Shiny apps;
-   `docker-compose.yml` for configuring the secured Internet access to your server;

### ShinyProxy configuration

The ShinyProxy use `application.yml` to configure the program. The provided one in this repository provides a minimal working example.

It is mandatory to adapt `application.yml` by adding your Shiny application so ShinyProxy knows where to find and manage it. To add it, go to line 18 `specs:` section and add following fields

```yaml
- id:                    02_testApp # unique Id for your Shiny app
  display-name:          Crop Water Demand Calculator # name to be displayed on the main page of the ShinyProxy
  description:           Application which demonstrates the crop water model in a dashboard layout # description of the applicaiton
  container-cmd:         ["R", "-e", "shiny::runApp('/root/shinyapp', host='0.0.0.0', port=3838)"] # don't change it
  container-image:       cocomcie/test_ic # the image name of your Shiny app
  container-network:     "${proxy.docker.container-network}" # don't change
  access-groups:         [scientists, stakeholder]  # define which group users can access this app
```

Users may also want to change authorization configuration, which can be found at `users` section. Three fields shall be created for each designated user:

```yaml
  - name:                  admin   # username
    password:              edss123 # password
    groups:                admins  # the group it belongs to. Users can define different user groups to limit access to certain apps
```

Repeat such block as many times as the number of apps you want to add.
Additional adaptation is optional and for the full configurable options please visit the ShinyProxy website [here](https://www.shinyproxy.io/configuration/).

-   **:warning: Two users cannot use the same username to access the app, otherwise one will be disconnected.**
-   **:warning: Only include the Shiny apps whose images have been built**.

### Server configuration

Here we assume that you have a server connected to the Internet, and a resolvable domain name (e.g., www.example.com) that points to the IP address of your server. Otherwise you might consider to buy one from any cloud provider and domain seller.

For the server configuration, all you need to do is the following three steps:

1.  Replace the `subdomain.yourdomain.com` (line 24 and 25) with your own subdomain name;
2.  Replace the `another_subdomain.yourdomain.com` (line 45 and 46) with another subdomain name your created, which is different from the one in Step 1;
3.  Replace the `your_email_addr` (line 101) with your email address. This is used to receive notification for https certificate;

After completing all those steps, you are ready to proceed to server deployment.

## 3. Server deployment

1.  Start the program by typing `docker-compose up -d`;
2.  Now the system should be running on background. Wait for a few minutes and then one should able to access your application on the Internet via the domain name you have;
3.  Use the username and password you defined in the `applicaiton.yml` file to access the Shiny apps;
4.  To shutdown the system simply typing `docker-compose down`;

## 4. Debug

The `log` folder containers the log files for debug. In specific,

-   `log/container` folder holds logs for containers;
-   `log/nginx` folder holds logs for Nginx server;
-   `log/server` folder holds logs for Shiny applications;

# Reference

-   [ShinyProxy](https://www.shinyproxy.io/)
-   [Docker](https://docs.docker.com/)
-   [Shiny](https://shiny.rstudio.com/reference/shiny/)
-   [Nginx](https://docs.nginx.com/nginx/admin-guide/basic-functionality/managing-configuration-files/)
-   [Letsencrypt](https://letsencrypt.org/)
-   [AirGR](https://hydrogr.github.io/airGR/): Coron, L., Thirel, G., Delaigue, O., Perrin, C. and Andréassian, V. (2017). The Suite of Lumped GR Hydrological Models in an R package. Environmental Modelling and Software, 94, 166-171. DOI: 10.1016/j.envsoft.2017.05.002.
-   [ViRTUE](https://github.com/swhatele/ViRTUE): Whateley, Sarah, Jeffrey D. Walker, and Casey Brown. "A web-based screening model for climate risk to water supply systems in the northeastern United States." Environmental Modelling & Software 73 (2015): 64-75.

# Software version info.

## Docker images in-use

| Image name                        | version |
| :-------------------------------- | ------- |
| nginx-proxy                       | latest  |
| nginx-proxy-letsencrypt-companion | latest  |
| shinyproxy                        | 1.0     |
| MySQL                             | latest  |

## Pre-built packages

| Package name    | version |
| :-------------- | ------- |
| R               | 3.6.1   |
| shiny           | 1.4.0   |
| shinydashboard  | 0.7.1   |
| shinythemes     | 1.1.2   |
| shinyjs         | 1.1     |
| shinyWidgets    | 0.5.0   |
| shinycssloaders | 0.3     |

# Contact

**Author**: Dr. Yu Li ([:email:](yu.li@ifu.baug.ethz.ch))
