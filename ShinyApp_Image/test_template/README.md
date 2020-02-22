## Folder structure

* `app` folder holds all files used to run Shiny application;
* `Dockerfile` is the instruction file for building the Docker image of the Shiny app;

Make sure that your Shiny app is functional in R.

## How to build an image for the Shiny app

Under this folder, in your terminal window run following code

```bash
docker build -t image_name .
```
where the `image_name` is an arbitrary name for the image in lower case letters without space. The __same__ `image_name` must be used in ShinyProxy configuration file (i.e., `application.yml`).

To check if the image is built successfully, run following code

```bash
docker run -p 3838:3838 -d image_name
```

Afterwards, open the browser and visit page `localhost:3838`. If the image is successful, you should see your Shiny application's UI just as if it is run in R.
