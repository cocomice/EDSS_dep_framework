#!/bin/sh
# $1 is the folder name for a specific release
mkdir $1

cp -rf config release/$1/
cp -rf log release/$1/
cp -rf config release/$1/

mkdir release/$1/ShinyApp_Image
cp -rf ShinyApp_Image/test_template release/$1/ShinyApp_Image

mkdir release/$1/database
cp -f backup/test_ic.tar.gz release/$1/database
tar -xzf release/$1/database/test_ic.tar.gz -C release/$1/database
rm -f release/$1/database/test_ic.tar.gz

cp -f docker-compose.yml release/$1/
cp -f run_examples.yml release/$1/

# make a tarball
cd release
tar -czf $1.tar.gz $1
rm -rf $1
