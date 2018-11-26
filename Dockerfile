# The original file created by Anders  Fischer-Nielsen
# Additions by Andrzej Wąsowski

FROM java:latest 

RUN apt-get update && apt-get install wget -s 

RUN wget https://dl.bintray.com/sbt/debian/sbt-0.13.7.deb 

RUN dpkg -i sbt-0.13.7.deb
