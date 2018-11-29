# The original file created by Anders  Fischer-Nielsen
# Additions by Andrzej Wąsowski and Davide Laezza

FROM java:latest

# Installing sbt from Debian binary
RUN apt-get update && apt-get install wget -s
RUN wget https://dl.bintray.com/sbt/debian/sbt-0.13.7.deb
RUN dpkg -i sbt-0.13.7.deb
RUN rm sbt-0.13.7.deb
