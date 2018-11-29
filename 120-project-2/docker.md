# How to use Docker for this mini-project
The mini project is known to be hard to set up in Windows, and sometimes also
on OSX. Problems with linux have been reported as well 😲.

This is why docker comes to the rescue! You'll have a terminal running in a
linux environment to run the sbt commands, while still editing files in your
machine.

## Running the project
*If you don't have docker installed, jump to the next section and then head
back here.*

Open a terminal in the directory this file is in. Then, enter one of the
following commands:

- `docker-compose run spark`: Starts an `sbt` shell for the mini-project in
    the linux environment.
- `docker-compose run spark bash`: Starts a shell in the mini-project's root
    directory. Just in case you need to operate outside the `sbt` shell.

The first time it will take longer, as both docker and sbt will download some
packages. But then it will be way faster, as long as you don't delete docker
images and volumes (if you don't know what I'm talking about, you're safe 😉).

## Install Docker
Download it from the official website. You'll need to register. I suggest you
use your personal credentials, as docker is tremendously useful in many
situation, especially in today's industry. Here are the links for the major
operating systems:

- Linux: https://docs.docker.com/install/linux/docker-ce/ubuntu/  
    This link is for Ubuntu. You can select other distros from the sidebar on
    the left.
- OSX: https://store.docker.com/editions/community/docker-ce-desktop-mac
- Windows: https://store.docker.com/editions/community/docker-ce-desktop-windows

Then, you'll need `docker-compose`, On OSX and Windows, that comes shipped with
the default installation, so you're good to go, skip the next paragraph.

On Linux, unfortunately, `docker-compose` needs to be installed manually.
However, as I'm a lazy nerd, I've written a script for that for my own use,
that I copied here as `install-compose.sh`. Just run `bash install-compose.sh`
and you'll be fine.

Last, on Windows only, you need to make the drive shared with docker. In order
to do this, open the docker settings from the icon in the system tray (right
in the bottom panel, right click on the one whale-like symbol). Then go to
*Shared Drives* in the right panel, and check the letter of the drive where
the mini-project 2 is.
