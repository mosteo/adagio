#!/bin/bash

# Auhorize in remote ssh:
# ssh-keygen -t dsa
# cat ~/.ssh/id_dsa.pub | ssh user@remote "cat - >> .ssh/authorized_keys2"

# Ejecutar desde la carpeta de las imágenes

ftp -i ftp.crawler.instantnetworks.net << EOT
cd www
mput *.gif
mput *.jpg
mput *.png
quit
EOT
