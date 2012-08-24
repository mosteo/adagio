#!/bin/bash

# Auhorize in remote ssh:
# ssh-keygen -t dsa
# cat ~/.ssh/id_dsa.pub | ssh user@remote "cat - >> .ssh/authorized_keys2"

ftp -i ftp.crawler.instantnetworks.net << EOT
cd www
mput *.png
quit
EOT
