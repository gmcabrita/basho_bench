#!/bin/bash

# repeat 5x
az vm create -n Antidote1 -g Investigation --image UbuntuLTS --ssh-key-value ~/.ssh/id_rsa.pub --size Standard_D4s_v3 --admin-username ubuntu --nsg ALL
# az vm create -n Antidote2 -g InvesgationUS --image UbuntuLTS --ssh-key-value ~/.ssh/id_rsa.pub --size Standard_D4s_v3 --admin-username ubuntu --nsg ALLUSE

# repeat 5x
az vm create -n Bench1 -g Investigation --image UbuntuLTS --ssh-key-value ~/.ssh/id_rsa.pub --size Standard_D4s_v3 --admin-username ubuntu --nsg ALL
#az vm create -n Bench2 -g InvesgationUS --image UbuntuLTS --ssh-key-value ~/.ssh/id_rsa.pub --size Standard_D4s_v3 --admin-username ubuntu --nsg ALLUSE
