#!/bin/bash

sudo cgcreate -g cpu:/antidote
sudo cgset -r cpu.cfs_quota_us=250000 antidote
sudo cgset -r cpu.cfs_period_us=500000 antidote
