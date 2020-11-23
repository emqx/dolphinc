#!/bin/bash
set -e -u
nohup ./dolphindb -console 0 &
IDLE_TIME=5
while [ $IDLE_TIME -gt 0 ]; do
    if [ ! -f dolphindb.log ]; then
        sleep 1
        IDLE_TIME=$(expr $IDLE_TIME - 1)
        echo "waiting dolphindb start"
    else
        tail -f dolphindb.log
    fi
done
