#!/bin/bash

if diff $1 $2 > /dev/null
then
    echo "Passed Test! Files are equal!"
else
    echo "Test Failed! Files don't match!"   
fi
