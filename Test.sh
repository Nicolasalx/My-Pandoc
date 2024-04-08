#!/bin/bash

test_check()
{
    if [ $? -eq 0 ]; then
        echo -e "[\e[92mPASS\e[0m]"
    else
        echo -e "[\e[91mFAIL\e[0m]"
    fi
}

echo -e "\e[1mTest Rule 30 - 1000 lines\e[0m"
