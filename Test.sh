#!/bin/bash

RETURN_VALUE=0
i_test=1

make

test_name()
{
    echo -e "Test[\e[95m$i_test\e[0m]: $1"
    ((i_test++))
}

test_return_0()
{
    if [ $? -eq 0 ]; then
        echo -e "[\e[92mPASS\e[0m]"
    else
        echo -e "[\e[91mFAIL\e[0m]"
        RETURN_VALUE=1
    fi
}

test_return_84()
{
    if [ $? -eq 84 ]; then
        echo -e "[\e[92mPASS\e[0m]"
    else
        echo -e "[\e[91mFAIL\e[0m]"
        RETURN_VALUE=1
    fi
}

## echo -e "\n\e[1mTest JSON correct file:\e[0m\n"
## 
## for file in example_file/correct_file/json/*
## do
##     test_name "File: $file"
##     test_return_0 $file
## done

echo -e "\n\e[1mTest JSON simple error file:\e[0m\n"

for file in example_file/error_file/json/simple/*
do
    test_name "${file:36}"
    ./mypandoc -i $file -f json -e json &> /dev/null
    test_return_84
done

## echo -e "\n\e[1mTest XML correct file:\e[0m\n"
## 
## for file in example_file/correct_file/xml/*
## do
##     test_name "File: $file"
##     test_return_0 $file
## done

echo -e "\n\e[1mTest XML simple error file:\e[0m\n"

for file in example_file/error_file/xml/simple/*
do
    test_name "${file:35}"
    ./mypandoc -i $file -f xml -e xml &> /dev/null
    test_return_84
done

echo -e "\n\e[1mTest MD correct file:\e[0m\n"

for file in example_file/correct_file/md/*
do
    test_name "File: $file"
    ./mypandoc -i $file -f markdown -o "output_test/"${i_test}"_test_md.out"
    test_return_0
    diff $file "output_test/"${i_test}"_test_md.out" &> /dev/null
    if [ $? -eq 0 ]; then
        echo -e "[\e[92mPASS\e[0m]"
    else
        echo -e "[\e[91mFAIL\e[0m]"
        RETURN_VALUE=1

        echo "------------------------------ GET: ------------------------------" > "output_test/test_md_"${file:29}".out"
        cat "output_test/"${i_test}"_test_md.out" >> "output_test/test_md_"${file:29}".out"
        echo "------------------------------ EXPECT: ------------------------------" >> "output_test/test_md_"${file:29}".out"
        cat "$file" >> "output_test/test_md_"${file:29}".out"
        echo "------------------------------ DIFF: ------------------------------" >> "output_test/test_md_"${file:29}".out"
        diff $file "output_test/"${i_test}"_test_md.out" >> "output_test/test_md_"${file:29}".out"
    fi
    rm "output_test/"${i_test}"_test_md.out"
done

echo -e "\n\e[1mTest MD simple error file:\e[0m\n"

for file in example_file/error_file/md/simple/*
do
    test_name "${file:34}"
    ./mypandoc -i $file -f markdown -e markdown &> /dev/null
    test_return_84
done

exit $RETURN_VALUE
