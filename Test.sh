#!/bin/bash

RETURN_VALUE=0
i_test=0

if [ $# -ne 1 ]; then
    echo "Usage: $0 [XML | JSON | MD | ALL]"
    exit 1
fi

make

test_name()
{
    ((i_test++))
    echo -e "Test[\e[95m$i_test\e[0m]: $1"
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

test_json()
{
    echo -e "\n\e[1mTest JSON -> JSON:\e[0m\n"

    for file in example_file/simple_correct_file/json/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f json -o "output_test/"${i_test}"_test.json"
        colordiff -u $file "output_test/"${i_test}"_test.json"
        test_return_0
    done

    echo -e "\n\e[1mTest JSON -> XML:\e[0m\n"

    for file in example_file/simple_correct_file/json/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f xml -o "output_test/"${i_test}"_test.xml"
        colordiff -u $(echo "$file" | sed 's/\/json\//\/xml\//' | sed 's/.json/.xml/') "output_test/"${i_test}"_test.xml"
        test_return_0
    done

    echo -e "\n\e[1mTest JSON -> MD:\e[0m\n"

    for file in example_file/simple_correct_file/json/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f markdown -o "output_test/"${i_test}"_test.md"
        colordiff -u $(echo "$file" | sed 's/\/json\//\/md\//' | sed 's/.json/.md/') "output_test/"${i_test}"_test.md"
        test_return_0
    done

    # echo -e "\n\e[1mTest JSON simple error file:\e[0m\n"

    # for file in example_file/error_file/json/simple/*
    # do
        # test_name "${file:36}"
        # ./mypandoc -i $file -f json -e json &> /dev/null
        # test_return_84
    # done
}

test_xml()
{
    echo -e "\n\e[1mTest XML -> XML:\e[0m\n"

    for file in example_file/simple_correct_file/xml/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f xml -o "output_test/"${i_test}"_test.xml"
        colordiff -u $file "output_test/"${i_test}"_test.xml"
        test_return_0
    done

    echo -e "\n\e[1mTest XML -> JSON:\e[0m\n"

    for file in example_file/simple_correct_file/xml/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f json -o "output_test/"${i_test}"_test.json"
        colordiff -u $(echo "$file" | sed 's/\/xml\//\/json\//' | sed 's/.xml/.json/') "output_test/"${i_test}"_test.json"
        test_return_0
    done

    echo -e "\n\e[1mTest XML -> MD:\e[0m\n"

    for file in example_file/simple_correct_file/xml/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f markdown -o "output_test/"${i_test}"_test.md"
        colordiff -u $(echo "$file" | sed 's/\/xml\//\/md\//' | sed 's/.xml/.md/') "output_test/"${i_test}"_test.md"
        test_return_0
    done

##    echo -e "\n\e[1mTest XML simple error file:\e[0m\n"
##
##    for file in example_file/error_file/xml/simple/*
##    do
##        test_name "${file:35}"
##        ./mypandoc -i $file -f xml -e xml &> /dev/null
##        test_return_84
##    done
}

test_md()
{
    echo -e "\n\e[1mTest MD -> MD:\e[0m\n"

    for file in example_file/simple_correct_file/md/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f markdown -o "output_test/"${i_test}"_test.md"
        colordiff -u $file "output_test/"${i_test}"_test.md"
        test_return_0
    done

    echo -e "\n\e[1mTest MD -> XML:\e[0m\n"

    for file in example_file/simple_correct_file/md/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f xml -o "output_test/"${i_test}"_test.xml"
        colordiff -u $(echo "$file" | sed 's/\/md\//\/xml\//' | sed 's/.md/.xml/') "output_test/"${i_test}"_test.xml"
        test_return_0
    done

    echo -e "\n\e[1mTest MD -> JSON:\e[0m\n"

    for file in example_file/simple_correct_file/md/*
    do
        test_name "File: $file"
        ./mypandoc -i $file -f json -o "output_test/"${i_test}"_test.json"
        colordiff -u $(echo "$file" | sed 's/\/md\//\/json\//' | sed 's/.md/.json/') "output_test/"${i_test}"_test.json"
        test_return_0
    done

    echo -e "\n\e[1mTest MD simple error file:\e[0m\n"

    for file in example_file/error_file/md/simple/*
    do
        test_name "${file:34}"
        ./mypandoc -i $file -f markdown -e markdown &> /dev/null
        test_return_84
    done
}


if [ "$1" = "JSON" ]; then
    test_json
elif [ "$1" = "XML" ]; then
    test_xml
elif [ "$1" = "MD" ]; then
    test_md
else
    test_json
    test_xml
    test_json
fi

exit $RETURN_VALUE
