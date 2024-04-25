#!/bin/bash

simple_convert()
{
    options=$(ls -1 example_file | awk '{print NR " " $0}')
    selected=$(whiptail --title "My Pandoc" --menu "Choose a file to convert" 20 78 10 $options 3>&1 1>&2 2>&3)
    filepath=$(echo "$options" | grep "^$selected " | awk '{print $2}')

    OPTION=$(whiptail --title "My Pandoc" --menu "Choose an option :" 15 50 4 \
        "1" "JSON" \
        "2" "XML" \
        "3" "MD" \
        3>&1 1>&2 2>&3)

        format=""

    case $OPTION in
        1) format="json";;
        2) format="xml";;
        3) format="markdown";;
    esac

    ./mypandoc -f $format -i example_file/$filepath > output.txt
    output_file=`cat output.txt`
    whiptail --msgbox --title "My Pandoc" "$output_file" 50 200
    rm output.txt
    interface_handler
}

convert_with_options()
{
    OPTION=$(whiptail --title "My Pandoc" --menu "Choose an input format :" 15 50 4 \
        "1" "JSON" \
        "2" "XML" \
        "3" "MD" \
        3>&1 1>&2 2>&3)

    inputFormat=""

    case $OPTION in
        1) inputFormat="json";;
        2) inputFormat="xml";;
        3) inputFormat="markdown";;
    esac

    ###

    options=$(ls -1 example_file | awk '{print NR " " $0}')
    selected=$(whiptail --title "My Pandoc" --menu "Choose a file to convert" 20 78 10 $options 3>&1 1>&2 2>&3)
    filepath=$(echo "$options" | grep "^$selected " | awk '{print $2}')

    ###

    OPTION=$(whiptail --title "My Pandoc" --menu "Choose an option :" 15 50 4 \
        "1" "JSON" \
        "2" "XML" \
        "3" "MD" \
        3>&1 1>&2 2>&3)

        format=""

    case $OPTION in
        1) format="json";;
        2) format="xml";;
        3) format="markdown";;
    esac

    ###

    outputFile=$(whiptail --inputbox "My Pandoc" 8 39 --title "Enter path of the output file" 3>&1 1>&2 2>&3)
    echo $outputFile
}

interface_handler()
{
    OPTION=$(whiptail --title "My Pandoc" --menu "Choose an option :" 15 50 4 \
        "1" "Simple Convert" \
        "2" "Convert With Options" \
        3>&1 1>&2 2>&3)

    case $OPTION in
        1) simple_convert;;
        2) convert_with_options;;
    esac
}

leave_process()
{
    exit
}

interface_handler
leave_process
