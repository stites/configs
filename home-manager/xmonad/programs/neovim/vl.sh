#!/bin/bash
# from https://github.com/auxiliary/vim-layout

#Show help and exit
if [ $# -eq 0 ] || [ $1 == "-h" ] || [ $1 == "--help" ];
then 
    echo "Usage: vl [OPTION] [FILE]..."
    echo "Open nvim for FILEs with automatic layouts."
    echo "OPTION:"
    echo -e "  -t, --tabbed  \t open 2 FILEs per tab, splitting each tab vertically"
    echo -e "  -H, --htabbed \t open 2 FILEs per tab, splitting each tab horizontally"
    echo -e "  -i, --install \t install this script"
    echo -e "  -h, --help    \t display this help text and exit"
    exit
fi

#Install and exit
if [ "$1" == "-i" ] || [ "$1" == "--install" ];
then
    install -m 755 $BASH_SOURCE /usr/local/bin/vl
    exit
fi

tabbed_mode=false
split_mode="vsplit"
vimc_split_mode="-O"

#If tabbed mode or the number of files is more than 5
if [ "$1" == "-t" ] || [ "$1" == "--tabbed" ] || [ $1 == "-H" ] || [ $1 == "--htabbed" ] || [ $# -ge 5 ];
then
    tabbed_mode=true
    if [ $1 == "-H" ] || [ $1 == "--htabbed" ];
    then
        split_mode="split"
        vimc_split_mode="-o"
        shift #remove the argument
    elif [ $1 == "-t" ] || [ $1 == "--tabbed" ];
    then
        shift #remove the argument
    fi
fi

if [ $tabbed_mode == true ];
then
    first2files=$1;
    shift
    first2files=$first2files" "$1;
    shift
    vimcommand="";
    first_iteration=1
    while [ $1 ]
    do
        if [ $first_iteration -eq 1 ];
        then
            vimcommand=$vimcommand":tabe "$1;
            first_iteration=0;
        else
            vimcommand=$vimcommand" | tabe "$1;
        fi
        shift
        if [ $1 ];
        then
            vimcommand=$vimcommand" | "$split_mode" "$1;
            shift
        fi
    done
    nvim $(echo $vimc_split_mode) -c "$vimcommand" $first2files
#Smart split
else
    if [ $# -eq 4 ];
    then
        nvim -O -c ":split $3 | wincmd l | split $4" $1 $2;
    elif [ $# -eq 3 ];  
    then
        nvim -O -c ":wincmd l | split $3" $1 $2; 
    elif [ $# -eq 2 ];
    then
        nvim -O $1 $2; 
    elif [ $# -eq 1 ];
    then
        nvim $1; 
    fi
fi

