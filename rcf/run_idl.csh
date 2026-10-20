#!/bin/csh

cd /homedir/ghong/sarb/CER7.2.1P2/rcf

# Compile the procedure file and execute the procedure
idl -e ".compile 'run_20130416_script' & run_20130416_script"
