#!/bin/csh

module switch intel pgi


make -f Makefile

cp *.x exec/
