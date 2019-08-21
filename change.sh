#!/bin/bash
echo Add /.. to all makefile in the line make.inc 
while read line; do
	echo $line'----------------------'
	for dir in `ls source/$line|grep -E '^v\d+'`;do
		sed -i -E 's/ ..\/..\/make\.inc/ ..\/..\/..\/make\.inc/g' source/$line/$dir/makefile
		rm source/$line/$dir/makefile-E
	done
done < components.dat
