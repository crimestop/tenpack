#!/bin/bash
echo Add /.. to all makefile in the line make.inc 
while read line; do
	echo $line'----------------------'
	sed -i -E 's/ ..\/..\/..\/make\.inc/ ..\/..\/make\.inc/g' source/$line/$dir/makefile
	rm source/$line/makefile-E
done < components.dat
