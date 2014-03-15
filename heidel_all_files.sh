#/bin/bash


for file in work/heidel/*
do
	echo "remove $file"
	rm $file

done

echo "execute heidel"


for file in work/*
do

	if [ ! -d $file ]; then
		java -jar lib/de.unihd.dbs.heideltime.standalone.jar -t news $file > work/heidel/${file:5}
	fi

	#java -jar lib/de.unihd.dbs.heideltime.standalone.jar -t news $file > work/heidel/${file:5}
	
	#echo ${file:5}
	
	#export hIndex=strindex $file "/"

done
