tr '[:upper:]' '[:lower:]' < nat2018.csv > nat2018_low.csv
sed s/É/e/g nat2018_low.csv | sed s/Ï/i/g | sed s/Ü/u/g |sed s/Ö/o/g | sed s/Ë/e/g | sed s/È/e/g | sed s/ç/c/g | sed s/xxxx/XXXX/g > nat2018_anglo.csv

awk 'BEGIN{OFS=FS=";"}
     NR>1{$2 = tolower($2);
          split($2,arr," ");
          for(x in arr)
              sub(arr[x],toupper(substr(arr[x],1,1))substr(arr[x],2),$2)
         }1' nat2018_anglo.csv > nat2018_format.csv