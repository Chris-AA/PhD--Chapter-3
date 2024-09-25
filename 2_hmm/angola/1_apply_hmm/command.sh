#Simple:
#python3 ../../hmm/hmm/hmm.py ../0_classified_images/MP_200[012345678].vrt --block 2000 2000 6000 6000 -v

# Loop for entire scene:

# Dimensions are: 19446 x 16215
ydim=17534
xdim=25215
step=2000

for ymin in `seq -f  "%04g" 0 $step $ydim`; do
  ymax=$(($ymin + $step))
  if [ $ymax -gt $ydim ]
  then
    ymax=$ydim
  fi
  ystep=$(($ymax - $ymin))
  for xmin in `seq -f "%04g" 0 $step $xdim`; do
    xmax=$(($xmin + $step))
    if [ $xmax -gt $xdim ]
    then
      xmax=$xdim    
    fi
    xstep=$(($xmax - $xmin))
    output_name="MP_test_${xmin}_${ymin}_${xmax}_${ymax}"
    echo Doing block: $xmin $ymin $xmax $ymax and outputting to $output_name
    echo python3 ../../hmm/hmm/hmm.py ../0_classified_images/MP_????.vrt -o $output_name --block $xmin $ymin $xstep $ystep
  done
done

