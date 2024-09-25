# Loop for entire scene:

# Dimensions
ydim=20807
xdim=24713
step=1000

commands=()
for ymin in `seq -f "%04g" 0 $step $ydim`; do
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
    #echo Doing block: $xmin $ymin $xmax $ymax and outputting to: $output_name
    commands+=("python3 ../../hmm/hmm/hmm.py ../0_classified_images/MP_????.vrt -o $output_name --block $xmin $ymin $xstep $ystep")
  done
done

#echo "${commands[18]}"
#echo "${#commands[@]}"

N=20 # Number of parallel processes
number_of_commands=${#commands[@]}

for n in `seq 0 1 $number_of_commands`; do
  ((i=i%N)); ((i++==0)) && wait
  echo ${commands[n]}; eval "${commands[n]}" &
done


